const std = @import("std");
const Scan = @import("scan.zig");
const Expr = @import("expression.zig");
const Report = @import("report.zig");

const Token = Scan.Token;
const TokenType = Scan.TokenType;

pub const ParseError = error{
    UnexpectedToken,
    IndexOutOfBounds,
    OutOfMemory,
};

const EOF = Token{
    .token_type = TokenType.EOF,
    .literal = "EOF",
    .lexeme = null,
    .line = 0,
};

const Parser = struct {
    tokens: []const Token,
    current: u32 = 0,
    allocator: std.heap.ArenaAllocator,
    exprs: std.ArrayList(Expr.Expression) = undefined,

    pub fn init(tokens: []const Token) Parser {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const allocator = arena.allocator();
        return Parser{
            .tokens = tokens,
            .allocator = arena,
            .exprs = std.ArrayList(Expr.Expression).init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.allocator.deinit();
    }

    fn check_type(self: *Parser, token_type: TokenType) bool {
        if (self.is_eof()) {
            return false;
        }
        return self.peek().token_type == token_type;
    }

    fn peek(self: *Parser) Token {
        if (self.current == self.tokens.len) {
            // sentinel value
            return EOF;
        }
        return self.tokens[self.current];
    }

    fn is_eof(self: *Parser) bool {
        return self.peek().token_type == TokenType.EOF;
    }

    fn previous(self: *Parser) ParseError!Token {
        if (self.current == 0) {
            return error.IndexOutOfBounds;
        }
        return self.tokens[self.current - 1];
    }

    fn advance(self: *Parser) !void {
        if (!self.is_eof()) {
            self.current += 1;
        }
    }

    fn match(self: *Parser, types: []const TokenType) ParseError!bool {
        if (self.is_eof()) return false;
        for (types) |token_type| {
            if (self.check_type(token_type)) {
                try self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) ParseError!void {
        if (self.check_type(token_type)) {
            try self.advance();
            return;
        }
        try Report.err(
            "[line {}] Error: {s}\n",
            .{ self.peek().line, message },
        );
        return error.UnexpectedToken;
    }

    fn get_last_expr_pointer(self: *Parser) ParseError!*const Expr.Expression {
        if (self.exprs.items.len == 0) {
            return error.IndexOutOfBounds;
        }
        return &self.exprs.items[self.exprs.items.len - 1];
    }

    fn expression(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.comparison();
        while (try self.match(&[_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL })) {
            const operator = try self.previous();
            const right = try self.comparison();
            try self.exprs.append(Expr.Expression{ .binary = Expr.Binary{
                .left = expr,
                .right = right,
                .operator = operator,
            } });
            expr = try self.get_last_expr_pointer();
        }
        return expr;
    }

    fn comparison(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.term();
        while (try self.match(&[_]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL })) {
            const operator = try self.previous();
            const right = try self.term();
            try self.exprs.append(Expr.Expression{ .binary = Expr.Binary{
                .left = expr,
                .right = right,
                .operator = operator,
            } });
            expr = try self.get_last_expr_pointer();
        }
        return expr;
    }

    fn term(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.factor();
        while (try self.match(&[_]TokenType{ TokenType.MINUS, TokenType.PLUS })) {
            const operator = try self.previous();
            const right = try self.factor();
            try self.exprs.append(Expr.Expression{ .binary = Expr.Binary{
                .left = expr,
                .right = right,
                .operator = operator,
            } });
            expr = try self.get_last_expr_pointer();
        }
        return expr;
    }

    fn factor(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.unary();
        while (try self.match(&[_]TokenType{ TokenType.SLASH, TokenType.STAR })) {
            const operator = try self.previous();
            const right = try self.unary();
            try self.exprs.append(Expr.Expression{ .binary = Expr.Binary{
                .left = expr,
                .right = right,
                .operator = operator,
            } });
            expr = try self.get_last_expr_pointer();
        }
        return expr;
    }

    fn unary(self: *Parser) ParseError!*const Expr.Expression {
        if (try self.match(&[_]TokenType{ TokenType.BANG, TokenType.MINUS })) {
            const operator = try self.previous();
            const right = try self.unary();
            try self.exprs.append(Expr.Expression{
                .unary = .{
                    .operator = operator,
                    .right = right,
                },
            });
            return try self.get_last_expr_pointer();
        }
        return try self.primary();
    }

    fn primary(self: *Parser) ParseError!*const Expr.Expression {
        if (try self.match(&[_]TokenType{TokenType.FALSE})) {
            try self.exprs.append(Expr.Expression{ .literal = Expr.Literal.init(Scan.Lexeme{ .boolean = false }) });
            return try self.get_last_expr_pointer();
        }
        if (try self.match(&[_]TokenType{TokenType.TRUE})) {
            try self.exprs.append(Expr.Expression{
                .literal = Expr.Literal.init(Scan.Lexeme{ .boolean = true }),
            });
            return try self.get_last_expr_pointer();
        }
        if (try self.match(&[_]TokenType{TokenType.NIL})) {
            try self.exprs.append(Expr.Expression{
                .literal = Expr.Literal.init(Scan.Lexeme.build_null()),
            });
            return try self.get_last_expr_pointer();
        }

        if (try self.match(&[_]TokenType{ TokenType.NUMBER, TokenType.STRING })) {
            const token = try self.previous();
            if (token.lexeme) |lexeme| {
                try self.exprs.append(Expr.Expression{ .literal = Expr.Literal.init(lexeme) });
                return try self.get_last_expr_pointer();
            } else {
                try Report.err("[line {}] Error: unexpected token: {s}", .{ token.line, token.literal });
                return error.UnexpectedToken;
            }
        }

        if (try self.match(&[_]TokenType{TokenType.LEFT_PAREN})) {
            const expr = try self.expression();
            try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
            try self.exprs.append(Expr.Expression{ .grouping = Expr.Grouping{ .expression = expr } });
            return try self.get_last_expr_pointer();
        }

        const token = self.peek();
        try Report.err("[line {}] Error: unexpected token {s}", .{ token.line, token.literal });
        return error.UnexpectedToken;
    }

    pub fn parse(self: *Parser) ParseError!*const Expr.Expression {
        return try self.expression();
    }
};

test "parse expression" {
    const tokens = [_]Token{
        try Token.init(TokenType.NUMBER, .{ .number = 1.0 }, "1.0", 1),
        try Token.init(TokenType.MINUS, null, "-", 1),
        try Token.init(TokenType.NUMBER, .{ .number = 2.0 }, "2.0", 1),
        try Token.init(TokenType.STAR, null, "*", 1),
        try Token.init(TokenType.NUMBER, .{ .number = 3.0 }, "4.0", 1),
    };
    var parser = Parser.init(&tokens);
    defer parser.deinit();
    const expr = try parser.parse();

    const buf = try std.fmt.allocPrint(std.testing.allocator, "{}", .{expr});
    defer std.testing.allocator.free(buf);
    try std.testing.expectEqualStrings("(- 1.0 (* 2.0 3.0))", buf);
}

pub fn parse(content: []const u8) !void {
    var scanner = Scan.Scanner.init(content);
    var tokens = try scanner.scan(std.heap.page_allocator);
    defer tokens.deinit();

    var parser = Parser.init(try tokens.toOwnedSlice());
    defer parser.deinit();
    try Report.print("{}\n", .{try parser.parse()});
}
