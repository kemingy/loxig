const std = @import("std");
const Scan = @import("scan.zig");
const Expr = @import("expression.zig");
const Report = @import("report.zig");

const Token = Scan.Token;
const TokenType = Scan.TokenType;
const Statement = Expr.Statement;

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

pub const Parser = struct {
    tokens: []const Token,
    current: u32 = 0,
    arena: std.heap.ArenaAllocator,

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        const arena = std.heap.ArenaAllocator.init(allocator);
        return Parser{
            .tokens = tokens,
            .arena = arena,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    fn create_expr(self: *Parser, args: anytype) ParseError!*const Expr.Expression {
        const expr = try self.arena.allocator().create(Expr.Expression);
        expr.* = args;
        return expr;
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

    fn match_one(self: *Parser, token_type: TokenType) ParseError!bool {
        if (self.is_eof()) return false;
        if (self.check_type(token_type)) {
            try self.advance();
            return true;
        }
        return false;
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

    fn assignment(self: *Parser) ParseError!*const Expr.Expression {
        const expr = try self.equality();
        if (try self.match_one(TokenType.EQUAL)) {
            const operator = try self.previous();
            const right = try self.assignment();
            switch (expr.*) {
                .variable => return try self.create_expr(.{
                    .assign = Expr.Assign{
                        .name = expr.variable.name,
                        .value = right,
                    },
                }),
                else => {
                    try Report.err("[line {d}] Error: Invalid assignment target.\n", .{operator.line});
                    return error.UnexpectedToken;
                },
            }
        }
        return expr;
    }

    fn expression(self: *Parser) ParseError!*const Expr.Expression {
        return try self.assignment();
    }

    fn equality(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.comparison();
        while (try self.match(&[_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL })) {
            const operator = try self.previous();
            const right = try self.comparison();
            expr = try self.create_expr(.{ .binary = Expr.Binary{
                .left = expr,
                .right = right,
                .operator = operator,
            } });
        }
        return expr;
    }

    fn comparison(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.term();
        while (try self.match(&[_]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL })) {
            const operator = try self.previous();
            const right = try self.term();
            expr = try self.create_expr(.{
                .binary = Expr.Binary{
                    .left = expr,
                    .right = right,
                    .operator = operator,
                },
            });
        }
        return expr;
    }

    fn term(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.factor();
        while (try self.match(&[_]TokenType{ TokenType.MINUS, TokenType.PLUS })) {
            const operator = try self.previous();
            const right = try self.factor();
            expr = try self.create_expr(.{
                .binary = Expr.Binary{
                    .left = expr,
                    .right = right,
                    .operator = operator,
                },
            });
        }
        return expr;
    }

    fn factor(self: *Parser) ParseError!*const Expr.Expression {
        var expr = try self.unary();
        while (try self.match(&[_]TokenType{ TokenType.SLASH, TokenType.STAR })) {
            const operator = try self.previous();
            const right = try self.unary();
            expr = try self.create_expr(.{
                .binary = Expr.Binary{
                    .left = expr,
                    .right = right,
                    .operator = operator,
                },
            });
        }
        return expr;
    }

    fn unary(self: *Parser) ParseError!*const Expr.Expression {
        if (try self.match(&[_]TokenType{ TokenType.BANG, TokenType.MINUS })) {
            const operator = try self.previous();
            const right = try self.unary();
            return try self.create_expr(.{
                .unary = Expr.Unary{
                    .operator = operator,
                    .right = right,
                },
            });
        }
        return try self.primary();
    }

    fn primary(self: *Parser) ParseError!*const Expr.Expression {
        if (try self.match(&[_]TokenType{
            TokenType.FALSE,
            TokenType.TRUE,
            TokenType.NIL,
            TokenType.STRING,
            TokenType.NUMBER,
        })) {
            const token = try self.previous();
            return try self.create_expr(.{
                .literal = Expr.Literal{ .lexeme = token.lexeme.? },
            });
        }

        if (try self.match_one(TokenType.IDENTIFIER)) {
            return try self.create_expr(.{
                .variable = Expr.Variable{ .name = try self.previous() },
            });
        }

        if (try self.match_one(TokenType.LEFT_PAREN)) {
            const expr = try self.expression();
            try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
            return try self.create_expr(.{
                .grouping = Expr.Grouping{ .expression = expr },
            });
        }

        const token = self.peek();
        try Report.err("[line {d}] Error at '{s}': Expect expression\n", .{ token.line, token.literal });
        return error.UnexpectedToken;
    }

    fn synchronize(self: *Parser) ParseError!void {
        try self.advance();
        while (!self.is_eof()) {
            const prev = try self.previous();
            if (prev.token_type == TokenType.SEMICOLON) {
                return;
            }

            const current = self.peek();
            switch (current.token_type) {
                TokenType.CLASS, TokenType.FUN, TokenType.VAR, TokenType.FOR, TokenType.IF, TokenType.WHILE, TokenType.PRINT, TokenType.RETURN => return,
                else => {},
            }
            try self.advance();
        }
    }

    fn print_statement(self: *Parser) ParseError!Statement {
        const expr = try self.expression();
        try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Statement{ .print = expr };
    }

    fn expression_statement(self: *Parser) ParseError!Statement {
        const expr = try self.expression();
        try self.consume(TokenType.SEMICOLON, "Expect ';' after expression.");
        return Statement{ .expression = expr };
    }

    fn build_block(self: *Parser) ParseError!std.ArrayList(Statement) {
        var statements = std.ArrayList(Statement).init(self.arena.allocator());
        while (!self.is_eof() and !self.check_type(TokenType.RIGHT_BRACE)) {
            try statements.append(try self.declaration());
        }
        try self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    fn block_statement(self: *Parser) ParseError!Statement {
        return Statement{ .block = try self.build_block() };
    }

    fn statement(self: *Parser) ParseError!Statement {
        if (try self.match_one(TokenType.PRINT)) {
            return try self.print_statement();
        }
        if (try self.match_one(TokenType.LEFT_BRACE)) {
            return try self.block_statement();
        }
        return try self.expression_statement();
    }

    fn var_declaration(self: *Parser) ParseError!Statement {
        if (try self.match_one(TokenType.IDENTIFIER)) {
            const name = try self.previous();
            var initializer: ?*const Expr.Expression = null;
            if (try self.match(&[_]TokenType{TokenType.EQUAL})) {
                initializer = try self.expression();
            }
            try self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
            return Statement{ .varlox = .{ .name = name, .initializer = initializer } };
        }
        return error.UnexpectedToken;
    }

    fn declaration(self: *Parser) ParseError!Statement {
        if (try self.match_one(TokenType.VAR)) {
            return self.var_declaration() catch {
                try self.synchronize();
                return try self.statement();
            };
        }
        return try self.statement();
    }

    pub fn parse_expr(self: *Parser) ParseError!*const Expr.Expression {
        return try self.expression();
    }

    pub fn parse(self: *Parser) ParseError!std.ArrayList(Statement) {
        var statements = std.ArrayList(Statement).init(self.arena.allocator());
        while (!self.is_eof()) {
            try statements.append(try self.declaration());
        }
        return statements;
    }
};

test "parse expression" {
    const tokens = [_]Token{
        try Token.init(TokenType.LEFT_PAREN, null, "(", 1),
        try Token.init(TokenType.NUMBER, .{ .number = 1.0 }, "1.0", 1),
        try Token.init(TokenType.MINUS, null, "-", 1),
        try Token.init(TokenType.NUMBER, .{ .number = 2.0 }, "2.0", 1),
        try Token.init(TokenType.RIGHT_PAREN, null, ")", 1),
        try Token.init(TokenType.STAR, null, "*", 1),
        try Token.init(TokenType.MINUS, null, "-", 1),
        try Token.init(TokenType.NUMBER, .{ .number = 3.0 }, "3.0", 1),
        try Token.init(TokenType.SEMICOLON, null, ";", 1),
    };
    var parser = Parser.init(&tokens, std.testing.allocator);
    defer parser.deinit();
    const statements = try parser.parse();

    try std.testing.expect(statements.items.len == 1);
    const buf = try std.fmt.allocPrint(std.testing.allocator, "{}", .{statements.getLast()});
    defer std.testing.allocator.free(buf);
    try std.testing.expectEqualStrings("(* (group (- 1.0 2.0)) (- 3.0))", buf);
}

pub fn parse(content: []const u8) !void {
    var scanner = Scan.Scanner.init(content);
    var tokens = try scanner.scan(std.heap.page_allocator);
    defer tokens.deinit();

    var parser = Parser.init(try tokens.toOwnedSlice(), std.heap.page_allocator);
    defer parser.deinit();
    const expr = parser.parse_expr() catch {
        std.process.exit(65);
    };
    try Report.print("{}\n", .{expr});
}
