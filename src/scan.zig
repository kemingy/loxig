const std = @import("std");
const Report = @import("report.zig");

pub const TokenType = enum {
    EOF,

    // single-character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // one or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // literals
    IDENTIFIER,
    STRING,
    NUMBER,

    // keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    pub fn format(
        self: TokenType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        return writer.print("{s}", .{@tagName(self)});
    }
};

const reserved_keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", TokenType.AND },
    .{ "class", TokenType.CLASS },
    .{ "else", TokenType.ELSE },
    .{ "false", TokenType.FALSE },
    .{ "fun", TokenType.FUN },
    .{ "for", TokenType.FOR },
    .{ "if", TokenType.IF },
    .{ "nil", TokenType.NIL },
    .{ "or", TokenType.OR },
    .{ "print", TokenType.PRINT },
    .{ "return", TokenType.RETURN },
    .{ "super", TokenType.SUPER },
    .{ "this", TokenType.THIS },
    .{ "true", TokenType.TRUE },
    .{ "var", TokenType.VAR },
    .{ "while", TokenType.WHILE },
});

pub fn number_contain_dot(number: f64) !bool {
    var buf: [64]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try std.fmt.format(stream.writer(), "{d}", .{number});
    return std.mem.count(u8, stream.getWritten(), ".") != 0;
}

pub const Lexeme = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nullptr: void,

    pub fn build_null() Lexeme {
        return Lexeme{ .nullptr = {} };
    }

    pub fn build_string(string: []const u8) !Lexeme {
        if (string.len > 64) {
            return error.LengthExceeded;
        }
        return Lexeme{ .string = string };
    }

    pub fn build_number(string: []const u8) !Lexeme {
        const number: f64 = try std.fmt.parseFloat(f64, string);
        return Lexeme{ .number = number };
    }

    pub fn build_boolean(string: []const u8) !Lexeme {
        if (std.mem.eql(u8, string, "true")) {
            return Lexeme{ .boolean = true };
        } else if (std.mem.eql(u8, string, "false")) {
            return Lexeme{ .boolean = false };
        }
        return error.InvalidBoolean;
    }

    pub fn format(
        self: Lexeme,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .string => {
                try writer.print("{s}", .{self.string});
            },
            .number => |num| {
                if (try number_contain_dot(num)) {
                    try writer.print("{d}", .{num});
                } else {
                    try writer.print("{d}.0", .{num});
                }
            },
            // just to make the codecrafter test happy
            else => try writer.writeAll("null"),
        }
    }
};

test "lexeme to string" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    var lexeme = Lexeme{ .string = "hello" };
    try buf.writer().print("{}", .{lexeme});
    try std.testing.expectEqualStrings("hello", buf.items);
    buf.clearRetainingCapacity();

    lexeme = Lexeme{ .number = 123.45 };
    try buf.writer().print("{}", .{lexeme});
    try std.testing.expectEqualStrings("123.45", buf.items);
    buf.clearRetainingCapacity();

    lexeme = try Lexeme.build_number("42");
    try buf.writer().print("{}", .{lexeme});
    try std.testing.expectEqualStrings("42.0", buf.items);
}

pub const Token = struct {
    token_type: TokenType,
    lexeme: ?Lexeme,
    literal: []const u8,
    line: u32,

    pub fn init(token_type: TokenType, lexeme: ?Lexeme, literal: []const u8, line: u32) !Token {
        if (literal.len > 64) {
            return error.LengthExceeded;
        }
        return Token{
            .token_type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn format(
        self: Token,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{} ", .{self.token_type});
        if (self.token_type == TokenType.STRING) {
            try writer.print("\"{s}\" ", .{self.literal});
        } else {
            try writer.print("{s} ", .{self.literal});
        }
        if (self.lexeme) |lexeme| {
            try writer.print("{}", .{lexeme});
        } else {
            try writer.print("null", .{});
        }
    }
};

test "token to string" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    var token = try Token.init(TokenType.SEMICOLON, null, ";", 1);
    try buf.writer().print("{}", .{token});
    try std.testing.expectEqualStrings("SEMICOLON ; null", buf.items);
    buf.clearRetainingCapacity();

    token = try Token.init(TokenType.STRING, try Lexeme.build_string("hello"), "hello", 1);
    try buf.writer().print("{}", .{token});
    try std.testing.expectEqualStrings("STRING \"hello\" hello", buf.items);
}

pub const Scanner = struct {
    has_error: bool = false,
    current: u32 = 0,
    start: u32 = 0,
    line: u32 = 1,
    content: []const u8,

    pub fn init(content: []const u8) Scanner {
        return Scanner{
            .content = content,
        };
    }

    fn is_eof(self: *Scanner) bool {
        return self.current >= self.content.len;
    }

    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.content[self.current - 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.is_eof()) return false;
        if (self.content[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: *Scanner) u8 {
        if (self.is_eof()) return @as(u8, 0);
        return self.content[self.current];
    }

    fn peek_next(self: *Scanner) u8 {
        if (self.current + 1 >= self.content.len) return @as(u8, 0);
        return self.content[self.current + 1];
    }

    fn is_digit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn is_alpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or char == '_';
    }

    fn is_alpha_numeric(char: u8) bool {
        return is_alpha(char) or is_digit(char);
    }

    fn string(self: *Scanner, tokens: *std.ArrayList(Token)) !void {
        while (self.peek() != '"' and !self.is_eof()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.is_eof()) {
            try Report.err("[line {}] Error: Unterminated string.\n", .{self.line});
            self.has_error = true;
            return;
        }
        _ = self.advance();
        try tokens.append(try Token.init(
            TokenType.STRING,
            try Lexeme.build_string(self.content[self.start + 1 .. self.current - 1]),
            self.content[self.start + 1 .. self.current - 1],
            self.line,
        ));
    }

    fn number(self: *Scanner, tokens: *std.ArrayList(Token)) !void {
        while (is_digit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and is_digit(self.peek_next())) {
            // consume "."
            _ = self.advance();
            while (is_digit(self.peek())) {
                _ = self.advance();
            }
        }
        try tokens.append(try Token.init(
            TokenType.NUMBER,
            try Lexeme.build_number(self.content[self.start..self.current]),
            self.content[self.start..self.current],
            self.line,
        ));
    }

    fn identifier(self: *Scanner, tokens: *std.ArrayList(Token)) !void {
        while (is_alpha_numeric(self.peek())) {
            _ = self.advance();
        }
        const kw = reserved_keywords.get(self.content[self.start..self.current]);
        const token_type = if (kw) |t| t else TokenType.IDENTIFIER;
        try tokens.append(try Token.init(
            token_type,
            switch (token_type) {
                .TRUE, .FALSE => try Lexeme.build_boolean(self.content[self.start..self.current]),
                .NIL => Lexeme.build_null(),
                else => null,
            },
            self.content[self.start..self.current],
            self.line,
        ));
    }

    fn has_error(self: *Scanner) bool {
        return self.has_error;
    }

    fn add_token(self: *Scanner, tokens: *std.ArrayList(Token), token_type: TokenType) !void {
        try tokens.append(try Token.init(
            token_type,
            null,
            self.content[self.start..self.current],
            self.line,
        ));
    }

    fn scan_token(self: *Scanner, tokens: *std.ArrayList(Token)) !void {
        switch (self.advance()) {
            '\n' => self.line += 1,
            ' ', '\r', '\t' => {},
            '(' => try self.add_token(tokens, TokenType.LEFT_PAREN),
            ')' => try self.add_token(tokens, TokenType.RIGHT_PAREN),
            '{' => try self.add_token(tokens, TokenType.LEFT_BRACE),
            '}' => try self.add_token(tokens, TokenType.RIGHT_BRACE),
            ',' => try self.add_token(tokens, TokenType.COMMA),
            '.' => try self.add_token(tokens, TokenType.DOT),
            '-' => try self.add_token(tokens, TokenType.MINUS),
            '+' => try self.add_token(tokens, TokenType.PLUS),
            ';' => try self.add_token(tokens, TokenType.SEMICOLON),
            '*' => try self.add_token(tokens, TokenType.STAR),
            '!' => {
                const token_type = if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
                try self.add_token(tokens, token_type);
            },
            '=' => {
                const token_type = if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL;
                try self.add_token(tokens, token_type);
            },
            '<' => {
                const token_type = if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS;
                try self.add_token(tokens, token_type);
            },
            '>' => {
                const token_type = if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER;
                try self.add_token(tokens, token_type);
            },
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.is_eof()) {
                        _ = self.advance();
                    }
                } else {
                    try self.add_token(tokens, TokenType.SLASH);
                }
            },
            '"' => try self.string(tokens),
            else => |char| {
                if (is_digit(char)) {
                    try self.number(tokens);
                } else if (is_alpha(char)) {
                    try self.identifier(tokens);
                } else {
                    try Report.err("[line {d}] Error: Unexpected character: {c}\n", .{
                        self.line,
                        char,
                    });
                    self.has_error = true;
                }
            },
        }
    }

    pub fn scan(self: *Scanner, allocator: std.mem.Allocator) !std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(allocator);

        while (!self.is_eof()) {
            self.start = self.current;
            try self.scan_token(&tokens);
        }
        try tokens.append(try Token.init(TokenType.EOF, null, "", self.line));
        return tokens;
    }
};

pub fn scan(content: []const u8) !void {
    var scanner = Scanner.init(content);
    const tokens = try scanner.scan(std.heap.page_allocator);
    defer tokens.deinit();

    for (tokens.items) |token| {
        try Report.print("{}\n", .{token});
    }

    if (scanner.has_error) {
        std.process.exit(65);
    }
}
