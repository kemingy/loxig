const std = @import("std");
const Report = @import("report.zig");

const TokenType = enum {
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
    ADD,
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
};

const Token = struct {
    token_type: TokenType,
    lexeme: ?[]const u8,
    literal: []const u8,
    line: u32,

    pub fn init(token_type: TokenType, lexeme: ?[]const u8, literal: []const u8, line: u32) Token {
        return Token{
            .token_type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn to_string(self: Token, allocator: std.mem.Allocator) ![]u8 {
        const literal = if (self.token_type == TokenType.STRING) try std.fmt.allocPrint(allocator, "\"{s}\"", .{self.literal}) else self.literal;
        const lexeme = self.lexeme orelse "null";
        return std.fmt.allocPrint(allocator, "{s} {s} {s}", .{
            @tagName(self.token_type),
            literal,
            lexeme,
        });
    }
};

const Scanner = struct {
    has_error: bool = false,
    current: u32 = 0,
    start: u32 = 0,
    line: u32 = 1,
    content: []const u8,

    fn init(content: []const u8) Scanner {
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

    fn is_digit(char: u8) bool {
        return char >= '0' and char <= '9';
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
        try tokens.append(Token.init(TokenType.STRING, self.content[self.start + 1 .. self.current - 1], self.content[self.start + 1 .. self.current - 1], self.line));
    }

    fn has_error(self: *Scanner) bool {
        return self.has_error;
    }

    fn add_token(self: *Scanner, tokens: *std.ArrayList(Token), token_type: TokenType) !void {
        try tokens.append(Token.init(token_type, null, self.content[self.start..self.current], self.line));
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
                    self.number(tokens);
                }
                try Report.err("[line {d}] Error: Unexpected character: {c}\n", .{
                    self.line,
                    char,
                });
                self.has_error = true;
            },
        }
    }

    pub fn scan(self: *Scanner, allocator: std.mem.Allocator) !std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(allocator);

        while (!self.is_eof()) {
            self.start = self.current;
            try self.scan_token(&tokens);
        }
        try tokens.append(Token.init(TokenType.EOF, null, "", self.line));
        return tokens;
    }
};

pub fn scan(content: []const u8) !void {
    var scanner = Scanner.init(content);
    const tokens = try scanner.scan(std.heap.page_allocator);
    defer tokens.deinit();

    for (tokens.items) |token| {
        const token_str = try token.to_string(std.heap.page_allocator);
        defer std.heap.page_allocator.free(token_str);
        try Report.print("{s}\n", .{token_str});
    }

    if (scanner.has_error) {
        std.process.exit(65);
    }
}
