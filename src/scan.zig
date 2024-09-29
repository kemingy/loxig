const std = @import("std");

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
        return std.fmt.allocPrint(allocator,
            "{s} {s} null", .{
                @tagName(self.token_type),
                self.literal,
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

    fn add_token(self: *Scanner, tokens: *std.ArrayList(Token), token_type: TokenType) !void {
        try tokens.append(Token.init(token_type, null, self.content[self.start..self.current], self.line));
    }

    fn scan_token(self: *Scanner, tokens: *std.ArrayList(Token)) !void {
        switch (self.advance()) {
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
            else => {
                std.debug.print("Unexpected character at line: {}\n", .{self.line});
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
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}\n", .{token_str});
    }
}
