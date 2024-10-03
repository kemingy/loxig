const std = @import("std");
const Scan = @import("scan.zig");

const Token = Scan.Token;
const Lexeme = Scan.Lexeme;

// This function doesn't simplify the code. Keep it here for reference.
pub fn define_ast(comptime attributes: anytype) type {
    var fields: [attributes.len]std.builtin.Type.StructField = undefined;
    for (attributes, 0..) |attribute, i| {
        fields[i] = .{
            .name = attribute[0],
            .type = attribute[1],
            .default_value = null,
            .is_comptime = false,
            .alignment = 0,
        };
    }

    return @Type(.{ .Struct = .{
        .layout = .auto,
        .fields = fields,
        .decls = &[_]std.builtin.Type.Declaration{},
        .is_tuple = false,
    } });
}

pub const Expression = union(enum) {
    binary: Binary,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,

    pub fn format(
        self: Expression,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            inline else => |case| try case.format(fmt, options, writer),
        }
    }
};

pub const Binary = struct {
    left: *const Expression,
    right: *const Expression,
    operator: Token,

    pub fn format(
        self: Binary,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({s} {} {})", .{ self.operator.literal, self.left, self.right });
    }
};

pub const Grouping = struct {
    expression: *const Expression,

    pub fn format(
        self: Grouping,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(group {})", .{self.expression});
    }
};

pub const Literal = struct {
    lexeme: Lexeme,

    pub fn init(lexeme: Lexeme) Literal {
        return Literal{ .lexeme = lexeme };
    }

    pub fn format(
        self: Literal,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        // just to make the codecrafter test happy
        // world prefer `try writer.print("{s}", .{self.lexeme});`
        switch (self.lexeme) {
            .string => |str| {
                try writer.print("{s}", .{str});
            },
            .number => |num| {
                if (try Scan.number_contain_dot(num)) {
                    try writer.print("{d}", .{num});
                } else {
                    try writer.print("{d}.0", .{num});
                }
            },
            .boolean => |b| {
                try writer.print("{s}", .{if (b) "true" else "false"});
            },
            .nullptr => {
                try writer.writeAll("nil");
            },
        }
    }
};

test "literal pretty print" {
    const literal = Literal.init(.{ .number = 12.0 });
    const buf = try std.fmt.allocPrint(std.testing.allocator, "{}", .{literal});
    defer std.testing.allocator.free(buf);
    try std.testing.expectEqualStrings("12.0", buf);
}

pub const Unary = struct {
    right: *const Expression,
    operator: Token,

    pub fn format(
        self: Unary,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({s} {})", .{ self.operator.literal, self.right });
    }
};

test "pretty print" {
    const expr = Expression{ .binary = Binary{
        .left = &Expression{
            .unary = Unary{
                .operator = Scan.Token{
                    .token_type = Scan.TokenType.MINUS,
                    .literal = "-",
                    .lexeme = null,
                    .line = 1,
                },
                .right = &Expression{
                    .literal = Literal{
                        .lexeme = .{ .number = 123.0 },
                    },
                },
            },
        },
        .right = &Expression{
            .grouping = Grouping{
                .expression = &Expression{
                    .literal = Literal{
                        .lexeme = .{ .number = 45.67 },
                    },
                },
            },
        },
        .operator = Scan.Token{
            .token_type = Scan.TokenType.STAR,
            .literal = "*",
            .lexeme = null,
            .line = 1,
        },
    } };

    const buf = try std.fmt.allocPrint(std.testing.allocator, "{}", .{expr});
    defer std.testing.allocator.free(buf);
    try std.testing.expectEqualStrings("(* (- 123.0) (group 45.67))", buf);
}
