const std = @import("std");
const Scan = @import("scan.zig");

const Token = Scan.Token;

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
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("({s} {} {})", .{ self.operator.literal, self.left, self.right });
    }
};

pub const Grouping = struct {
    expression: *const Expression,

    pub fn format(
        self: Grouping,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("(group {})", .{self.expression});
    }
};

pub const Literal = struct {
    value: []const u8,
    token_type: Scan.TokenType,

    pub fn init(value: []const u8, token_type: Scan.TokenType) Literal {
        return Literal{ .value = value, .token_type = token_type };
    }

    pub fn format(
        self: Literal,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.token_type == Scan.TokenType.NUMBER and std.mem.count(u8, self.value, ".") == 0) {
            try writer.print("{s}.0", .{self.value});
            return;
        }
        try writer.print("{s}", .{self.value});
    }
};

test "literal pretty print" {
    const literal = Literal.init("12", Scan.TokenType.NUMBER);
    const buf = try std.fmt.allocPrint(std.testing.allocator, "{}", .{literal});
    defer std.testing.allocator.free(buf);
    try std.testing.expectEqualStrings("12.0", buf);
}

pub const Unary = struct {
    right: *const Expression,
    operator: Token,

    pub fn format(
        self: Unary,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
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
                        .value = "123",
                        .token_type = Scan.TokenType.NUMBER,
                    },
                },
            },
        },
        .right = &Expression{
            .grouping = Grouping{
                .expression = &Expression{
                    .literal = Literal{
                        .value = "45.67",
                        .token_type = Scan.TokenType.NUMBER,
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
