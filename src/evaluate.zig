const std = @import("std");
const Expr = @import("expression.zig");
const Scan = @import("scan.zig");
const Report = @import("report.zig");
const Parse = @import("parser.zig");

const Expression = Expr.Expression;
const Token = Scan.Token;
const Statement = Expr.Statement;

const Object = union(enum) {
    nil: void,
    boolean: bool,
    number: f64,
    string: []const u8,

    pub fn is_truth(self: Object) bool {
        switch (self) {
            .nil => return false,
            .boolean => return self.boolean,
            else => return true,
        }
    }

    pub fn try_as_number(self: Object, expr: *const Expression) !f64 {
        switch (self) {
            .number => return self.number,
            else => {
                try Report.err("Operand must be a numbers.\n[line {}]\n", .{expr.get_line()});
                return error.UnsupportedOperator;
            },
        }
    }

    pub fn format(
        self: Object,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .nil => try writer.writeAll("nil"),
            .boolean => try writer.print(
                "{s}",
                .{if (self.boolean) "true" else "false"},
            ),
            .number => try writer.print("{d}", .{self.number}),
            .string => try writer.print("{s}", .{self.string}),
        }
    }
};

const Environment = struct {
    table: std.StringHashMap(Object),

    pub fn init(allocator: std.mem.Allocator) Environment {
        const table = std.StringHashMap(Object).init(allocator);
        return Environment{ .table = table };
    }

    pub fn deinit(self: *Environment) void {
        self.table.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Object) !void {
        try self.table.put(name, value);
    }

    pub fn assign(self: *Environment, name: []const u8, value: Object) !void {
        if (self.table.contains(name)) {
            try self.table.put(name, value);
            return;
        }
        return error.UndefinedVariable;
    }

    pub fn get(self: *Environment, name: []const u8) !Object {
        if (self.table.contains(name)) {
            return self.table.get(name).?;
        }
        return error.UndefinedVariable;
    }
};

const EvalError = error{
    OutOfMemory,
    UndefinedVariable,
    UnsupportedOperator,
};

const Evaluator = struct {
    arena: std.heap.ArenaAllocator,
    env: Environment,

    pub fn init(allocator: std.mem.Allocator) Evaluator {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const env = Environment.init(arena.allocator());
        return Evaluator{ .arena = arena, .env = env };
    }

    pub fn deinit(self: *Evaluator) void {
        self.env.deinit();
        self.arena.deinit();
    }

    fn concat_string(self: *Evaluator, left: []const u8, right: []const u8) EvalError![]const u8 {
        return try std.mem.concat(
            self.arena.allocator(),
            u8,
            &[_][]const u8{ left, right },
        );
    }

    fn plus(self: *Evaluator, left: *const Object, right: *const Object) EvalError!Object {
        switch (left.*) {
            .number => |x| switch (right.*) {
                .number => |y| return .{ .number = x + y },
                else => return error.UnsupportedOperator,
            },
            .string => |x| switch (right.*) {
                .string => |y| return .{ .string = try self.concat_string(x, y) },
                else => return error.UnsupportedOperator,
            },
            else => return error.UnsupportedOperator,
        }
    }

    fn is_equal(_: *Evaluator, left: *const Object, right: *const Object) bool {
        return switch (left.*) {
            .nil => switch (right.*) {
                .nil => true,
                else => false,
            },
            .boolean => |x| switch (right.*) {
                .boolean => |y| x == y,
                else => false,
            },
            .number => |x| switch (right.*) {
                .number => |y| x == y,
                else => false,
            },
            .string => |x| switch (right.*) {
                .string => |y| std.mem.eql(u8, x, y),
                else => false,
            },
        };
    }

    pub fn interpret(self: *Evaluator, statements: std.ArrayList(Statement)) EvalError!void {
        for (statements.items) |statement| {
            switch (statement) {
                .expression => |expr| _ = try self.evaluate(expr),
                .print => |expr| {
                    const obj = try self.evaluate(expr);
                    try Report.print("{}\n", .{obj});
                },
                .varlox => |varlox| {
                    const obj = if (varlox.initializer) |initial| try self.evaluate(initial) else Object{ .nil = {} };
                    try self.env.define(varlox.name.literal, obj);
                },
            }
        }
    }

    fn evaluate(self: *Evaluator, expr: *const Expression) EvalError!Object {
        switch (expr.*) {
            .binary => return try self.eval_binary(expr),
            .grouping => return try self.eval_grouping(expr),
            .literal => return try self.eval_literal(expr),
            .unary => return try self.eval_unary(expr),
            .variable => return try self.eval_variable(expr),
            .assign => return try self.eval_assign(expr),
        }
    }

    fn eval_assign(self: *Evaluator, expr: *const Expression) EvalError!Object {
        const value = try self.evaluate(expr.assign.value);
        try self.env.assign(expr.assign.name.literal, value);
        return value;
    }

    fn eval_variable(self: *Evaluator, expr: *const Expression) EvalError!Object {
        return self.env.get(expr.variable.name.literal) catch |err| {
            try Report.err("Undefined variable '{s}'\n[line {}]\n", .{
                expr.variable.name.literal,
                expr.get_line(),
            });
            return err;
        };
    }

    fn eval_grouping(self: *Evaluator, expr: *const Expression) EvalError!Object {
        return try self.evaluate(expr.grouping.expression);
    }

    fn eval_literal(_: *Evaluator, expr: *const Expression) EvalError!Object {
        switch (expr.literal.lexeme) {
            .number => return .{ .number = expr.literal.lexeme.number },
            .string => return .{ .string = expr.literal.lexeme.string },
            .boolean => return .{ .boolean = expr.literal.lexeme.boolean },
            .nullptr => return .{ .nil = {} },
        }
    }

    fn eval_unary(self: *Evaluator, expr: *const Expression) EvalError!Object {
        const right = try self.evaluate(expr.unary.right);

        switch (expr.unary.operator.token_type) {
            .MINUS => return .{ .number = -try right.try_as_number(expr) },
            .BANG => {
                return .{ .boolean = !right.is_truth() };
            },
            else => unreachable,
        }

        unreachable;
    }

    fn eval_binary(self: *Evaluator, expr: *const Expression) EvalError!Object {
        const left = try self.evaluate(expr.binary.left);
        const right = try self.evaluate(expr.binary.right);

        switch (expr.binary.operator.token_type) {
            .MINUS => return .{ .number = try left.try_as_number(expr) - try right.try_as_number(expr) },
            .SLASH => return .{ .number = try left.try_as_number(expr) / try right.try_as_number(expr) },
            .STAR => return .{ .number = try left.try_as_number(expr) * try right.try_as_number(expr) },
            .PLUS => return self.plus(&left, &right),
            .GREATER => return .{ .boolean = try left.try_as_number(expr) > try right.try_as_number(expr) },
            .GREATER_EQUAL => return .{ .boolean = try left.try_as_number(expr) >= try right.try_as_number(expr) },
            .LESS => return .{ .boolean = try left.try_as_number(expr) < try right.try_as_number(expr) },
            .LESS_EQUAL => return .{ .boolean = try left.try_as_number(expr) <= try right.try_as_number(expr) },
            .BANG_EQUAL => return .{ .boolean = !self.is_equal(&left, &right) },
            .EQUAL_EQUAL => return .{ .boolean = self.is_equal(&left, &right) },
            else => unreachable,
        }

        unreachable;
    }
};

test "eval normal" {
    var evaluator = Evaluator.init(std.testing.allocator);
    defer evaluator.deinit();

    const obj = try evaluator.evaluate(&Expression{ .binary = .{
        .left = &Expression{
            .literal = .{
                .lexeme = .{ .number = 12.0 },
            },
        },
        .right = &Expression{
            .literal = .{
                .lexeme = .{ .number = 3.0 },
            },
        },
        .operator = .{
            .token_type = .PLUS,
            .line = 1,
            .literal = "+",
            .lexeme = null,
        },
    } });
    const buf = try std.fmt.allocPrint(std.testing.allocator, "{}", .{obj});
    defer std.testing.allocator.free(buf);
    try std.testing.expectEqualStrings("15", buf);
}

test "eval abnormal" {
    var evaluator = Evaluator.init(std.testing.allocator);
    defer evaluator.deinit();

    _ = evaluator.evaluate(&Expression{ .binary = .{
        .left = &Expression{
            .literal = .{
                .lexeme = .{ .string = "hello" },
            },
        },
        .right = &Expression{
            .literal = .{
                .lexeme = .{ .number = 3.0 },
            },
        },
        .operator = .{
            .token_type = .STAR,
            .line = 1,
            .literal = "*",
            .lexeme = null,
        },
    } }) catch |err| {
        try std.testing.expectEqual(err, EvalError.UnsupportedOperator);
    };
}

pub fn evaluate(content: []const u8) !void {
    var scanner = Scan.Scanner.init(content);
    var tokens = try scanner.scan(std.heap.page_allocator);
    defer tokens.deinit();

    var parser = Parse.Parser.init(try tokens.toOwnedSlice(), std.heap.page_allocator);
    defer parser.deinit();
    const expr = parser.parse_expr() catch {
        std.process.exit(65);
    };

    var evaluator = Evaluator.init(std.heap.page_allocator);
    defer evaluator.deinit();
    const obj = evaluator.evaluate(expr) catch {
        std.process.exit(70);
    };
    try Report.print("{}\n", .{obj});
}

pub fn run(content: []const u8) !void {
    var scanner = Scan.Scanner.init(content);
    var tokens = try scanner.scan(std.heap.page_allocator);
    defer tokens.deinit();

    var parser = Parse.Parser.init(try tokens.toOwnedSlice(), std.heap.page_allocator);
    defer parser.deinit();
    const statements = parser.parse() catch {
        std.process.exit(65);
    };

    var evaluator = Evaluator.init(std.heap.page_allocator);
    defer evaluator.deinit();
    evaluator.interpret(statements) catch {
        std.process.exit(70);
    };
}
