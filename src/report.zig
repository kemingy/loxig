const std = @import("std");

pub fn print(comptime fmt: []const u8, args: anytype) !void {
    const writer = std.io.getStdOut().writer();
    try writer.print(fmt, args);
}

pub fn err(comptime fmt: []const u8, args: anytype) !void {
    const writer = std.io.getStdErr().writer();
    try writer.print(fmt, args);
}
