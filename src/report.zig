const std = @import("std");
const builtin = @import("builtin");

pub fn print(comptime fmt: []const u8, args: anytype) !void {
    const writer = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(writer);
    nosuspend {
        writer.print(fmt, args) catch return;
        bw.flush() catch return;
    }
}

pub fn err(comptime fmt: []const u8, args: anytype) !void {
    if (builtin.is_test) {
        return;
    }
    const writer = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(writer);
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    nosuspend {
        writer.print(fmt, args) catch return;
        bw.flush() catch return;
    }
}
