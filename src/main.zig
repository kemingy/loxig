const std = @import("std");
const Scan = @import("scan.zig");
const Eval = @import("evaluate.zig");
const Parser = @import("parser.zig");

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];
    const file_contents = try std.fs.cwd().readFileAlloc(
        std.heap.page_allocator,
        filename,
        std.math.maxInt(usize),
    );
    defer std.heap.page_allocator.free(file_contents);

    if (std.mem.eql(u8, command, "tokenize")) {
        try Scan.scan(file_contents);
    } else if (std.mem.eql(u8, command, "parse")) {
        try Parser.parse(file_contents);
    } else if (std.mem.eql(u8, command, "evaluate")) {
        try Eval.evaluate(file_contents);
    } else if (std.mem.eql(u8, command, "run")) {
        try Eval.run(file_contents);
    } else {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }
}
