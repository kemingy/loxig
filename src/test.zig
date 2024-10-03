const std = @import("std");

// import all the tests here
comptime {
    _ = @import("expression.zig");
    _ = @import("parser.zig");
    _ = @import("scan.zig");
    _ = @import("evaluate.zig");
}

test {
    std.testing.refAllDecls(@This());
}
