const std = @import("std");

pub var had_error = false;

pub fn reportError(line: u32, msg: []const u8) void {
    report(line, "", msg);
}

fn report(line: u32, where: []const u8, msg: []const u8) void {
    std.debug.print("[line {any}] Error{s}: {s}\n", .{ line, where, msg });
    had_error = true;
}
