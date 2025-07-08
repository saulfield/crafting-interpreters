const std = @import("std");

pub const Opcode = enum(u8) {
    op_constant,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_negate,
    op_return,
};

pub const Value = union(enum) {
    num: f64,
    bool: bool,
};

pub fn printOpcode(opcode: Opcode) void {
    var buf: [16]u8 = undefined;
    const str = std.ascii.upperString(&buf, @tagName(opcode));
    std.debug.print("{s}", .{str});
}

pub fn printValue(value: Value) void {
    switch (value) {
        .num => |num| std.debug.print("{d}", .{num}),
        else => std.debug.print("{any}", .{value}),
    }
}
