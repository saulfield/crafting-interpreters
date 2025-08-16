const std = @import("std");

pub const Opcode = enum(u8) {
    op_constant,
    op_nil,
    op_true,
    op_false,
    op_pop,
    op_get_local,
    op_set_local,
    op_get_global,
    op_set_global,
    op_define_global,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_not,
    op_negate,
    op_print,
    op_jump,
    op_jump_if_false,
    op_loop,
    op_call,
    op_return,
    op_func_begin,
    op_func_end,

    pub fn print(opcode: Opcode) void {
        var buf: [16]u8 = undefined;
        const str = std.ascii.upperString(&buf, @tagName(opcode));
        std.debug.print("{s}", .{str});
    }

    pub fn hasOperand(opcode: Opcode) bool {
        return switch (opcode) {
            .op_func_begin,
            .op_func_end,
            .op_constant,
            .op_define_global,
            .op_get_global,
            .op_set_global,
            .op_get_local,
            .op_set_local,
            .op_jump,
            .op_jump_if_false,
            .op_loop,
            .op_call,
            => true,
            else => false,
        };
    }

    pub fn isLocalOrCallInstr(opcode: Opcode) bool {
        return switch (opcode) {
            .op_get_local, .op_set_local, .op_call => true,
            else => false,
        };
    }

    pub fn isJumpInstr(opcode: Opcode) bool {
        return switch (opcode) {
            .op_jump,
            .op_jump_if_false,
            .op_loop,
            => true,
            else => false,
        };
    }
};
