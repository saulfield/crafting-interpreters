const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lox = @import("lox.zig");
const Value = Lox.Value;
const Opcode = Lox.Opcode;
const printOpcode = Lox.printOpcode;
const printValue = Lox.printValue;

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn writeChunk(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        try self.constants.append(value);
        const index: u8 = @intCast(self.constants.items.len - 1);
        return index;
    }

    pub fn disassemble(self: *Chunk) void {
        var i: usize = 0;
        while (i < self.code.items.len) {
            const byte = self.code.items[i];
            const opcode: Opcode = @enumFromInt(byte);
            switch (opcode) {
                .op_constant => {
                    const constIndex = self.code.items[i + 1];
                    const value = self.constants.items[constIndex];
                    printOpcode(opcode);
                    std.debug.print(" ", .{});
                    printValue(value);
                    std.debug.print("\n", .{});
                    i += 2;
                },
                else => {
                    printOpcode(opcode);
                    std.debug.print("\n", .{});
                    i += 1;
                },
            }
        }
    }
};
