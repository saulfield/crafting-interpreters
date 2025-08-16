const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ozlox = @import("ozlox.zig");
const Value = ozlox.Value;
const Opcode = ozlox.Opcode;

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn create(allocator: Allocator) !*Chunk {
        const chunk = try allocator.create(Chunk);
        chunk.* = .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
        return chunk;
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn writeOpcode(self: *Chunk, opcode: Opcode) !void {
        try self.writeChunk(@intFromEnum(opcode));
    }

    pub fn writeConstInstr(self: *Chunk, value: Value, opcode: Opcode) !void {
        const index = try self.addConstant(value);
        try self.writeOpcode(opcode);
        try self.writeChunk(index);
    }

    pub fn writeChunk(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    fn addConstant(self: *Chunk, value: Value) !u8 {
        try self.constants.append(value);
        const index: u8 = @intCast(self.constants.items.len - 1);
        return index;
    }

    pub fn disassemble(self: *Chunk) void {
        var i: usize = 0;
        while (i < self.code.items.len) {
            const byte = self.code.items[i];
            const opcode: Opcode = @enumFromInt(byte);
            if (opcode.hasOperand()) {
                if (opcode.isLocalOrCallInstr()) {
                    const arg = self.code.items[i + 1];
                    opcode.print();
                    std.debug.print(" {d}\n", .{arg});
                    i += 2;
                } else if (opcode.isJumpInstr()) {
                    const upper = @as(u16, self.code.items[i + 1]);
                    const lower = @as(u16, self.code.items[i + 2]);
                    const offset: u16 = @intCast((upper << 8) | lower);
                    opcode.print();
                    std.debug.print(" {d}\n", .{offset});
                    i += 3;
                } else {
                    opcode.print();
                    std.debug.print(" ", .{});

                    const constIndex = self.code.items[i + 1];
                    const value: Value = self.constants.items[constIndex];
                    value.print();
                    std.debug.print("\n", .{});
                    i += 2;
                }
            } else {
                opcode.print();
                std.debug.print("\n", .{});
                i += 1;
            }
        }
    }
};
