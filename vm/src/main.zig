const std = @import("std");
const expect = std.testing.expect;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Opcode = enum(u8) {
    op_constant,
    op_return,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(f64),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(f64).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn writeChunk(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    pub fn addConstant(self: *Chunk, value: f64) !u8 {
        try self.constants.append(value);
        const index: u8 = @intCast(self.constants.items.len - 1);
        return index;
    }

    fn printOpcode(opcode: Opcode) void {
        var buf: [16]u8 = undefined;
        const str = std.ascii.upperString(&buf, @tagName(opcode));
        std.debug.print("{s}", .{str});
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
                    std.debug.print(" {d}\n", .{value});
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

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const constIndex = try chunk.addConstant(42.5);
    try chunk.writeChunk(@intFromEnum(Opcode.op_constant));
    try chunk.writeChunk(constIndex);
    try chunk.writeChunk(@intFromEnum(Opcode.op_return));

    chunk.disassemble();

    std.process.cleanExit();
}
