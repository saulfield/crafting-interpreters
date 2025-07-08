const std = @import("std");
const Allocator = std.mem.Allocator;

const Lox = @import("lox.zig");
const Chunk = @import("chunk.zig").Chunk;
const VM = @import("vm.zig").VM;
const Value = Lox.Value;
const Opcode = Lox.Opcode;

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    // var constIndex: u8 = undefined;
    try chunk.writeChunk(@intFromEnum(Opcode.op_constant));
    try chunk.writeChunk(try chunk.addConstant(Value{ .num = 1.2 }));
    try chunk.writeChunk(@intFromEnum(Opcode.op_constant));
    try chunk.writeChunk(try chunk.addConstant(Value{ .num = 3.4 }));
    try chunk.writeChunk(@intFromEnum(Opcode.op_add));
    try chunk.writeChunk(@intFromEnum(Opcode.op_constant));
    try chunk.writeChunk(try chunk.addConstant(Value{ .num = 5.6 }));
    try chunk.writeChunk(@intFromEnum(Opcode.op_divide));
    try chunk.writeChunk(@intFromEnum(Opcode.op_negate));
    try chunk.writeChunk(@intFromEnum(Opcode.op_return));
    chunk.disassemble();

    var vm = VM.init(allocator);
    const result = vm.interpret(&chunk);
    std.debug.print("{any}\n", .{result});

    std.process.cleanExit();
}
