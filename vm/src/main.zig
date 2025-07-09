const std = @import("std");
const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;

const Lox = @import("lox.zig");
const Chunk = @import("chunk.zig").Chunk;
const VM = @import("vm.zig").VM;
const Value = Lox.Value;
const Opcode = Lox.Opcode;

fn loadBytecode(chunk: *Chunk, src: []u8) !void {
    _ = src;

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
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    // read bytecode text file
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const src = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(src);

    // load bytecode
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    try loadBytecode(&chunk, src);
    chunk.disassemble();

    // run interpreter
    var vm = VM.init(allocator);
    const result = vm.interpret(&chunk);
    std.debug.print("{any}\n", .{result});
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = args[1..];
    switch (options.len) {
        1 => try runFile(allocator, options[0]),
        else => {
            try stdout.print("Usage: vm [script]\n", .{});
            return error.InvalidArgs;
        },
    }

    std.process.cleanExit();
}
