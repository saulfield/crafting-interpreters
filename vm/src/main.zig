const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();

const GC = @import("gc.zig").GC;
const VM = @import("vm.zig").VM;
const Bytecode = @import("bytecode.zig");
const Chunk = Bytecode.Chunk;
const Value = Bytecode.Value;
const Opcode = Bytecode.Opcode;

fn runFile(allocator: Allocator, path: []const u8) !void {
    // read bytecode text file
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const src = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(src);

    // init GC
    var gc = GC.init(allocator);
    defer gc.deinit();

    // load bytecode
    var chunk = Chunk.init(allocator);
    try Bytecode.load(&chunk, &gc, src);
    defer chunk.deinit();
    chunk.disassemble();

    // run interpreter
    var vm = VM.init(&gc);
    const result = try vm.interpret(&chunk);
    _ = result;
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
