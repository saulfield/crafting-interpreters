const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();

const ozlox = @import("ozlox.zig");
const Compiler = ozlox.Compiler;
const GC = ozlox.GC;
const VM = ozlox.VM;

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
    var compiler = Compiler.init(&gc, src);
    const functionObj = try compiler.compile(false);
    // functionObj.chunk.disassemble();

    // run interpreter
    var vm = VM.init(allocator, &gc);
    defer vm.deinit();
    const result = try vm.run(functionObj);
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
