const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

fn run(src: []const u8) !void {
    try stdout.print("{s}\n", .{src});
}

fn run_prompt() !void {
    var buffer: [512]u8 = undefined;
    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterOrEof(&buffer, '\n');
        if (line) |val| {
            try stdout.print("{s}\n", .{val});
        } else {
            try stdout.print("\n", .{});
            break;
        }
    }
}

fn run_file(path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const mb = (1 << 10) << 10;
    const src = try file.readToEndAlloc(allocator, 1 * mb);
    try run(src);
}

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    // for (args, 0..) |arg, i| {
    //     try stdout.print("arg {}: {s}\n", .{ i, arg });
    // }

    const options = args[1..];
    switch (options.len) {
        0 => try run_prompt(),
        1 => try run_file(options[0]),
        else => {
            try stdout.print("Usage: zlox [script]\n", .{});
            return error.InvalidArgs;
        },
    }
}
