const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;

const Scanner = @import("scanner.zig").Scanner;
const MAX_SIZE = std.math.maxInt(usize);

fn run(allocator: Allocator, src: []const u8) !void {
    var scanner = Scanner.init(allocator, src);
    const tokens = scanner.scanTokens();
    for (tokens) |token| {
        try stdout.print("{any}", .{token.kind});
        if (token.literal) |val| {
            switch (val) {
                .str => try stdout.print(" \"{s}\"", .{val.str}),
                .num => try stdout.print(" {d}", .{val.num}),
            }
        }
        try stdout.print("\n", .{});
    }
}

fn runPrompt(allocator: Allocator) !void {
    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', MAX_SIZE);
        if (line) |line_str| {
            try run(allocator, line_str);
        } else {
            try stdout.print("\n", .{});
            break;
        }
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const src = try file.readToEndAlloc(allocator, MAX_SIZE);
    try run(allocator, src);
}

pub fn main() !void {
    // Create a single allocator that will be freed at the end of the program
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse command line args
    const args = try std.process.argsAlloc(allocator);
    const options = args[1..];
    switch (options.len) {
        0 => try runPrompt(allocator),
        1 => try runFile(allocator, options[0]),
        else => {
            try stdout.print("Usage: zlox [script]\n", .{});
            return error.InvalidArgs;
        },
    }
}
