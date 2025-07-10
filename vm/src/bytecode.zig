const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const isDigit = std.ascii.isDigit;
const activeTag = std.meta.activeTag;

pub const Opcode = enum(u8) {
    op_constant,
    op_nil,
    op_true,
    op_false,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_not,
    op_negate,
    op_return,

    pub fn print(opcode: Opcode) void {
        var buf: [16]u8 = undefined;
        const str = std.ascii.upperString(&buf, @tagName(opcode));
        std.debug.print("{s}", .{str});
    }
};

pub const Value = union(enum) {
    bool: bool,
    num: f64,
    nil: bool, // TODO: find a more idiomatic way for doing this

    pub fn fromNum(num: f64) Value {
        return .{ .num = num };
    }

    pub fn fromBool(b: bool) Value {
        return .{ .bool = b };
    }

    pub fn fromNil() Value {
        return .{ .nil = true };
    }

    pub fn isNum(self: Value) bool {
        return switch (self) {
            .num => true,
            else => false,
        };
    }

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .nil => true,
            .bool => |b| !b,
            else => false,
        };
    }

    pub fn equal(a: Value, b: Value) bool {
        if (activeTag(a) != activeTag(b)) return false;
        return switch (a) {
            .bool => a.bool == b.bool,
            .num => a.num == b.num,
            .nil => true,
        };
    }

    pub fn print(self: Value) void {
        switch (self) {
            .bool => |b| std.debug.print("{any}", .{b}),
            .num => |num| std.debug.print("{d}", .{num}),
            .nil => std.debug.print("nil", .{}),
        }
    }
};

const KEYWORDS = std.StaticStringMap(Opcode).initComptime(.{
    .{ "CONST", .op_constant },
    .{ "PUSH_NIL", .op_nil },
    .{ "PUSH_TRUE", .op_true },
    .{ "PUSH_FALSE", .op_false },
    .{ "EQ", .op_equal },
    .{ "GT", .op_greater },
    .{ "LT", .op_less },
    .{ "ADD", .op_add },
    .{ "SUB", .op_subtract },
    .{ "MUL", .op_multiply },
    .{ "DIV", .op_divide },
    .{ "NOT", .op_not },
    .{ "NEG", .op_negate },
    .{ "RET", .op_return },
});

fn isAlpha(c: u8) bool {
    return switch (c) {
        'A'...'Z', 'a'...'z', '_' => true,
        else => false,
    };
}

fn scanOpcode(src: []u8, curr: *usize) Opcode {
    const start: usize = curr.* - 1;
    while (curr.* < src.len and isAlpha(src[curr.*])) {
        curr.* += 1;
    }
    const str = src[start..curr.*];
    return KEYWORDS.get(str).?;
}

fn scanNumber(src: []u8, curr: *usize) !f64 {
    const start: usize = curr.*;
    while (curr.* < src.len and isDigit(src[curr.*])) {
        curr.* += 1;
    }
    if (src[curr.*] == '.') {
        curr.* += 1;
        while (curr.* < src.len and isDigit(src[curr.*])) {
            curr.* += 1;
        }
    }
    const str = src[start..curr.*];
    return try std.fmt.parseFloat(f64, str);
}

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

    fn writeOpcode(self: *Chunk, opcode: Opcode) !void {
        try self.writeChunk(@intFromEnum(opcode));
    }

    fn writeConstant(self: *Chunk, value: Value) !void {
        const index = try self.addConstant(value);
        try self.writeOpcode(Opcode.op_constant);
        try self.writeChunk(index);
    }

    fn writeChunk(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    fn addConstant(self: *Chunk, value: Value) !u8 {
        try self.constants.append(value);
        const index: u8 = @intCast(self.constants.items.len - 1);
        return index;
    }

    pub fn load(self: *Chunk, src: []u8) !void {
        var curr: usize = 0;
        while (curr < src.len) {
            const c = src[curr];
            curr += 1;
            switch (c) {
                ' ', '\n' => {},
                'A'...'Z' => {
                    const opcode = scanOpcode(src, &curr);
                    if (opcode == Opcode.op_constant) {
                        curr += 1;
                        const num = try scanNumber(src, &curr);
                        try self.writeConstant(Value{ .num = num });
                    } else {
                        try self.writeOpcode(opcode);
                    }
                },
                else => return error.UnexpectedCharacter,
            }
        }
    }

    pub fn disassemble(self: *Chunk) void {
        var i: usize = 0;
        while (i < self.code.items.len) {
            const byte = self.code.items[i];
            const opcode: Opcode = @enumFromInt(byte);
            switch (opcode) {
                .op_constant => {
                    const constIndex = self.code.items[i + 1];
                    const value: Value = self.constants.items[constIndex];
                    opcode.print();
                    std.debug.print(" ", .{});
                    value.print();
                    std.debug.print("\n", .{});
                    i += 2;
                },
                else => {
                    opcode.print();
                    std.debug.print("\n", .{});
                    i += 1;
                },
            }
        }
    }
};
