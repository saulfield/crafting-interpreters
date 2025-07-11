const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const isDigit = std.ascii.isDigit;
const activeTag = std.meta.activeTag;

const GC = @import("gc.zig").GC;
const ObjType = GC.ObjType;
const Object = GC.Object;

pub const Opcode = enum(u8) {
    op_constant,
    op_nil,
    op_true,
    op_false,
    op_pop,
    op_define_global,
    op_get_global,
    op_set_global,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_not,
    op_negate,
    op_print,
    op_return,

    pub fn print(opcode: Opcode) void {
        var buf: [16]u8 = undefined;
        const str = std.ascii.upperString(&buf, @tagName(opcode));
        std.debug.print("{s}", .{str});
    }

    pub fn hasOperand(opcode: Opcode) bool {
        return switch (opcode) {
            .op_constant, .op_define_global, .op_get_global, .op_set_global => true,
            else => false,
        };
    }
};

pub const Value = union(enum) {
    bool: bool,
    num: f64,
    nil: bool, // TODO: find a more idiomatic way for doing this
    obj: *Object,

    pub fn fromNum(num: f64) Value {
        return .{ .num = num };
    }

    pub fn fromBool(b: bool) Value {
        return .{ .bool = b };
    }

    pub fn fromNil() Value {
        return .{ .nil = true };
    }

    pub fn fromObj(obj: *Object) Value {
        return .{ .obj = obj };
    }

    pub fn isNum(self: Value) bool {
        return switch (self) {
            .num => true,
            else => false,
        };
    }

    pub fn isStr(self: Value) bool {
        return switch (self) {
            .obj => |obj| activeTag(obj.data) == ObjType.str,
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
            .obj => a.obj == b.obj,
        };
    }

    pub fn print(self: Value) void {
        switch (self) {
            .bool => |b| std.debug.print("{any}", .{b}),
            .num => |num| std.debug.print("{d}", .{num}),
            .nil => std.debug.print("nil", .{}),
            .obj => |obj| {
                switch (obj.*.data) {
                    .str => |s| std.debug.print("\"{s}\"", .{s}),
                }
            },
        }
    }
};

const KEYWORDS = std.StaticStringMap(Opcode).initComptime(.{
    .{ "CONST", .op_constant },
    .{ "PUSH_NIL", .op_nil },
    .{ "PUSH_TRUE", .op_true },
    .{ "PUSH_FALSE", .op_false },
    .{ "POP", .op_pop },
    .{ "DEFINE_GLOBAL", .op_define_global },
    .{ "GET_GLOBAL", .op_get_global },
    .{ "SET_GLOBAL", .op_set_global },
    .{ "EQ", .op_equal },
    .{ "GT", .op_greater },
    .{ "LT", .op_less },
    .{ "ADD", .op_add },
    .{ "SUB", .op_subtract },
    .{ "MUL", .op_multiply },
    .{ "DIV", .op_divide },
    .{ "NOT", .op_not },
    .{ "NEG", .op_negate },
    .{ "PRINT", .op_print },
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

fn scanString(src: []u8, curr: *usize) ![]u8 {
    const start: usize = curr.*;
    while (curr.* < src.len and src[curr.*] != '"') {
        curr.* += 1;
    }
    const str = src[start..curr.*];
    curr.* += 1;
    return str;
}

pub fn load(chunk: *Chunk, gc: *GC, src: []u8) !void {
    var curr: usize = 0;
    while (curr < src.len) {
        const c = src[curr];
        curr += 1;
        switch (c) {
            ' ', '\n' => {},
            'A'...'Z' => {
                const opcode = scanOpcode(src, &curr);
                if (!opcode.hasOperand()) {
                    try chunk.writeOpcode(opcode);
                    continue;
                }

                curr += 1; // skip space
                if (isDigit(src[curr])) {
                    const num = try scanNumber(src, &curr);
                    try chunk.writeConstInstr(Value.fromNum(num), opcode);
                } else {
                    std.debug.assert(src[curr] == '"');
                    curr += 1;
                    const str = try scanString(src, &curr);
                    const gcStr = try gc.allocAndCopyString(str);
                    const strObject = try gc.createStrObject(gcStr);
                    try chunk.writeConstInstr(Value.fromObj(strObject), opcode);
                }
            },
            else => return error.UnexpectedCharacter,
        }
    }
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

    fn writeConstInstr(self: *Chunk, value: Value, opcode: Opcode) !void {
        const index = try self.addConstant(value);
        try self.writeOpcode(opcode);
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

    pub fn disassemble(self: *Chunk) void {
        var i: usize = 0;
        while (i < self.code.items.len) {
            const byte = self.code.items[i];
            const opcode: Opcode = @enumFromInt(byte);
            switch (opcode.hasOperand()) {
                true => {
                    const constIndex = self.code.items[i + 1];
                    const value: Value = self.constants.items[constIndex];
                    opcode.print();
                    std.debug.print(" ", .{});
                    value.print();
                    std.debug.print("\n", .{});
                    i += 2;
                },
                false => {
                    opcode.print();
                    std.debug.print("\n", .{});
                    i += 1;
                },
            }
        }
    }
};
