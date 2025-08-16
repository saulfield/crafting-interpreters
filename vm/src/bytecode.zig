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
    op_get_local,
    op_set_local,
    op_get_global,
    op_set_global,
    op_define_global,
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
    op_jump,
    op_jump_if_false,
    op_loop,
    op_call,
    op_return,
    op_func_begin,
    op_func_end,

    pub fn print(opcode: Opcode) void {
        var buf: [16]u8 = undefined;
        const str = std.ascii.upperString(&buf, @tagName(opcode));
        std.debug.print("{s}", .{str});
    }

    pub fn hasOperand(opcode: Opcode) bool {
        return switch (opcode) {
            .op_func_begin,
            .op_func_end,
            .op_constant,
            .op_define_global,
            .op_get_global,
            .op_set_global,
            .op_get_local,
            .op_set_local,
            .op_jump,
            .op_jump_if_false,
            .op_loop,
            .op_call,
            => true,
            else => false,
        };
    }

    pub fn isLocalOrCallInstr(opcode: Opcode) bool {
        return switch (opcode) {
            .op_get_local, .op_set_local, .op_call => true,
            else => false,
        };
    }

    pub fn isJumpInstr(opcode: Opcode) bool {
        return switch (opcode) {
            .op_jump,
            .op_jump_if_false,
            .op_loop,
            => true,
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
    .{ "FUNC_BEGIN", .op_func_begin },
    .{ "FUNC_END", .op_func_end },
    .{ "CONST", .op_constant },
    .{ "PUSH_NIL", .op_nil },
    .{ "PUSH_TRUE", .op_true },
    .{ "PUSH_FALSE", .op_false },
    .{ "POP", .op_pop },
    .{ "GET_LOCAL", .op_get_local },
    .{ "SET_LOCAL", .op_set_local },
    .{ "GET_GLOBAL", .op_get_global },
    .{ "SET_GLOBAL", .op_set_global },
    .{ "DEFINE_GLOBAL", .op_define_global },
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
    .{ "JUMP", .op_jump },
    .{ "JUMP_IF_FALSE", .op_jump_if_false },
    .{ "LOOP", .op_loop },
    .{ "CALL", .op_call },
    .{ "RET", .op_return },
});

pub const Compiler = struct {
    allocator: Allocator,
    gc: *GC,
    chunks: ArrayList(*Chunk),
    curr: usize,
    src: []const u8,

    pub fn init(allocator: Allocator, gc: *GC, src: []const u8) Compiler {
        return .{
            .allocator = allocator,
            .gc = gc,
            .chunks = ArrayList(*Chunk).init(allocator),
            .src = src,
            .curr = 0,
        };
    }

    pub fn deinit(self: *Compiler) void {
        for (self.chunks.items) |chunk| {
            chunk.deinit();
            self.allocator.destroy(chunk);
        }
        self.chunks.deinit();
    }

    fn isAlpha(c: u8) bool {
        return switch (c) {
            'A'...'Z', 'a'...'z', '_' => true,
            else => false,
        };
    }

    fn scanOpcode(self: *Compiler) Opcode {
        const start: usize = self.curr - 1;
        while (self.curr < self.src.len and isAlpha(self.src[self.curr])) {
            self.curr += 1;
        }
        const str = self.src[start..self.curr];
        return KEYWORDS.get(str).?;
    }

    fn scanNumber(self: *Compiler) !f64 {
        const start: usize = self.curr;
        while (self.curr < self.src.len and isDigit(self.src[self.curr])) {
            self.curr += 1;
        }
        if (self.src[self.curr] == '.') {
            self.curr += 1;
            while (self.curr < self.src.len and isDigit(self.src[self.curr])) {
                self.curr += 1;
            }
        }
        const str = self.src[start..self.curr];
        return try std.fmt.parseFloat(f64, str);
    }

    fn scanInt(self: *Compiler, comptime T: type) !T {
        const start: usize = self.curr;
        while (self.curr < self.src.len and isDigit(self.src[self.curr])) {
            self.curr += 1;
        }
        const str = self.src[start..self.curr];
        return try std.fmt.parseInt(T, str, 0);
    }

    fn scanString(self: *Compiler) []const u8 {
        const start: usize = self.curr;
        while (self.curr < self.src.len and self.src[self.curr] != '"') {
            self.curr += 1;
        }
        const str = self.src[start..self.curr];
        self.curr += 1;
        return str;
    }

    // Reads a sequence of bytecode instructions into a new chunk
    pub fn loadChunk(self: *Compiler) !*Chunk {
        var chunk = try self.allocator.create(Chunk);
        chunk.from_alloc(self.allocator);
        try self.chunks.append(chunk);
        while (self.curr < self.src.len) {
            const c = self.src[self.curr];
            self.curr += 1;
            switch (c) {
                ' ', '\n' => {},
                'A'...'Z' => {
                    const opcode = self.scanOpcode();
                    if (!opcode.hasOperand()) {
                        try chunk.writeOpcode(opcode);
                        continue;
                    }

                    self.curr += 1; // skip space
                    switch (opcode) {
                        .op_func_begin => {
                            self.curr += 1;
                            const name = self.scanString();
                            self.curr += 1;
                            const arity = try self.scanInt(u8);
                            _ = name;
                            _ = arity;
                            // const innerChunk = self.loadChunk();
                            continue;
                        },
                        .op_func_end => {
                            // return &chunk;
                            continue;
                        },
                        else => {},
                    }

                    if (opcode.isLocalOrCallInstr()) {
                        const arg = try self.scanInt(u8);
                        try chunk.writeOpcode(opcode);
                        try chunk.writeChunk(arg);
                    } else if (opcode.isJumpInstr()) {
                        const offset = try self.scanInt(u16);
                        try chunk.writeOpcode(opcode);
                        try chunk.writeChunk(@intCast((offset >> 8) & 0xFF));
                        try chunk.writeChunk(@intCast(offset & 0xFF));
                    } else if (isDigit(self.src[self.curr])) {
                        const num = try self.scanNumber();
                        try chunk.writeConstInstr(Value.fromNum(num), opcode);
                    } else {
                        std.debug.assert(self.src[self.curr] == '"');
                        self.curr += 1;
                        const str = self.scanString();
                        const gcStr = try self.gc.allocAndCopyString(str);
                        const strObject = try self.gc.createStrObject(gcStr);
                        try chunk.writeConstInstr(Value.fromObj(strObject), opcode);
                    }
                },
                else => return error.UnexpectedCharacter,
            }
        }
        return chunk;
    }
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn from_alloc(self: *Chunk, allocator: Allocator) void {
        self.code = ArrayList(u8).init(allocator);
        self.constants = ArrayList(Value).init(allocator);
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
