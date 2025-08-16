const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const isDigit = std.ascii.isDigit;
const activeTag = std.meta.activeTag;

const ozlox = @import("ozlox.zig");
const Value = ozlox.Value;
const Opcode = ozlox.Opcode;
const Chunk = ozlox.Chunk;
const GC = ozlox.GC;

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

pub const BytecodeReader = struct {
    allocator: Allocator,
    gc: *GC,
    chunks: ArrayList(*Chunk),
    curr: usize,
    src: []const u8,

    pub fn init(allocator: Allocator, gc: *GC, src: []const u8) BytecodeReader {
        return .{
            .allocator = allocator,
            .gc = gc,
            .chunks = ArrayList(*Chunk).init(allocator),
            .src = src,
            .curr = 0,
        };
    }

    pub fn deinit(self: *BytecodeReader) void {
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

    fn scanOpcode(self: *BytecodeReader) Opcode {
        const start: usize = self.curr - 1;
        while (self.curr < self.src.len and isAlpha(self.src[self.curr])) {
            self.curr += 1;
        }
        const str = self.src[start..self.curr];
        return KEYWORDS.get(str).?;
    }

    fn scanNumber(self: *BytecodeReader) !f64 {
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

    fn scanInt(self: *BytecodeReader, comptime T: type) !T {
        const start: usize = self.curr;
        while (self.curr < self.src.len and isDigit(self.src[self.curr])) {
            self.curr += 1;
        }
        const str = self.src[start..self.curr];
        return try std.fmt.parseInt(T, str, 0);
    }

    fn scanString(self: *BytecodeReader) []const u8 {
        const start: usize = self.curr;
        while (self.curr < self.src.len and self.src[self.curr] != '"') {
            self.curr += 1;
        }
        const str = self.src[start..self.curr];
        self.curr += 1;
        return str;
    }

    // Reads a sequence of bytecode instructions into a new chunk
    pub fn loadChunk(self: *BytecodeReader) !*Chunk {
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
