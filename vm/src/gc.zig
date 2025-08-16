const std = @import("std");
const ozlox = @import("ozlox.zig");
const Object = ozlox.Object;
const FunctionObj = ozlox.FunctionObj;
const Chunk = ozlox.Chunk;

pub const GC = struct {
    allocator: std.mem.Allocator,
    objects: std.ArrayList(Object),
    strings: std.StringHashMap(Object),

    pub fn init(allocator: std.mem.Allocator) GC {
        return GC{
            .allocator = allocator,
            .objects = std.ArrayList(Object).init(allocator),
            .strings = std.StringHashMap(Object).init(allocator),
        };
    }

    pub fn deinit(self: *GC) void {
        for (self.objects.items) |obj| {
            switch (obj) {
                .str => |str| self.allocator.free(str),
                .func => |func| {
                    func.chunk.deinit();
                    self.allocator.destroy(func);
                },
            }
        }
        self.objects.deinit();
        self.strings.deinit();
    }

    pub fn allocAndCopyString(self: *GC, str: []const u8) ![]u8 {
        return try self.allocator.dupe(u8, str);
    }

    pub fn allocString(self: *GC, len: usize) ![]u8 {
        return try self.allocator.alloc(u8, len);
    }

    pub fn createStrObject(self: *GC, str: []u8) !Object {
        if (self.strings.get(str)) |obj| {
            self.allocator.free(str);
            return obj;
        }
        const obj = Object{ .str = str };
        try self.objects.append(obj);
        try self.strings.put(str, obj);
        return obj;
    }

    pub fn createChunk(self: *GC) !*Chunk {
        const chunk = try Chunk.create(self.allocator);
        try self.chunks.append(chunk);
        return chunk;
    }

    pub fn createFuncObject(self: *GC) !Object {
        const func = try self.allocator.create(FunctionObj);
        func.* = .{
            .arity = 0,
            .name = &.{},
            .chunk = Chunk.init(self.allocator),
        };
        const obj = Object{ .func = func };
        try self.objects.append(obj);
        return obj;
    }
};
