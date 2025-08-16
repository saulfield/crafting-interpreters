const std = @import("std");
const ozlox = @import("ozlox.zig");
const Object = ozlox.Object;
const ObjFunction = ozlox.ObjFunction;
const Chunk = ozlox.Chunk;

pub const GC = struct {
    allocator: std.mem.Allocator,
    objects: std.ArrayList(*Object),
    strings: std.StringHashMap(*Object),
    chunks: std.ArrayList(*Chunk),

    pub fn init(allocator: std.mem.Allocator) GC {
        return GC{
            .allocator = allocator,
            .objects = std.ArrayList(*Object).init(allocator),
            .strings = std.StringHashMap(*Object).init(allocator),
            .chunks = std.ArrayList(*Chunk).init(allocator),
        };
    }

    pub fn deinit(self: *GC) void {
        for (self.objects.items) |obj| {
            switch (obj.*.data) {
                .str => |str| self.allocator.free(str),
                // .func => |func| func.*.chunk.deinit(),
            }
            self.allocator.destroy(obj);
        }
        for (self.chunks.items) |chunk| {
            chunk.deinit();
            self.allocator.destroy(chunk);
        }
        self.objects.deinit();
        self.strings.deinit();
        self.chunks.deinit();
    }

    pub fn createObject(self: *GC) !*Object {
        const object = try self.allocator.create(Object);
        try self.objects.append(object);
        return object;
    }

    pub fn allocAndCopyString(self: *GC, str: []const u8) ![]u8 {
        return try self.allocator.dupe(u8, str);
    }

    pub fn allocString(self: *GC, len: usize) ![]u8 {
        return try self.allocator.alloc(u8, len);
    }

    pub fn createStrObject(self: *GC, str: []u8) !*Object {
        if (self.strings.get(str)) |obj| {
            self.allocator.free(str);
            return obj;
        }
        const obj = try self.createObject();
        obj.*.data.str = str;
        try self.strings.put(str, obj);
        return obj;
    }

    pub fn createChunk(self: *GC) !*Chunk {
        const chunk = try Chunk.create(self.allocator);
        try self.chunks.append(chunk);
        return chunk;
    }

    // pub fn createFuncObject(self: *GC) !*Object {
    //     var funObj = try self.createObject();
    //     funObj.func = .{
    //         .arity = 0,
    //         .name = &.{},
    //         .chunk = Chunk.init(self.allocator),
    //     };
    //     return funObj;
    // }
};
