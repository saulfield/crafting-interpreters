const std = @import("std");

pub const GC = struct {
    pub const ObjType = enum {
        str,
    };

    pub const Object = struct {
        data: union(ObjType) {
            str: []u8,
        },
    };

    allocator: std.mem.Allocator,
    objects: std.ArrayList(*Object),
    strings: std.StringHashMap(*Object),

    pub fn init(allocator: std.mem.Allocator) GC {
        return GC{
            .allocator = allocator,
            .objects = std.ArrayList(*Object).init(allocator),
            .strings = std.StringHashMap(*Object).init(allocator),
        };
    }

    pub fn deinit(self: *GC) void {
        for (self.objects.items) |object| {
            switch (object.*.data) {
                .str => |str| self.allocator.free(str),
            }
            self.allocator.destroy(object);
        }
        self.objects.deinit();
        self.strings.deinit();
    }

    pub fn createObject(self: *GC) !*Object {
        const object = try self.allocator.create(Object);
        try self.objects.append(object);
        return object;
    }

    pub fn allocAndCopyString(self: *GC, str: []u8) ![]u8 {
        return try self.allocator.dupe(u8, str);
    }

    pub fn allocString(self: *GC, len: usize) ![]u8 {
        return try self.allocator.alloc(u8, len);
    }

    pub fn createStrObject(self: *GC, str: []u8) !*Object {
        if (self.strings.get(str)) |strObj| {
            self.allocator.free(str);
            return strObj;
        }
        var strObj = try self.createObject();
        strObj.data.str = str;
        try self.strings.put(str, strObj);
        return strObj;
    }
};
