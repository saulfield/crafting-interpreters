const std = @import("std");

pub const GC = struct {
    pub const Object = struct {
        data: union(enum) {
            str: []u8,
        },
    };

    allocator: std.mem.Allocator,
    objects: std.ArrayList(*Object),

    pub fn init(allocator: std.mem.Allocator) GC {
        return GC{
            .allocator = allocator,
            .objects = std.ArrayList(*Object).init(allocator),
        };
    }

    pub fn deinit(self: *GC) void {
        for (self.objects.items) |object| {
            switch (object.*.data) {
                .str => |s| self.allocator.free(s),
            }
            self.allocator.destroy(object);
        }
        self.objects.deinit();
    }

    pub fn createObject(self: *GC) !*Object {
        const object = try self.allocator.create(Object);
        try self.objects.append(object);
        return object;
    }

    pub fn createStrObject(self: *GC, str: []u8) !*Object {
        var object = try self.createObject();
        const copiedStr = try self.allocator.dupe(u8, str);
        object.data.str = copiedStr;
        return object;
    }
};
