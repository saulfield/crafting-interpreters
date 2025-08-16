pub const ObjType = enum {
    str,
};

pub const Object = struct {
    data: union(ObjType) {
        str: []u8,
    },
};
