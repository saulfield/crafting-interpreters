const ozlox = @import("ozlox.zig");
const Chunk = ozlox.Chunk;

pub const ObjFunction = struct {
    arity: i32,
    name: []u8,
    chunk: Chunk,
};

pub const ObjType = enum {
    str,
    // func,
};

pub const Object = struct {
    data: union(ObjType) {
        str: []u8,
        // func: *ObjFunction,
    },
};

// pub const Object = union(enum) {
//     strObj: []u8,
//     funObj: ObjFunction,
// };
