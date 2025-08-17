const ozlox = @import("ozlox.zig");
const Chunk = ozlox.Chunk;

pub const FunctionObj = struct {
    arity: u8,
    name: []const u8,
    chunk: Chunk,
};

pub const Object = union(enum) {
    str: []u8,
    func: *FunctionObj,
};
