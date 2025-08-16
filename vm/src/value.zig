const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const isDigit = std.ascii.isDigit;
const activeTag = std.meta.activeTag;

const ozlox = @import("ozlox.zig");
const Object = ozlox.Object;
const ObjType = ozlox.ObjType;

pub const Value = union(enum) {
    nil,
    bool: bool,
    num: f64,
    obj: *Object,

    pub fn fromNum(num: f64) Value {
        return .{ .num = num };
    }

    pub fn fromBool(b: bool) Value {
        return .{ .bool = b };
    }

    pub fn fromNil() Value {
        return .nil;
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
                    // .func => |func| std.debug.print("<fn {s}>", .{func.*.name}),
                }
            },
        }
    }
};
