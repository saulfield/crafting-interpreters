const std = @import("std");
const Allocator = std.mem.Allocator;

const Lox = @import("lox.zig");
const Chunk = @import("chunk.zig").Chunk;
const Value = Lox.Value;
const Opcode = Lox.Opcode;
const printValue = Lox.printValue;

pub const InterpretResult = enum {
    interpret_ok,
    interpret_compile_error,
    interpret_runtime_error,
};

const STACK_MAX = 256;
pub const VM = struct {
    allocator: Allocator,
    sp: usize,
    ip: usize,
    stack: [STACK_MAX]Value,
    chunk: *Chunk,

    pub fn init(allocator: Allocator) VM {
        return .{
            .allocator = allocator,
            .sp = 0,
            .ip = 0,
            .stack = undefined,
            .chunk = undefined,
        };
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    pub fn pop(self: *VM) Value {
        self.sp -= 1;
        return self.stack[self.sp];
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.*.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    fn binOp(self: *VM, opcode: Opcode) void {
        const b = self.pop();
        const a = self.pop();
        const result = switch (opcode) {
            .op_add => a.num + b.num,
            .op_subtract => a.num - b.num,
            .op_multiply => a.num * b.num,
            .op_divide => a.num / b.num,
            else => unreachable,
        };
        self.push(Value{ .num = result });
    }

    pub fn interpret(self: *VM, chunk: *Chunk) InterpretResult {
        self.ip = 0;
        self.chunk = chunk;
        while (true) {
            const opcode: Opcode = @enumFromInt(self.readByte());
            switch (opcode) {
                .op_constant => {
                    const index = self.readByte();
                    const value = self.chunk.*.constants.items[index];
                    self.push(value);
                },
                .op_add, .op_subtract, .op_multiply, .op_divide => self.binOp(opcode),
                .op_negate => {
                    const value = self.pop();
                    self.push(Value{ .num = -value.num });
                },
                .op_return => {
                    printValue(self.pop());
                    std.debug.print("\n", .{});
                    return InterpretResult.interpret_ok;
                },
            }
        }
        return InterpretResult.interpret_ok;
    }
};
