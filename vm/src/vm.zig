const std = @import("std");
const Allocator = std.mem.Allocator;

const Bytecode = @import("bytecode.zig");
const Chunk = Bytecode.Chunk;
const Value = Bytecode.Value;
const Opcode = Bytecode.Opcode;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
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

    pub fn peek(self: *VM, dist: usize) Value {
        return self.stack[self.sp - 1 - dist];
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

    fn binOp(self: *VM, comptime T: type, opcode: Opcode) !void {
        if (!self.peek(0).isNum() or !self.peek(1).isNum()) {
            self.runtimeError("Operands must be numbers.");
            return error.WrongOperandType;
        }
        const b = self.pop().num;
        const a = self.pop().num;

        switch (@typeInfo(T)) {
            .float => {
                const result = switch (opcode) {
                    .op_add => a + b,
                    .op_subtract => a - b,
                    .op_multiply => a * b,
                    .op_divide => a / b,
                    else => unreachable,
                };
                self.push(Value.fromNum(result));
            },
            .bool => {
                const result = switch (opcode) {
                    .op_greater => a > b,
                    .op_less => a < b,
                    else => unreachable,
                };
                self.push(Value.fromBool(result));
            },
            else => @compileError("Unsupported type."),
        }
    }

    fn runtimeError(self: *VM, message: []const u8) void {
        std.debug.print("{s}\n", .{message});
        self.sp = 0; // reset stack
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
                .op_nil => self.push(Value.fromNil()),
                .op_true => self.push(Value.fromBool(true)),
                .op_false => self.push(Value.fromBool(false)),
                .op_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.fromBool(Value.equal(a, b)));
                },
                .op_greater, .op_less => self.binOp(bool, opcode) catch return .runtime_error,
                .op_add, .op_subtract, .op_multiply, .op_divide => self.binOp(f64, opcode) catch return .runtime_error,
                .op_not => {
                    const value = self.pop();
                    self.push(Value.fromBool(value.isFalsey()));
                },
                .op_negate => {
                    if (!self.peek(0).isNum()) {
                        self.runtimeError("Operand must be a number.");
                        return InterpretResult.runtime_error;
                    }
                    const value = self.pop();
                    self.push(Value.fromNum(-value.num));
                },
                .op_return => {
                    self.pop().print();
                    std.debug.print("\n", .{});
                    return InterpretResult.ok;
                },
            }
        }
        unreachable;
    }
};
