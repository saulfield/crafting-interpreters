const std = @import("std");
const Allocator = std.mem.Allocator;

const ozlox = @import("ozlox.zig");
const GC = ozlox.GC;
const Chunk = ozlox.Chunk;
const Value = ozlox.Value;
const Opcode = ozlox.Opcode;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const STACK_MAX = 256;
pub const VM = struct {
    gc: *GC,
    sp: usize,
    ip: usize,
    stack: [STACK_MAX]Value,
    globals: std.StringHashMap(Value),
    chunk: *Chunk,

    pub fn init(allocator: Allocator, gc: *GC) VM {
        return .{
            .gc = gc,
            .sp = 0,
            .ip = 0,
            .stack = undefined,
            .globals = std.StringHashMap(Value).init(allocator),
            .chunk = undefined,
        };
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
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

    fn readShort(self: *VM) u16 {
        const upper = @as(u16, self.chunk.*.code.items[self.ip]);
        const lower = @as(u16, self.chunk.*.code.items[self.ip + 1]);
        self.ip += 2;
        return (upper << 8) | lower;
    }

    fn readConst(self: *VM) Value {
        const index = self.readByte();
        return self.chunk.*.constants.items[index];
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

    pub fn interpret(self: *VM, chunk: *Chunk) !InterpretResult {
        self.ip = 0;
        self.chunk = chunk;
        while (true) {
            const opcode: Opcode = @enumFromInt(self.readByte());
            switch (opcode) {
                .op_constant => self.push(self.readConst()),
                .op_nil => self.push(Value.fromNil()),
                .op_true => self.push(Value.fromBool(true)),
                .op_false => self.push(Value.fromBool(false)),
                .op_pop => _ = self.pop(),
                .op_get_local => {
                    const slot = self.readByte();
                    self.push(self.stack[slot]);
                },
                .op_set_local => {
                    const slot = self.readByte();
                    self.stack[slot] = self.peek(0);
                },
                .op_get_global => {
                    const name = self.readConst().obj.str;
                    if (self.globals.get(name)) |value| {
                        self.push(value);
                    } else {
                        // TODO: print name in error message
                        self.runtimeError("Undefined variable.");
                        return .runtime_error;
                    }
                },
                .op_set_global => {
                    const name = self.readConst().obj.str;
                    if (self.globals.get(name) == null) {
                        // TODO: print name in error message
                        self.runtimeError("Undefined variable.");
                        return .runtime_error;
                    }
                    try self.globals.put(name, self.peek(0));
                },
                .op_define_global => {
                    const name = self.readConst().obj.str;
                    try self.globals.put(name, self.peek(0));
                    _ = self.pop();
                },
                .op_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.fromBool(Value.equal(a, b)));
                },
                .op_greater, .op_less => self.binOp(bool, opcode) catch return .runtime_error,
                .op_subtract, .op_multiply, .op_divide => self.binOp(f64, opcode) catch return .runtime_error,
                .op_add => {
                    if (self.peek(0).isNum() and self.peek(1).isNum()) {
                        const b = self.pop().num;
                        const a = self.pop().num;
                        self.push(Value.fromNum(a + b));
                    } else if (self.peek(0).isStr() and self.peek(1).isStr()) {
                        const b = self.pop().obj.str;
                        const a = self.pop().obj.str;
                        const newStr = try self.gc.allocString(a.len + b.len);
                        std.mem.copyForwards(u8, newStr, a);
                        std.mem.copyForwards(u8, newStr[a.len..], b);
                        const strObj = try self.gc.createStrObject(newStr);
                        self.push(Value.fromObj(strObj));
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.");
                        return .runtime_error;
                    }
                },
                .op_not => {
                    const value = self.pop();
                    self.push(Value.fromBool(value.isFalsey()));
                },
                .op_negate => {
                    if (!self.peek(0).isNum()) {
                        self.runtimeError("Operand must be a number.");
                        return .runtime_error;
                    }
                    const value = self.pop();
                    self.push(Value.fromNum(-value.num));
                },
                .op_print => {
                    self.pop().print();
                    std.debug.print("\n", .{});
                },
                .op_jump => {
                    const offset = self.readShort();
                    self.ip += offset;
                },
                .op_jump_if_false => {
                    const offset = self.readShort();
                    if (self.peek(0).isFalsey())
                        self.ip += offset;
                },
                .op_loop => {
                    const offset = self.readShort();
                    self.ip -= offset;
                },
                .op_return => {
                    return .ok;
                },
                else => unreachable,
            }
        }
        unreachable;
    }
};
