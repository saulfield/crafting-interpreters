const std = @import("std");
const Allocator = std.mem.Allocator;

const ozlox = @import("ozlox.zig");
const GC = ozlox.GC;
const Chunk = ozlox.Chunk;
const Value = ozlox.Value;
const Opcode = ozlox.Opcode;
const FunctionObj = ozlox.FunctionObj;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const CallFrame = struct {
    function: *FunctionObj,
    ip: usize,
    slots: []Value,
};

const FRAMES_MAX = 64;
const STACK_MAX = 256 * FRAMES_MAX;
pub const VM = struct {
    gc: *GC,
    sp: usize,
    stack: [STACK_MAX]Value,
    fp: usize,
    frames: [FRAMES_MAX]CallFrame,
    frame: *CallFrame,
    chunk: *Chunk,
    globals: std.StringHashMap(Value),

    pub fn init(allocator: Allocator, gc: *GC) VM {
        return .{
            .gc = gc,
            .sp = 0,
            .stack = undefined,
            .fp = 0,
            .frames = undefined,
            .frame = undefined,
            .chunk = undefined,
            .globals = std.StringHashMap(Value).init(allocator),
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
        const byte = self.frame.function.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) u16 {
        const upper = @as(u16, self.frame.function.chunk.code.items[self.frame.ip]);
        const lower = @as(u16, self.frame.function.chunk.code.items[self.frame.ip + 1]);
        self.frame.ip += 2;
        return (upper << 8) | lower;
    }

    fn readConst(self: *VM) Value {
        const index = self.readByte();
        return self.frame.function.chunk.constants.items[index];
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

    fn callValue(self: *VM, callee: Value, argCount: u8) bool {
        switch (callee) {
            .obj => |obj| {
                switch (obj) {
                    .func => |functionObj| {
                        if (argCount != functionObj.*.arity) {
                            self.runtimeError("Unexpected number of arguments.");
                            return false;
                        }

                        if (self.fp == FRAMES_MAX) {
                            self.runtimeError("Stack overflow.");
                            return false;
                        }

                        self.frame = &self.frames[self.fp];
                        self.frame.* = .{
                            .function = functionObj,
                            .ip = 0,
                            .slots = self.stack[self.sp - argCount ..],
                        };
                        self.fp += 1;
                        return true;
                    },
                    else => {},
                }
            },
            else => {},
        }
        self.runtimeError("Can only call functions and classes.");
        return false;
    }

    fn printStack(self: *VM) void {
        for (self.stack[0..self.sp]) |val| {
            std.debug.print("[", .{});
            val.print();
            std.debug.print("]", .{});
        }
        // std.debug.print("\n", .{});
    }

    fn runtimeError(self: *VM, message: []const u8) void {
        std.debug.print("{s}\n", .{message});
        self.sp = 0; // reset stack
    }

    pub fn run(self: *VM, functionObj: *FunctionObj) !InterpretResult {
        const scriptFunction = Value.fromObj(.{ .func = functionObj });
        self.push(scriptFunction);
        _ = self.callValue(scriptFunction, 0);
        while (true) {
            const opcode: Opcode = @enumFromInt(self.readByte());

            // Tracing
            // const func = Value.fromObj(.{ .func = self.frame.function });
            // func.print();
            // std.debug.print(" ", .{});
            // opcode.print();
            // std.debug.print(" ", .{});
            // self.printStack();
            // std.debug.print("\n", .{});

            switch (opcode) {
                .op_constant => self.push(self.readConst()),
                .op_nil => self.push(Value.fromNil()),
                .op_true => self.push(Value.fromBool(true)),
                .op_false => self.push(Value.fromBool(false)),
                .op_pop => _ = self.pop(),
                .op_get_local => {
                    const slot = self.readByte();
                    self.push(self.frame.slots[slot]);
                },
                .op_set_local => {
                    const slot = self.readByte();
                    self.frame.slots[slot] = self.peek(0);
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
                    self.frame.ip += offset;
                },
                .op_jump_if_false => {
                    const offset = self.readShort();
                    if (self.peek(0).isFalsey())
                        self.frame.ip += offset;
                },
                .op_loop => {
                    const offset = self.readShort();
                    self.frame.ip -= offset;
                },
                .op_call => {
                    const argCount = self.readByte();
                    const callee = self.peek(argCount);
                    if (!self.callValue(callee, argCount))
                        return .runtime_error;
                },
                .op_return => {
                    const result = self.pop();
                    self.fp -= 1;
                    if (self.fp == 0) {
                        // _ = self.pop();
                        return .ok;
                    }

                    self.sp -= self.frame.function.arity + 1;
                    self.push(result);
                    self.frame = &self.frames[self.fp - 1];
                },
                else => unreachable,
            }
        }
        unreachable;
    }
};
