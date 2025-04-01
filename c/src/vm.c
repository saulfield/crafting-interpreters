#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "vm.h"

VM vm;

static void reset_stack() {
    vm.sp = vm.stack;
}

void init_vm(void) {
    reset_stack();
}

void free_vm(void) {

}

void push(Value value) {
    *vm.sp++ = value;
}

Value pop(void) {
    vm.sp--;
    return *vm.sp;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op) \
    do { \
      double b = pop(); \
      double a = pop(); \
      push(a op b); \
    } while (false)

    for (;;) {

#ifdef DEBUG_TRACE_EXECUTION
        if (vm.sp != vm.stack) {
            printf("          ");
            for (Value* slot = vm.stack; slot < vm.sp; slot++) {
                printf("[ ");
                print_value(*slot);
                printf(" ]");
            }
            printf("\n");
        }
        disassemble_instruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                // print_value(constant);
                // printf("\n");
                break;
            }
            case OP_ADD:      BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE:   BINARY_OP(/); break;
            case OP_NEGATE: push(-pop()); break;
            case OP_RETURN: {
                print_value(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }
#undef BINARY_OP
#undef READ_CONSTANT
#undef READ_BYTE
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    vm.ip = vm.chunk->code;
    return run();
}