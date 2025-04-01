#ifndef lox_vm_h
#define lox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunk;
  uint8_t* ip;
  Value stack[STACK_MAX];
  Value* sp;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void init_vm(void);
void free_vm(void);
InterpretResult interpret(Chunk* chunk);
void push(Value value);
Value pop(void);

#endif