#include <stdio.h>

#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, char* argv[]) {
    init_vm();

    Chunk chunk;
    init_chunk(&chunk);
    load_bytecode(&chunk, "../out.byte");

    // disassemble_chunk(&chunk, "test chunk");
    interpret(&chunk);

    free_vm();
    free_chunk(&chunk);
    return 0;
}