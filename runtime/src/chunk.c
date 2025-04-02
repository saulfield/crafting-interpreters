#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

void init_chunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    init_value_array(&chunk->constants);
}

void free_chunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    free_value_array(&chunk->constants);
    init_chunk(chunk);
}

void write_chunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->count + 1 > chunk->capacity) {
        int old_capacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(old_capacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, old_capacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, old_capacity, chunk->capacity);
    }
    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

int add_constant(Chunk* chunk, Value value) {
    write_value_array(&chunk->constants, value);
    return chunk->constants.count - 1;
}

static char* read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    char* buffer = (char*)malloc(file_size + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }
    size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
    if (bytes_read < file_size) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }
    buffer[bytes_read] = '\0';

    fclose(file);
    return buffer;
}

static void scan_keyword(char **src, const char* expected) {
    const char* p = expected + 1;
    while (*p) {
        if (*p != **src) {
            printf("Expected keyword: %s\n", expected);
            exit(1);
        }
        p++;
        (*src)++;
    }
}

static double scan_number(char **src) {
    const char* start = *src;
    while (isdigit(**src)) (*src)++;
    while (**src != '\n') (*src)++;
    return strtod(start, NULL);
}

void load_bytecode(Chunk* chunk, const char* path) {
#define EMIT_OP(keyword, opcode) { \
    scan_keyword(&src, keyword); \
    write_chunk(chunk, opcode, 1); \
    break; }

    char* buffer = read_file(path);
    char* src = buffer;
    for (;;) {
        char c = *src++;
        if (c == '\0') break;
        switch (c) {
            case '\n': break;
            case 'C': {
                scan_keyword(&src, "CONST ");
                double num = scan_number(&src);
                int const_index = add_constant(chunk, num);
                write_chunk(chunk, OP_CONSTANT, 1);
                write_chunk(chunk, const_index, 1);
                break;
            }
            case 'A': EMIT_OP("ADD", OP_ADD)
            case 'D': EMIT_OP("DIV", OP_DIVIDE)
            case 'N': EMIT_OP("NEG", OP_NEGATE)
            case 'R': EMIT_OP("RET", OP_RETURN)
            default:
                printf("Unexpected character: %c\n", c);
                exit(1);
        }
    }
    free(buffer);
#undef EMIT_OP
}