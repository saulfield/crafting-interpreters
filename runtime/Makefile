# Compiler and flags
CC = clang
CFLAGS = -std=c99 -Wall -Wextra -Werror -Wno-unused-parameter -Wformat -Wformat=2 -Wstrict-prototypes

# Directories
SRC_DIR = src
BUILD_DIR = build

# Find all source files
SRCS = $(wildcard $(SRC_DIR)/*.c)
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)
DEPS = $(OBJS:.o=.d)

# Default target
TARGET = $(BUILD_DIR)/main

.PHONY: all clean debug release

# Default build is release
all: release

# Debug build
debug: CFLAGS += -g -O0 -DDEBUG
debug: $(TARGET)

# Release build
release: CFLAGS += -O3 -DNDEBUG
release: $(TARGET)

# Create necessary directories
$(BUILD_DIR):
	@ mkdir -p $@

# Compile source files into object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Link object files into executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

# Clean build files
clean:
	rm -rf $(BUILD_DIR)

# Include dependency files
-include $(DEPS)