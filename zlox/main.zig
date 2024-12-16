const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const MAX_SIZE = std.math.maxInt(usize);
var global_allocator: Allocator = undefined;
var had_error = false;

const TokenKind = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

const Literal = union {
    num: f64,
    str: []const u8,
};

const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    literal: ?Literal = null,
    line: u32,
};

const Scanner = struct {
    src: []const u8,
    tokens: ArrayList(Token),
    start: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,

    pub fn init(allocator: Allocator, src: []const u8) Scanner {
        return Scanner{
            .src = src,
            .tokens = ArrayList(Token).init(allocator),
        };
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.src.len;
    }

    fn advance(self: *Scanner) u8 {
        const char = self.src[self.current];
        self.current += 1;
        return char;
    }

    fn addToken(self: *Scanner, kind: TokenKind) void {
        self.addTokenLiteral(kind, null);
    }

    fn addTokenLiteral(self: *Scanner, kind: TokenKind, literal: ?Literal) void {
        self.tokens.append(Token{
            .kind = kind,
            .lexeme = self.src[self.start..self.current],
            .literal = literal,
            .line = self.line,
        }) catch {};
    }

    fn scanToken(self: *Scanner) void {
        const c = self.advance();
        switch (c) {
            '(' => self.addToken(TokenKind.LEFT_PAREN),
            ')' => self.addToken(TokenKind.RIGHT_PAREN),
            '{' => self.addToken(TokenKind.LEFT_BRACE),
            '}' => self.addToken(TokenKind.RIGHT_BRACE),
            ',' => self.addToken(TokenKind.COMMA),
            '.' => self.addToken(TokenKind.DOT),
            '-' => self.addToken(TokenKind.MINUS),
            '+' => self.addToken(TokenKind.PLUS),
            ';' => self.addToken(TokenKind.SEMICOLON),
            '*' => self.addToken(TokenKind.STAR),
            else => {},
        }
    }

    pub fn scanTokens(self: *Scanner) ArrayList(Token) {
        while (!self.isAtEnd()) {
            self.start = self.current;
            self.scanToken();
        }
        self.addToken(TokenKind.EOF);
        return self.tokens;
    }
};

fn run(allocator: Allocator, src: []const u8) !void {
    var scanner = Scanner.init(allocator, src);
    const tokens = scanner.scanTokens();
    for (tokens.items) |token| {
        try stdout.print("{any}\n", .{token});
    }
}

fn runPrompt(allocator: Allocator) !void {
    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', MAX_SIZE);
        if (line) |line_str| {
            try run(allocator, line_str);
        } else {
            try stdout.print("\n", .{});
            break;
        }
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const src = try file.readToEndAlloc(allocator, MAX_SIZE);
    try run(allocator, src);
}

pub fn main() !void {
    // Create a single allocator that will be free at the end of the program
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    global_allocator = arena.allocator();

    // Parse command line args
    const args = try std.process.argsAlloc(global_allocator);
    const options = args[1..];
    switch (options.len) {
        0 => try runPrompt(global_allocator),
        1 => try runFile(global_allocator, options[0]),
        else => {
            try stdout.print("Usage: zlox [script]\n", .{});
            return error.InvalidArgs;
        },
    }
}
