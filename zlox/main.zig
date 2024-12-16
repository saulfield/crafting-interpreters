const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const MAX_SIZE = std.math.maxInt(usize);
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

const KEYWORDS = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "and", TokenKind.AND },
    .{ "class", TokenKind.CLASS },
    .{ "else", TokenKind.ELSE },
    .{ "false", TokenKind.FALSE },
    .{ "for", TokenKind.FOR },
    .{ "fun", TokenKind.FUN },
    .{ "if", TokenKind.IF },
    .{ "nil", TokenKind.NIL },
    .{ "or", TokenKind.OR },
    .{ "print", TokenKind.PRINT },
    .{ "return", TokenKind.RETURN },
    .{ "super", TokenKind.SUPER },
    .{ "this", TokenKind.THIS },
    .{ "true", TokenKind.TRUE },
    .{ "var", TokenKind.VAR },
    .{ "while", TokenKind.WHILE },
});

const Literal = union(enum) {
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

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.src.len;
    }

    fn advance(self: *Scanner) void {
        self.current += 1;
    }

    fn next(self: *Scanner) u8 {
        const char = self.src[self.current];
        self.advance();
        return char;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.src[self.current];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.src[self.current] != expected) return false;
        self.advance();
        return true;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return switch (c) {
            'a'...'z', 'A'...'Z', '_' => true,
            else => false,
        };
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn scanString(self: *Scanner) void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            self.advance();
        }
        if (self.isAtEnd()) {
            reportError(self.line, "Unterminated string.");
            return;
        }
        self.advance();
        const str = self.src[self.start + 1 .. self.current - 1];
        self.addTokenLiteral(TokenKind.STRING, Literal{ .str = str });
    }

    fn scanNumber(self: *Scanner) void {
        while (isDigit(self.peek()))
            self.advance();

        if (self.match('.')) {
            if (!isDigit(self.peek()))
                reportError(self.line, "Expected digit after decimal point.");
            while (isDigit(self.peek()))
                self.advance();
        }

        const str = self.src[self.start..self.current];
        const num = std.fmt.parseFloat(f64, str) catch {
            reportError(self.line, "Failed to parse number.");
            return;
        };
        self.addTokenLiteral(TokenKind.NUMBER, Literal{ .num = num });
    }

    fn scanIdentifier(self: *Scanner) void {
        while (isAlphaNumeric(self.peek()))
            self.advance();

        const str = self.src[self.start..self.current];
        self.addToken(KEYWORDS.get(str) orelse TokenKind.IDENTIFIER);
    }

    fn scanToken(self: *Scanner) void {
        const c = self.next();
        switch (c) {
            ' ', '\r', '\t' => return,
            '\n' => self.line += 1,
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd())
                        self.advance();
                } else {
                    self.addToken(TokenKind.SLASH);
                }
            },
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
            '!' => self.addToken(if (self.match('=')) TokenKind.BANG_EQUAL else TokenKind.BANG),
            '=' => self.addToken(if (self.match('=')) TokenKind.EQUAL_EQUAL else TokenKind.EQUAL),
            '<' => self.addToken(if (self.match('=')) TokenKind.LESS_EQUAL else TokenKind.LESS),
            '>' => self.addToken(if (self.match('=')) TokenKind.GREATER_EQUAL else TokenKind.GREATER),
            '"' => self.scanString(),
            '0'...'9' => self.scanNumber(),
            'a'...'z', 'A'...'Z', '_' => self.scanIdentifier(),
            else => reportError(self.line, "Unexpected character."),
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

fn reportError(line: u32, msg: []const u8) void {
    report(line, "", msg);
}

fn report(line: u32, where: []const u8, msg: []const u8) void {
    std.debug.print("[line {any}] Error{s}: {s}\n", .{ line, where, msg });
    had_error = true;
}

fn run(allocator: Allocator, src: []const u8) !void {
    var scanner = Scanner.init(allocator, src);
    const tokens = scanner.scanTokens();
    for (tokens.items) |token| {
        try stdout.print("{any}", .{token.kind});
        if (token.literal) |val| {
            switch (val) {
                .str => try stdout.print(" \"{s}\"", .{val.str}),
                .num => try stdout.print(" {d}", .{val.num}),
            }
        }
        try stdout.print("\n", .{});
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
    // Create a single allocator that will be freed at the end of the program
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse command line args
    const args = try std.process.argsAlloc(allocator);
    const options = args[1..];
    switch (options.len) {
        0 => try runPrompt(allocator),
        1 => try runFile(allocator, options[0]),
        else => {
            try stdout.print("Usage: zlox [script]\n", .{});
            return error.InvalidArgs;
        },
    }
}
