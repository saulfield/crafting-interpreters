from dataclasses import dataclass, field
from enum import Enum, auto

import plox


class TokenKind(Enum):
    # Single-character tokens.
    LEFT_PAREN = auto()
    RIGHT_PAREN = auto()
    LEFT_BRACE = auto()
    RIGHT_BRACE = auto()
    COMMA = auto()
    DOT = auto()
    MINUS = auto()
    PLUS = auto()
    SEMICOLON = auto()
    SLASH = auto()
    STAR = auto()

    # One or two character tokens.
    BANG = auto()
    BANG_EQUAL = auto()
    EQUAL = auto()
    EQUAL_EQUAL = auto()
    GREATER = auto()
    GREATER_EQUAL = auto()
    LESS = auto()
    LESS_EQUAL = auto()

    # Literals.
    IDENTIFIER = auto()
    STRING = auto()
    NUMBER = auto()

    # Keywords.
    AND = auto()
    CLASS = auto()
    ELSE = auto()
    FALSE = auto()
    FUN = auto()
    FOR = auto()
    IF = auto()
    NIL = auto()
    OR = auto()
    PRINT = auto()
    RETURN = auto()
    SUPER = auto()
    THIS = auto()
    TRUE = auto()
    VAR = auto()
    WHILE = auto()

    # EOF
    EOF = auto()


KEYWORDS = {
    "and": TokenKind.AND,
    "class": TokenKind.CLASS,
    "else": TokenKind.ELSE,
    "false": TokenKind.FALSE,
    "for": TokenKind.FOR,
    "fun": TokenKind.FUN,
    "if": TokenKind.IF,
    "nil": TokenKind.NIL,
    "or": TokenKind.OR,
    "print": TokenKind.PRINT,
    "return": TokenKind.RETURN,
    "super": TokenKind.SUPER,
    "this": TokenKind.THIS,
    "true": TokenKind.TRUE,
    "var": TokenKind.VAR,
    "while": TokenKind.WHILE,
}


@dataclass
class Token:
    kind: TokenKind
    lexeme: str
    literal: object
    line: int

    def __repr__(self) -> str:
        s = f"{self.kind}"
        if self.lexeme:
            s += f" {self.lexeme}"
        if self.literal is not None:
            s += f" {self.literal}"
        return s


@dataclass
class Scanner:
    src: str
    start: int = 0
    current: int = 0
    line: int = 1
    tokens: list[Token] = field(default_factory=list)

    def __post_init__(self):
        self.src_len = len(self.src)

    def is_at_end(self) -> bool:
        return self.current >= self.src_len

    def peek(self) -> str:
        if self.is_at_end():
            return "\0"
        return self.src[self.current]

    def advance(self):
        self.current += 1

    def next(self) -> str:
        c = self.peek()
        self.advance()
        return c

    def match(self, expected: str) -> bool:
        if self.is_at_end():
            return False
        if self.peek() != expected:
            return False
        self.advance()
        return True

    def add_token(self, kind: TokenKind):
        self.add_token_literal(kind, None)

    def add_token_literal(self, kind: TokenKind, literal: object):
        self.tokens.append(Token(kind, "", literal, self.line))

    def scan_string(self):
        while self.peek() != '"' and not self.is_at_end():
            if self.peek() == "\n":
                self.line += 1
            self.advance()

        if self.is_at_end():
            plox.error(self.line, "Unterminated string")
            return

        self.advance()
        str_val = self.src[self.start + 1 : self.current - 1]
        self.add_token_literal(TokenKind.STRING, str_val)

    def scan_number(self):
        while self.peek().isdigit():
            self.advance()

        if self.match("."):
            if not self.peek().isdigit():
                plox.error(self.line, "Expected digit after decimal point")
            while self.peek().isdigit():
                self.advance()
        num_val = float(self.src[self.start : self.current])
        self.add_token_literal(TokenKind.NUMBER, num_val)

    def scan_identifier(self):
        while self.peek().isalnum():
            self.advance()
        str_val = self.src[self.start : self.current]
        keyword = KEYWORDS.get(str_val)
        kind = keyword if keyword else TokenKind.IDENTIFIER
        self.add_token(kind)

    def scan_token(self):
        c = self.next()
        match c:
            case " " | "\r" | "\t":
                pass
            case "\n":
                self.line += 1
            case "/":
                if self.match("/"):
                    while self.peek() != "\n" and not self.is_at_end():
                        self.advance()
                else:
                    self.add_token(TokenKind.SLASH)
            case "(":
                self.add_token(TokenKind.LEFT_PAREN)
            case ")":
                self.add_token(TokenKind.RIGHT_PAREN)
            case "{":
                self.add_token(TokenKind.LEFT_BRACE)
            case "}":
                self.add_token(TokenKind.RIGHT_BRACE)
            case ",":
                self.add_token(TokenKind.COMMA)
            case ".":
                self.add_token(TokenKind.DOT)
            case "-":
                self.add_token(TokenKind.MINUS)
            case "+":
                self.add_token(TokenKind.PLUS)
            case ";":
                self.add_token(TokenKind.SEMICOLON)
            case "*":
                self.add_token(TokenKind.STAR)
            case "!":
                self.add_token(TokenKind.BANG_EQUAL if self.match("=") else TokenKind.BANG)
            case "=":
                self.add_token(TokenKind.EQUAL_EQUAL if self.match("=") else TokenKind.EQUAL)
            case "<":
                self.add_token(TokenKind.LESS_EQUAL if self.match("=") else TokenKind.LESS)
            case ">":
                self.add_token(TokenKind.GREATER_EQUAL if self.match("=") else TokenKind.GREATER)
            case '"':
                self.scan_string()
            case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9":
                self.scan_number()
            case _:
                if c.isalpha():
                    self.scan_identifier()
                else:
                    plox.error(self.line, "Unexpected character.")

    def scan_tokens(self) -> list[Token]:
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()
        self.tokens.append(Token(TokenKind.EOF, "", None, self.line))
        return self.tokens
