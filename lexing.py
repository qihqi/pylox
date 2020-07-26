import enum
import error

class TokenType(enum.Enum):

  (# Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  # One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  # Literals.
  IDENTIFIER, STRING, NUMBER,

  # Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  EOF) = tuple(range(39))


class Token:

    def __init__(self, type_, lexeme, literal, line):
        self.type_ = type_
        self.lexeme = lexeme
        self.literal = literal
        self.line = line

    def __str__(self):
        return ' '.join(map(str,
            (self.type_, self.lexeme, self.literal)))

    def __repr__(self):
        return str(self)


class ExprType(enum.Enum):
    BINARY, GROUPING, UNARY, LITERAL = tuple(range(4))


class Scanner:

    def __init__(self, code):
        self.source = code
        self.tokens = []
        self._start = 0
        self._current = 0
        self._line = 1

    def scan_tokens(self):
        while not self.is_at_end():
            self._start = self._current
            self.scan_token()
        self.tokens.append(Token(TokenType.EOF, '', None, self._line))
        return self.tokens

    # scan one or more tokens
    def scan_token(self):
        c = self.source[self._current]
        type_of_c = {
                '(': TokenType.LEFT_PAREN,
                ')': TokenType.RIGHT_PAREN,
                '{': TokenType.LEFT_BRACE,
                '}': TokenType.RIGHT_BRACE,
                ',': TokenType.COMMA,
                '.': TokenType.DOT,
                '-': TokenType.MINUS,
                '+': TokenType.PLUS,
                ';': TokenType.SEMICOLON,
                '*': TokenType.STAR,
        }.get(c)

        if type_of_c is not None:
            self.advance()
            self.add_token(type_of_c)
            return


        if type_of_c is None:
            if c == '!':
                self.advance()
                if self.match('='):
                    type_of_c = TokenType.BANG_EQUAL
                else:
                    type_of_c = TokenType.BANG
            if c == '=':
                self.advance()
                if self.match('='):
                    type_of_c = TokenType.EQUAL_EQUAL
                else:
                    type_of_c = TokenType.EQUAL
            if c == '<':
                self.advance()
                if self.match('='):
                    type_of_c = TokenType.LESS_EQUAL
                else:
                    type_of_c = TokenType.LESS
            if c == '>':
                self.advance()
                if self.match('='):
                    type_of_c = TokenType.GREATER_EQUAL
                else:
                    type_of_c = TokenType.GREATER
            if c == '/':
                self.advance()
                if self.match('/'):
                    while (self.source[self._current] != '\n' and
                            not self.is_at_end()):
                        self.advance()
                    return
                else:
                    type_of_c = TokenType.SLASH

        if type_of_c is not None:
            self.add_token(type_of_c)
            return

        if c in ' \r\t':
            # ignore whitespace
            self.advance()
            return

        if c == '\n':
            # ignore whitespace
            self.advance()
            self._line += 1
            return

        if c == '"':
            self.advance()
            self.string()
            return

        if c.isdigit():
            self.number()
            return

        if c.isidentifier():
            self.identifier()
            return

        error.error(self._line, 'Unpected character: ' + c)

    def identifier(self):
        c = self.source[self._current]
        while c.isidentifier() or c.isdigit():
            self.advance()
            c = self.source[self._current]
        text = self.source[self._start:self._current]
        token_type = {
            "and": TokenType.AND,
            "class": TokenType.CLASS,
            "else": TokenType.ELSE,
            "false": TokenType.FALSE,
            "for": TokenType.FOR,
            "fun": TokenType.FUN,
            "if": TokenType.IF,
            "nil": TokenType.NIL,
            "or": TokenType.OR,
            "print": TokenType.PRINT,
            "return": TokenType.RETURN,
            "super": TokenType.SUPER,
            "this": TokenType.THIS,
            "true": TokenType.TRUE,
            "var": TokenType.VAR,
            "while": TokenType.WHILE,
            }.get(text, TokenType.IDENTIFIER)
        value = {
            "true": True,
            "nil": None,
            "false": False,
        }.get(text)
        self.add_token(token_type, value)

    def string(self):
        while self.source[self._current] != '"' and not self.is_at_end():
            c = self.source[self._current]
            if c == '\n':
                self.line += 1
            self.advance()

        if self.is_at_end():
            error.error(self._line, 'unterminated string')
            return
        # consume closing "
        self.advance()

        value = self.source[self._start+1:self._current-1]
        self.add_token(TokenType.STRING, value)

    def number(self):
        c = self.source[self._current]
        while c.isdigit():
            self.advance()
            c = self.source[self._current]

        c = self.source[self._current]
        if c == '.' and (
            self._current + 1 < len(self.source) and
            self.source[self._current + 1].isdigit()):
            c = self.advance()
            while c.isdigit():
                c = self.advance()
        self.add_token(TokenType.NUMBER,
                float(self.source[self._start:self._current]))

    def match(self, expected):
        if self.is_at_end():
            return False
        if self.source[self._current] != expected:
            return False
        self._current += 1
        return True

    def advance(self):
        self._current += 1
        return self.source[self._current-1]

    def add_token(self, type_, literal=None):
        text = self.source[self._start:self._current].strip()
        self.tokens.append(Token(type_, text, literal, self._line))

    def is_at_end(self):
        return self._current >= len(self.source)



