import enum
import sys

had_error = False


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

    def scan_token(self):
        c = self.advance()
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


        if type_of_c is None:
            if c == '!':
                if self.match('='):
                    type_of_c = TokenType.BANG_EQUAL
                else:
                    type_of_c = TokenType.BANG
            if c == '=':
                if self.match('='):
                    type_of_c = TokenType.EQUAL_EQUAL
                else:
                    type_of_c = TokenType.EQUAL
            if c == '<':
                if self.match('='):
                    type_of_c = TokenType.LESS_EQUAL
                else:
                    type_of_c = TokenType.LESS
            if c == '>':
                if self.match('='):
                    type_of_c = TokenType.GREATER_EQUAL
                else:
                    type_of_c = TokenType.GREATER
            if c == '/':
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
            return

        if c == '\n':
            # ignore whitespace
            self._line += 1
            return

        if c == '"':
            self.string()
            return

        if c.isdigit():
            self.number()
            return

        if c.isidentifier():
            self.identifier()
            return

        error(self._line, 'Unpected character: ' + c)

    def identifier(self):
        c = self.source[self._current]
        while c.isidentifier() or c.isdigit():
            c = self.advance()
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
        self.add_token(token_type)


    def string(self):
        while self.source[self._current] != '"' and not self.is_at_end():
            c = self.source[self._current]
            if c == '\n':
                self.line += 1
            self.advance()

        if self.is_at_end():
            error(self._line, 'unterminated string')
            return
        # consume closing "
        self.advance()

        value = self.source[self._start+1:self._current-1]
        self.add_token(TokenType.STRING, value)

    def number(self):
        c = self.source[self._current]
        while c.isdigit():
            c = self.advance()

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
        text = self.source[self._start:self._current]
        self.tokens.append(Token(type_, text, literal, self._line))

    def is_at_end(self):
        return self._current >= len(self.source)



def error(line, message):
    report(line, '', message)


def report(line, where,  message):
    print('[line {}] Error {}: {}'.format(line, where, message),
          file=sys.stderr)
    global had_error
    had_error = True


def run(code):
    scanner = Scanner(code)
    for c in scanner.scan_tokens():
        print(c)


def run_file(path):
    with open(path) as f:
        run(f.read())
        if had_error:
            sys.exit(65)


def run_prompt():
    global had_error
    print('> ', end='', flush=True)

    while True:
        line = sys.stdin.readline()
        if not line:
            break
        run(line)
        had_error = False
        print('> ', end='', flush=True)


def main():
    if len(sys.argv) > 2:
        print('Usage: pylox [script]')
        sys.exit(64)
    elif len(sys.argv) == 2:
        run_file(sys.argv[1])
    else:
        run_prompt()



if __name__ == '__main__':
    main()
