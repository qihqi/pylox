import enum
import sys

from lexing import TokenType, Token, Scanner
from parsing import Expr, Parser
import error
import interpreter

interp = interpreter.Interpreter()



def run(code):
    scanner = Scanner(code)
    tokens = scanner.scan_tokens()
    if error.had_error:
        return
    parser = Parser(tokens)
    stmts = parser.parse()
    if error.had_error:
        return
    interp.interpret(stmts)


def run_file(path):
    with open(path) as f:
        run(f.read())
        if error.had_error:
            sys.exit(65)


def run_prompt():
    print('> ', end='', flush=True)

    while True:
        line = sys.stdin.readline()
        if not line:
            break
        run(line)
        error.had_error = False
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
