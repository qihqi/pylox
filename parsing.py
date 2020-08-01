import enum
import error
from lexing import Token, TokenType

class ParseError(Exception):
    pass


class ExprType(enum.Enum):
    LITERAL = 0,
    UNARY = 1
    BINARY = 2
    GROUPING = 3
    VARIABLE = 4
    ASSIGN = 5
    LOGICAL= 6
    CALL = 7


class Expr:

    """
      "Binary   : Expr left, Token operator, Expr right",
      "Grouping : Expr expression",
      "Literal  : Object value",
      "Unary    : Token operator, Expr right"
      "Assign   : Token operator(assignment), Token lhs, Expr rhs
      "CAll : Token operator(paren), Expr func, Expr args ...
    """

    def __init__(self, op, operands):
        self.op = op
        self.operands = operands

    @property
    def expr_type(self):
        if self.op and self.op.type_ == TokenType.RIGHT_PAREN:
            return ExprType.CALL
        if len(self.operands) == 2:
            if self.op and self.op.type_ == TokenType.EQUAL:
                return ExprType.ASSIGN
            if self.op and self.op.type_ in (TokenType.AND, TokenType.OR):
                return ExprType.LOGICAL
            return ExprType.BINARY
        if len(self.operands) == 1:
            if self.op is not None:
                return ExprType.UNARY
            else:
                return ExprType.GROUPING
        if len(self.operands) == 0:
            if self.op.type_ == TokenType.IDENTIFIER:
                return ExprType.VARIABLE
            return ExprType.LITERAL
        raise AssertionError('invalid', self.op, self.operands)

    def __str__(self):
        if self.expr_type == ExprType.LITERAL:
            return str(self.op.literal)
        opstr = 'group' if self.op is None else str(self.op.type_)
        return '({} {})'.format(
                opstr,
                ' '.join(map(str, self.operands)))

    @classmethod
    def binary(cls, left, op, right):
        return cls(op, [left, right])

    @classmethod
    def literal(cls, token):
        return cls(token, [])

    @classmethod
    def unary(cls, op, arg):
        return cls(op, [arg])

    @classmethod
    def assign(cls, token_assign, token_var, expr):
        return cls(token_assign, [token_var, expr])


class StmtType(enum.Enum):
    PRINT = 0
    EXPRESSION = 1
    VARIABLE = 2
    BLOCK = 3
    IF = 4
    WHILE = 5
    FUNCTION = 6
    RETURN = 7


class Stmt(object):

    def __init__(self, expr, type_, name=None):
        self.expr = expr
        self._type = type_
        self.name = name

    @property
    def type_(self):
        return self._type


class Block(Stmt):

    def __init__(self, statements):
        self.statements = statements

    @property
    def type_(self):
        return StmtType.BLOCK

class IfStmt(Stmt):

    def __init__(self, condition, then_br, else_br):
        self.condition = condition
        self.then_br = then_br
        self.else_br = else_br

    @property
    def type_(self):
        return StmtType.IF

class WhileStmt(Stmt):

    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    @property
    def type_(self):
        return StmtType.WHILE


class FunctionStmt(Stmt):

    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

    @property
    def type_(self):
        return StmtType.FUNCTION


class ReturnStmt(Stmt):

    def __init__(self, keyword, expr):
        self.keyword = keyword
        self.expr = expr

    @property
    def type_(self):
        return StmtType.RETURN


class Parser:

    def __init__(self, tokens):
        self.tokens = tokens
        self._current = 0

    def parse(self):
        statements = []
        while not self.at_end():
            statements.append(self.declaration())
        return statements

    def declaration(self):
        "declaration -> var decl | statement"
        try:
            if self.match(TokenType.FUN):
                return self.function('function')
            if self.match(TokenType.VAR):
                return self.var_decl()
            return self.statement()
        except ParseError:
            self.synchronize()
            return None

    def function(self, kind):
        name = self.consume(TokenType.IDENTIFIER,
                'Expect {} name.'.format(kind))
        self.consume(TokenType.LEFT_PAREN, '')
        parameters = []
        if not self.check(TokenType.RIGHT_PAREN):
            while True:
                if len(parameters) > 256:
                    error.error(self.peek(),
                            'cannot have more than255 params.')
                parameters.append(self.consume(TokenType.IDENTIFIER,
                    'expect parameter name'))
                if not self.match(TokenType.COMMA):
                    break
        self.consume(TokenType.RIGHT_PAREN)
        self.consume(TokenType.LEFT_BRACE,
                     'Expect {{ before {} body'.format(kind))
        body = self.block()
        return FunctionStmt(name, parameters, body)

    def var_decl(self):
        name = self.consume(TokenType.IDENTIFIER,
                'Expect variable name.')
        initializer = None
        if self.match(TokenType.EQUAL):
            initializer = self.expression()

        self.consume(TokenType.SEMICOLON, 'Expect ;')
        return Stmt(initializer, StmtType.VARIABLE, name)

    def statement(self):
        if self.match(TokenType.RETURN):
            return self.return_statement()
        if self.match(TokenType.FOR):
            return self.for_statement()
        if self.match(TokenType.IF):
            return self.if_statement()
        if self.match(TokenType.WHILE):
            return self.while_statement()
        if self.match(TokenType.PRINT):
            return self.print_statement()
        if self.match(TokenType.LEFT_BRACE):
            return Block(self.block())
        return self.expression_statement();

    def return_statement(self):
        keyword = self.previous()
        expr = None
        if not self.check(TokenType.SEMICOLON):
            expr = self.expression()
        self.consume(TokenType.SEMICOLON, 'Expect ; after return')
        return ReturnStmt(keyword, expr)

    def for_statement(self):
        self.consume(TokenType.LEFT_PAREN, 'Expect "(" after "while"')
        initializer = None
        if self.match(TokenType.SEMICOLON):
            pass
        elif self.match(TokenType.VAR):
            initializer = self.var_decl()
        else:
            initializer = self.expression_statement()

        condition = None
        if not self.check(TokenType.SEMICOLON):
            condition = self.expression()
        self.consume(TokenType.SEMICOLON, 'Expect ; after loop cond')

        increment = None
        if not self.check(TokenType.RIGHT_PAREN):
            increment = Stmt(self.expression(), StmtType.EXPRESSION)

        self.consume(TokenType.RIGHT_PAREN, 'Expect ")" after if condition')

        body = self.statement()
        if increment is not None:
            body = Block([body, increment])
        if condition is None:
            condition = Expr.literal(
                    Token(TokenType.TRUE, 'true', True, 0))
        body = WhileStmt(condition, body)
        if initializer is not None:
            body = Block([initializer, body])
        return body

    def if_statement(self):
        self.consume(TokenType.LEFT_PAREN, 'Expect "(" after "if"')
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN, 'Expect ")" after if condition')
        then_br = self.statement()
        else_br = None
        if self.match(TokenType.ELSE):
            else_br = self.statement()
        return IfStmt(condition, then_br, else_br)

    def while_statement(self):
        self.consume(TokenType.LEFT_PAREN, 'Expect "(" after "while"')
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN,
                     'Expect ")" after while condition')
        body = self.statement()
        return WhileStmt(condition, body)

    def print_statement(self):
        value = self.expression()
        self.consume(TokenType.SEMICOLON, 'Exprect ; after value')
        return Stmt(value, StmtType.PRINT)

    def block(self):
        statements = []
        while (not self.check(TokenType.RIGHT_BRACE) and
               not self.at_end()):
            statements.append(self.declaration())
        self.consume(TokenType.RIGHT_BRACE, 'Expect } after block.')
        return statements

    def expression_statement(self):
        value = self.expression()
        self.consume(TokenType.SEMICOLON, 'Exprect ; after value')
        return Stmt(value, StmtType.EXPRESSION)

    def match(self, *args):
        for a in args:
            if self.check(a):
                self.advance()
                return True

        return False

    def check(self, t):
        if self.at_end():
            return False
        return self.peek().type_ == t

    def advance(self):
        if not self.at_end():
            self._current += 1
        return self.previous()

    def previous(self):
        return self.tokens[self._current - 1]

    def peek(self):
        return self.tokens[self._current]

    def at_end(self):
        return self.peek().type_ == TokenType.EOF

    def consume(self, token_type, message=''):
        if self.check(token_type):
            return self.advance()
        error.error(self.peek().line, message)
        raise ParseError()

    def synchronize(self):
        self.advance()
        while not self.at_end:
            if self.previous().type_ == TokenType.SEMICOLON:
                return

            if self.peek().type_ in {
                TokenType.CLASS,
                TokenType.FUN,
                TokenType.VAR,
                TokenType.FOR,
                TokenType.IF,
                TokenType.WHILE,
                TokenType.PRINT,
                TokenType.RETURN,
                }:
                return
            self.advance()

    def expression(self):
        return self.assignment()

    def assignment(self):
        expr = self.or_expr()
        if self.match(TokenType.EQUAL):
            equals = self.previous()
            value = self.or_expr()

            if expr.expr_type == ExprType.VARIABLE:
                return Expr.assign(equals, expr.op, value)

            error.error(equals.line, 'Invalid assignment target.')

        return expr

    def or_expr(self):
        expr = self.and_expr()
        while self.match(TokenType.OR):
            op = self.previous()
            right = self.and_expr()
            expr = Expr.binary(expr, op, right)
        return expr

    def and_expr(self):
        expr = self.equality()
        while self.match(TokenType.AND):
            op = self.previous()
            right = self.equality()
            expr = Expr.binary(expr, op, right)
        return expr

    def equality(self):
        expr = self.comparison()
        while self.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL):
            operator = self.previous()
            right = comparison()
            expr = Expr.binary(expr, operator, right)
        return expr

    def comparison(self):
        expr = self.addition()
        while self.match(
                TokenType.GREATER,
                TokenType.GREATER_EQUAL,
                TokenType.LESS,
                TokenType.LESS_EQUAL):
            operator = self.previous()
            right = self.addition()
            expr = Expr.binary(expr, operator, right)
        return expr

    def addition(self):
        expr = self.multiplication()
        while self.match(TokenType.MINUS, TokenType.PLUS):
            op = self.previous()
            right = self.multiplication()
            expr = Expr.binary(expr, op, right)
        return expr

    def multiplication(self):
        expr = self.unary()
        while self.match(TokenType.SLASH, TokenType.STAR):
            op = self.previous()
            right = self.unary()
            expr = Expr.binary(expr, op, right)
        return expr

    def unary(self):
        if self.match(TokenType.BANG, TokenType.MINUS):
            operator = self.previous()
            right = self.unary()
            return Expr.unary(operator, right)
        return self.call()

    def call(self):
        expr = self.primary()
        while True:
            if self.match(TokenType.LEFT_PAREN):
                expr = self.finish_call(expr)
            else:
                break
        return expr

    def finish_call(self, callee):
        args = [callee]
        if not self.check(TokenType.RIGHT_PAREN):
            args.append(self.expression())
            while self.match(TokenType.COMMA):
                if len(args) >= 256:
                    error.error(self.peek(),
                        'Cannot have more than 255 args')
                args.append(self.expression())
        paren = self.consume(TokenType.RIGHT_PAREN,
            'Expect ) after arguments')
        return Expr(paren, args)

    def primary(self):
        if self.match(TokenType.FALSE):
            return Expr.literal(self.previous())
        if self.match(TokenType.TRUE):
            return Expr.literal(self.previous())
        if self.match(TokenType.NIL):
            return Expr.literal(self.previous())
        if self.match(TokenType.NUMBER, TokenType.STRING):
            return Expr.literal(self.previous())
        if self.match(TokenType.IDENTIFIER):
            return Expr.literal(self.previous())
        if self.match(TokenType.LEFT_PAREN):
            expr = self.expression()
            self.consume(TokenType.RIGHT_PAREN, 'Expect ) after expression')
            return expr
        error.error(self.peek().line, "Expect expression")
        raise ParseError()

