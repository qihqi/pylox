import time
from lexing import TokenType
from parsing import Expr, ExprType, StmtType


class InterpreterError(Exception):
    pass

def _plus(vals):
    return vals[0] + vals[1]

def _minus(vals):
    return vals[0] - vals[1]

def _times(vals):
    return vals[0] * vals[1]

def _divides(vals):
    return vals[0] / vals[1]

def _neg(vals):
    return -vals[0]

def _not(vals):
    return not bool(vals[0])

def _id(vals):
    return vals[0]

def _lit(vals):
    return

def _greater(vals):
    return vals[0] > vals[1]

def _greater_equal(vals):
    return vals[0] >= vals[1]

def _less(vals):
    return vals[0] < vals[1]

def _less_equal(vals):
    return vals[0] <= vals[1]

def _bang_equal(vals):
    return vals[0] != vals[1]

def _equal_equal(vals):
    return vals[0] == vals[1]


def get_func(expr):
    if expr.expr_type == ExprType.UNARY:
        return {
                TokenType.BANG: _not,
                TokenType.MINUS: _neg
            }[expr.op.type_]
    if expr.expr_type == ExprType.BINARY:
        return {
            TokenType.MINUS: _minus,
            TokenType.PLUS: _plus,
            TokenType.STAR: _times,
            TokenType.SLASH: _divides,
            TokenType.GREATER: _greater,
            TokenType.LESS: _less,
            TokenType.GREATER_EQUAL: _greater_equal,
            TokenType.LESS_EQUAL: _less_equal,
            TokenType.BANG_EQUAL: _bang_equal,
            TokenType.EQUAL_EQUAL: _equal_equal,
        }[expr.op.type_]
    if expr.expr_type == ExprType.GROUPING:
        return _id

# foreign function that tells time
class Clock(object):

    def arity(self):
        return 0

    def __call__(self, interpreter, args):
        return time.time()

    def __str__(self):
        return '<native fn>'

class Return(Exception):

    def __init__(self, value):
        self.value = value

class LoxFunction(object):

    def __init__(self, decl, closure):
        self.decl = decl
        self.closure = closure

    def __call__(self, interpreter, args):
        env = Environment(self.closure)
        for name, arg in zip(self.decl.params, args):
            env.define(name.lexeme, arg)
        try:
            interpreter.exec_block(self.decl.body, env)
        except Return as r:
            return r.value

    def arity(self):
        return len(self.decl.params)

    def __str__(self):
        return '<fn {} >'.format(self.decl.name.lexeme)


class Interpreter(object):

    def __init__(self):
        self._globals = Environment()
        self._env = self._globals
        self._env.define('clock', Clock())
        self._locals = {}

    def interpret_expr(self, expr):
        if expr.expr_type == ExprType.CALL:
            operands = list(map(self.interpret_expr, expr.operands))
            func = operands[0]
            args = operands[1:]
            if func.arity() != len(args):
                raise InterpreterError(
                        'Expected {} args, got {}'.format(
                            func.arity(), args))
            return func(self, args)
        if expr.expr_type == ExprType.LITERAL:
            return expr.op.literal
        if expr.expr_type == ExprType.VARIABLE:
            dist = self._locals.get(expr)
            if dist is not None:
                return self._env.get_at(dist, expr.op)
            return self._globals.get(expr.op)
        if expr.expr_type == ExprType.ASSIGN:
            tok = expr.operands[0]
            val = self.interpret_expr(expr.operands[1])
            return self._env.assign(tok, val)
        if expr.expr_type == ExprType.LOGICAL:
            left = self.interpret_expr(expr.operands[0])
            if expr.op.type_ == TokenType.OR:
                if left:
                    return left
            else:
                if not left:
                    return left
            value = self.interpret_expr(expr.operands[1])
            return value
        operands = list(map(self.interpret_expr, expr.operands))
        func = get_func(expr)
        return func(operands)

    def interpret_stmt(self, stmt):
        if stmt.type_ == StmtType.RETURN:
            value = None
            if stmt.expr is not None:
                value = self.interpret_expr(stmt.expr)
            raise Return(value)
        elif stmt.type_ == StmtType.FUNCTION:
            func = LoxFunction(stmt, self._env)
            self._env.define(stmt.name.lexeme, func)
        elif stmt.type_ == StmtType.IF:
            if self.interpret_expr(stmt.condition):
                self.interpret_stmt(stmt.then_br)
            elif stmt.else_br is not None:
                self.interpret_stmt(stmt.else_br)
        elif stmt.type_ == StmtType.WHILE:
            while self.interpret_expr(stmt.condition):
                self.interpret_stmt(stmt.body)
        elif stmt.type_ == StmtType.PRINT:
            value = self.interpret_expr(stmt.expr)
            print(value)
        elif stmt.type_ == StmtType.VARIABLE:
            value = None
            if stmt.expr is not None:
                value = self.interpret_expr(stmt.expr)
            self._env.define(stmt.name.lexeme, value)
        elif stmt.type_ == StmtType.BLOCK:
            self.exec_block(stmt.statements, Environment(self._env))
        else:
            self.interpret_expr(stmt.expr)

    def exec_block(self, statements, environment):
        old_env = self._env
        try:
            self._env = environment
            self.interpret(statements)
        finally:
            self._env = old_env

    def interpret(self, stmts):
        for s in stmts:
            self.interpret_stmt(s)

    def resolve(self, expr, depth):
        self._locals[expr] = depth



class Environment(object):

    def __init__(self, enclosing=None):
        self.values = {}
        self.enclosing = enclosing

    def define(self, k, v):
        self.values[k] = v

    def get(self, k):
        if k.lexeme in self.values:
            return self.values[k.lexeme]
        if self.enclosing is not None:
            return self.enclosing.get(k)
        raise InterpreterError('Undefined variable: {}'.format(
            k.lexeme))

    def get_at(self, dist, key):
        if dist == 0:
            return self.values[key.lexeme]
        return self.enclosing.get_at(dist - 1, key)

    def assign(self, k, v):
        if k.lexeme in self.values:
            self.values[k.lexeme] = v
            return
        if self.enclosing is not None:
            return self.enclosing.assign(k, v)

        raise InterpreterError('Undefined variable: ', k.lexeme)

    def assign_at(self, dist, k, v):
        if dist == 0:
            self.values[k.lexeme] = v
        self.enclosing(dist - 1, k, v)


