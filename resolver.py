import enum
import error
import time
from lexing import TokenType
from parsing import Expr, ExprType, StmtType

class FunctionType(enum.Enum):
    NONE = 0
    FUNCTION = 1
    METHOD = 2
    INITIALIZER = 3


class ClassType(enum.Enum):
    NONE = 0
    CLASS = 1


class Resolver(object):

    def __init__(self, interpreter):
        self._interpreter = interpreter
        self._scopes = []
        self._current_function = FunctionType.NONE
        self._current_class = ClassType.NONE

    def visit_block(self, stmt):
        self.begin_scope()
        self.resolve(stmt.statements)
        self.end_scope()

    def resolve(self, statements):
        for s in statements:
            self.resolve_stmt(s)

    def begin_scope(self):
        self._scopes.append({})

    def end_scope(self):
        self._scopes.pop()

    def resolve_expr(self, expr):
        if expr.expr_type == ExprType.THIS:
            if self._current_class == ClassType.NONE:
                error.error(expr.op, 'cannot use this outside of a class')
            self.resolve_local(expr, expr.op)
        if expr.expr_type == ExprType.SET:
            self.resolve_expr(expr.value)
            self.resolve_expr(expr.obj)
        elif expr.expr_type == ExprType.GET:
            self.resolve_expr(expr.obj)
        elif expr.expr_type == ExprType.VARIABLE:
            if self._scopes and not self._scopes[-1].get(expr.op.lexeme, True):
                error.error(
                    expr.op,
                    'Cannot read local varialble in its own initializer')
            self.resolve_local(expr, expr.op)
        elif expr.expr_type == ExprType.ASSIGN:
            self.resolve_expr(expr.operands[1])
            self.resolve_local(expr, expr.op)
        else:
            for op in expr.operands:
                self.resolve_expr(op)


    def resolve_local(self, expr, name):
        for i, s in enumerate(reversed(self._scopes)):
            if name.lexeme in s:
                self._interpreter.resolve(expr, i)
        # global

    def declare(self, name):
        if not self._scopes:
            return
        scope = self._scopes[-1]
        if name.lexeme in scope:
            error.error(name, 'Variable with this name already declared in scope..')
        scope[name.lexeme] = False

    def define(self, name):
        if not self._scopes:
            return
        scope = self._scopes[-1]
        scope[name.lexeme] = True

    def resolve_function(self, function, function_type):
        enclosing = self._current_function
        self._current_function = function_type

        self.begin_scope()
        for param in function.params:
            self.declare(param)
            self.define(param)
        self.resolve(function.body)
        self.end_scope()
        self._current_function = enclosing

    def resolve_stmt(self, stmt):
        # switch on type
        if stmt.type_ == StmtType.CLASS:
            enclosing_class = self._current_class
            self._current_class = ClassType.CLASS
            self.declare(stmt.name)
            self.define(stmt.name)
            self.begin_scope()
            self._scopes[0]['this'] = True
            for method in stmt.methods:
                decl = FunctionType.METHOD
                if method.name.lexeme == 'init':
                    decl = FunctionType.INITIALIZER
                self.resolve_function(method, decl)
            self.end_scope()
            self._current_class = enclosing_class
        elif stmt.type_ == StmtType.FUNCTION:
            self.declare(stmt.name)
            self.define(stmt.name)
            self.resolve_function(stmt, FunctionType.FUNCTION)
        elif stmt.type_ == StmtType.IF:
            self.resolve_expr(stmt.condition)
            self.resolve_stmt(stmt.then_br)
            if stmt.else_br:
                self.resolve_stmt(stmt.else_br)
        elif stmt.type_ == StmtType.PRINT:
            self.resolve_expr(stmt.expr)
        elif stmt.type_ == StmtType.RETURN:
            if self._current_function == FunctionType.NONE:
                error.error(stmt.keyword, 'Cannot return from top level')
            if stmt.expr:
                if self._current_function == FunctionType.INITIALIZER:
                    error.error(stmt.op,
                            'Cannot return a value from an initializer')
                self.resolve_expr(stmt.expr)
        elif stmt.type_ == StmtType.WHILE:
            self.resolve_expr(stmt.condition)
            self.resolve_stmt(stmt.body)
        elif stmt.type_ == StmtType.VARIABLE:
            self.declare(stmt.name)
            if stmt.expr is not None:
                self.resolve_expr(stmt.expr)
            self.define(stmt.name)
        elif stmt.type_ == StmtType.BLOCK:
            self.begin_scope()
            self.resolve(stmt.statements)
            self.end_scope()
        else:
            self.resolve_expr(stmt.expr)
