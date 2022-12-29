from typing import Protocol, TypeVar

from frontend.ast.node import Node
from frontend.ast.tree import *
from frontend.ast.visitor import Visitor
from frontend.scope.globalscope import GlobalScope
from frontend.scope.scope import Scope
from frontend.scope.scopestack import ScopeStack
from frontend.type.array import ArrayType
from utils.error import *

"""
The typer phase: type check abstract syntax tree.
"""


class Typer(Visitor[ScopeStack, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> Program:
        program.globalScope = GlobalScope
        ctx = ScopeStack(program.globalScope)
        
        program.accept(self, ctx)
        return program
    
    def visitProgram(self, program: Program, ctx: ScopeStack) -> None:
        for func in program.children:
            func.accept(self, ctx)

    def visitFunction(self, func: Function, ctx: ScopeStack) -> None:
        if func.getattr('symbol').parameterNum != len(func.params):
            raise DecafGlobalVarDefinedTwiceError(func.ident.value)
        for num, item in enumerate(func.getattr('symbol').para_type):
            if item != func.params[num].var_t.type:
                raise DecafGlobalVarDefinedTwiceError(func.ident.value)
        if func.body:
            func.body.accept(self, ctx)

    def visitCall(self, call: Call, ctx: ScopeStack) -> None:
        if call.getattr('symbol').parameterNum != len(call.argu_list):
            raise DecafBadFuncCallError(call.ident.value)
        for item in call.argu_list:
            # if item != call.argu_list[num].getattr('symbol')      type set and type check, omitted
            # raise DecafBadFuncCallError(call.ident.value)
            item.accept(self, ctx)

    def visitBlock(self, block: Block, ctx: ScopeStack) -> None:
        for child in block:
            child.accept(self, ctx)

    def visitReturn(self, stmt: Return, ctx: ScopeStack) -> None:
        stmt.expr.accept(self, ctx)

    def visitFor(self, stmt: For, ctx: ScopeStack) -> None:
        """
        1. Open a local scope for stmt.init.
        2. Visit stmt.init, stmt.cond, stmt.update.
        3. Open a loop in ctx (for validity checking of break/continue)
        4. Visit body of the loop.
        5. Close the loop and the local scope.
        """
        stmt.init.accept(self, ctx)
        stmt.cond.accept(self, ctx)
        stmt.update.accept(self, ctx)
        stmt.body.accept(self, ctx)

    def visitIf(self, stmt: If, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        stmt.then.accept(self, ctx)

        # check if the else branch exists
        if stmt.otherwise:
            stmt.otherwise.accept(self, ctx)

    def visitWhile(self, stmt: While, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        stmt.body.accept(self, ctx)

    
    def visitDoWhile(self, stmt: DoWhile, ctx: ScopeStack) -> None:
        """
        1. Open a loop in ctx (for validity checking of break/continue)
        2. Visit body of the loop.
        3. Close the loop.
        4. Visit the condition of the loop.
        """
        stmt.body.accept(self, ctx)
        stmt.cond.accept(self, ctx)

    def visitDeclaration(self, decl: Declaration, ctx: ScopeStack) -> None:
        """
        1. Use ctx.findConflict to find if a variable with the same name has been declared.
        2. If not, build a new VarSymbol, and put it into the current scope using ctx.declare.
        3. Set the 'symbol' attribute of decl.
        4. If there is an initial value, visit it.
        """
        if decl.init_expr:
            decl.init_expr.accept(self, ctx)

    def visitAssignment(self, expr: Assignment, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        expr.lhs.accept(self, ctx)
        expr.rhs.accept(self, ctx)

    def visitUnary(self, expr: Unary, ctx: ScopeStack) -> None:
        expr.operand.accept(self, ctx)

    def visitBinary(self, expr: Binary, ctx: ScopeStack) -> None:
        expr.lhs.accept(self, ctx)
        expr.rhs.accept(self, ctx)

    def visitCondExpr(self, expr: ConditionExpression, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        expr.cond.accept(self, ctx)
        expr.then.accept(self, ctx)
        expr.otherwise.accept(self, ctx)
