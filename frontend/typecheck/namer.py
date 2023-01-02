from typing import Protocol, TypeVar, cast

from frontend.ast.node import Node, NullType
from frontend.ast.tree import *
from frontend.ast.visitor import RecursiveVisitor, Visitor
from frontend.scope.globalscope import GlobalScope
from frontend.scope.scope import Scope, ScopeKind
from frontend.scope.scopestack import ScopeStack
from frontend.symbol.funcsymbol import FuncSymbol
from frontend.symbol.symbol import Symbol
from frontend.symbol.varsymbol import VarSymbol
from frontend.type.array import ArrayType
from frontend.type.type import DecafType
from utils.error import *
from utils.riscv import MAX_INT

"""
The namer phase: resolve all symbols defined in the abstract syntax tree and store them in symbol tables (i.e. scopes).
"""


class Namer(Visitor[ScopeStack, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> Program:
        # Global scope. You don't have to consider it until Step 9.
        program.globalScope = GlobalScope
        ctx = ScopeStack(program.globalScope)

        program.accept(self, ctx)
        return program

    def visitProgram(self, program: Program, ctx: ScopeStack) -> None:
        # Check if the 'main' function is missing
        if not program.hasMainFunc():
            raise DecafNoMainFuncError
        for fun_or_ident in program.children:
            fun_or_ident.accept(self, ctx)
        # program.mainFunc().accept(self, ctx)

    def visitFunction(self, func: Function, ctx: ScopeStack) -> None:
        has_key =  ctx.globalscope.containsKey(func.ident.value)
        symbol = None
        if has_key:
            symbol = ctx.globalscope.get(func.ident.value)
            if (not symbol.isFunc) or (symbol.type.type != func.ret_t.type):  # 本需检查函数自变量类型是否一致，但由于只有int型故忽略，类型检查由typer完成
                # raise DecafGlobalVarDefinedTwiceError(str(type(symbol.type)) + str(type(func.ret_t.type)))
                raise DecafGlobalVarDefinedTwiceError(func.ident.value)
            funcScope = symbol.scope
            func.setattr('symbol', symbol)
        else:
            funcScope = Scope(ScopeKind.FORMAL)
        if func.body:
            if has_key:
                if symbol.hasbody:
                    raise DecafGlobalVarDefinedTwiceError(func.ident.value)
            if not has_key:
                newfunc = FuncSymbol(func.ident.value, func.ret_t.type, funcScope, True)
                func.setattr('symbol', newfunc)
                ctx.globalscope.declare(newfunc)
            ctx.open(funcScope)
            if not has_key:
                for param in func.params:
                    param.accept(self, ctx)
            func.body.accept(self, ctx)
            ctx.close()
        else:
            if not has_key:
                newfunc = FuncSymbol(func.ident.value, func.ret_t.type, funcScope, False)
                ctx.globalscope.declare(newfunc)
                ctx.open(funcScope)
                for param in func.params:
                    param.accept(self, ctx)
                ctx.close()
                func.setattr('symbol', newfunc)
        if not has_key:
            func.getattr('symbol').para_type = []
            for param in func.params:
                func.getattr('symbol').addParaType(param.var_t.type)
        
    def visitIndexExpr(self, indexExpr: IndexExpr, ctx: ScopeStack) -> None:
        if not indexExpr.isDeclaration:
            symbol = ctx.lookup(indexExpr.base.value)
            if not symbol or not symbol.type.dim == len(indexExpr.index):
                raise DecafUndefinedVarError("array ( %s )" % (str(indexExpr.base.value)))
            for item in indexExpr.index:
                item.accept(self, ctx)
                if item.getattr('type') != INT:
                    raise DecafBadArraySizeError()
            indexExpr.setattr('type', symbol.type.full_indexed)
            indexExpr.setattr('symbol', symbol)

    def visitCall(self, call: Call, ctx: ScopeStack) -> None:
        conflict = ctx.currentScope().containsKey(call.ident.value)
        if conflict:
            raise DecafBadFuncCallError(call.ident.value)
        has_key =  ctx.globalscope.containsKey(call.ident.value)
        func = ctx.globalscope.get(call.ident.value)
        if not has_key or not func.isFunc:
            raise DecafUndefinedFuncError(call.ident.value)
        call.setattr('symbol', func)
        for item in call.argu_list:
            item.accept(self, ctx)
        call.setattr('type', func.type)

    def visitParameter(self, parameter: Parameter, ctx: ScopeStack) -> None:
        symbol = ctx.findConflict(parameter.ident.value)
        if symbol:
            raise DecafDeclConflictError(parameter.ident.value)
        if isinstance(parameter.ident, Identifier):
            new_sym = VarSymbol(parameter.ident.value, parameter.var_t.type)
        elif isinstance(parameter.ident, IndexExpr):
            arr_type = ArrayType.multidim(INT, [index.value for index in parameter.ident.index])
            new_sym = VarSymbol(parameter.ident.value, arr_type)
        ctx.declare(new_sym)
        parameter.ident.setattr('symbol', new_sym)
        parameter.setattr('type', new_sym.type)

    def visitBlock(self, block: Block, ctx: ScopeStack) -> None:
        if ctx.currentScope().kind != ScopeKind.FORMAL:
            ctx.open(Scope(ScopeKind.LOCAL))
            for child in block:
                child.accept(self, ctx)
            ctx.close()
        else: 
            ctx.currentScope().kind = ScopeKind.LOCAL
            for child in block:
                child.accept(self, ctx)
            ctx.currentScope().kind = ScopeKind.FORMAL
            

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
        ctx.open(Scope(ScopeKind.LOCAL))
        stmt.init.accept(self, ctx)
        stmt.cond.accept(self, ctx)
        stmt.update.accept(self, ctx)
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()
        ctx.close()

    def visitIf(self, stmt: If, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        stmt.then.accept(self, ctx)

        # check if the else branch exists
        if not stmt.otherwise is NULL:
            stmt.otherwise.accept(self, ctx)

    def visitWhile(self, stmt: While, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()

    
    def visitDoWhile(self, stmt: DoWhile, ctx: ScopeStack) -> None:
        """
        1. Open a loop in ctx (for validity checking of break/continue)
        2. Visit body of the loop.
        3. Close the loop.
        4. Visit the condition of the loop.
        """
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()
        stmt.cond.accept(self, ctx)

    def visitBreak(self, stmt: Break, ctx: ScopeStack) -> None:
        if not ctx.inLoop():
            raise DecafBreakOutsideLoopError()

    def visitContinue(self, stmt: Continue, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBreak.
        """
        if not ctx.inLoop():
            raise DecafContinueOutsideLoopError()

    def visitDeclaration(self, decl: Declaration, ctx: ScopeStack) -> None:
        """
        1. Use ctx.findConflict to find if a variable with the same name has been declared.
        2. If not, build a new VarSymbol, and put it into the current scope using ctx.declare.
        3. Set the 'symbol' attribute of decl.
        4. If there is an initial value, visit it.
        """
        if isinstance(decl.ident, Identifier):
            symbol = ctx.findConflict(decl.ident.value) # 注意是value不是name，value对应变量名，name对应对象类型（identifier）
            s1 = VarSymbol(decl.ident.value, decl.var_t.type)
        elif isinstance(decl.ident, IndexExpr):
            symbol = ctx.findConflict(decl.ident.base.value) # 注意是value不是name，value对应变量名，name对应对象类型（identifier
            for index in decl.ident.index:
                if index.value <= 0:
                    raise DecafBadIndexError(decl.ident)
            arr_type = ArrayType.multidim(INT, [index.value for index in decl.ident.index])
            s1 = VarSymbol(decl.ident.base.value, arr_type)
        else:
            raise DecafUndefinedVarError(decl.ident.value)
        if symbol:
            raise DecafDeclConflictError(decl.ident.value) # 注意不是DecafGlobalVarDefinedTwiceError
        ctx.declare(s1)
        decl.setattr('symbol', s1)
        decl.setattr('type', s1.type)
        if ctx.isGlobalScope():
            if decl.init_expr:
                if not isinstance(decl.init_expr, IntLiteral):      # step12 针对数组初始化需要修正
                    raise DecafGlobalVarBadInitValueError(str(decl.init_expr))
                else:
                    s1.setInitValue(decl.init_expr.value)
        else:
            if decl.init_expr:
                decl.init_expr.accept(self, ctx)
                if decl.init_expr.getattr('type') != decl.var_t.type:
                    raise DecafBadAssignTypeError()

    def visitAssignment(self, expr: Assignment, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        if isinstance(expr.lhs, Identifier):
            value = expr.lhs.value
        elif isinstance(expr.lhs, IndexExpr):
            value = expr.lhs.base.value
        else:
            raise DecafBadAssignTypeError()
        if not ctx.lookup(value):
            raise DecafUndefinedVarError(value)
        expr.lhs.accept(self, ctx)
        expr.rhs.accept(self, ctx)
        if expr.rhs.getattr('type') != expr.lhs.getattr('type') or expr.lhs.getattr('type') != INT:
            # raise DecafBadFuncCallError(str(expr.rhs.getattr('type')) + str(expr.lhs.getattr('type')))
            raise DecafBadAssignTypeError()
        expr.setattr('type', expr.lhs.getattr('type'))

    def visitUnary(self, expr: Unary, ctx: ScopeStack) -> None:
        expr.operand.accept(self, ctx)
        if expr.operand.getattr('type') != INT:
            raise DecafBadAssignTypeError()
        expr.setattr('type', expr.getattr('type'))

    def visitBinary(self, expr: Binary, ctx: ScopeStack) -> None:
        expr.lhs.accept(self, ctx)
        expr.rhs.accept(self, ctx)
        if expr.rhs.getattr('type') != expr.lhs.getattr('type'):
            raise DecafBadAssignTypeError()
        expr.setattr('type', expr.lhs.getattr('type'))

    def visitCondExpr(self, expr: ConditionExpression, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        expr.cond.accept(self, ctx)
        expr.then.accept(self, ctx)
        expr.otherwise.accept(self, ctx)
        if not expr.cond.getattr('type') != INT or expr.then.getattr('type') != expr.otherwise.getattr('type'):
            raise DecafBadAssignTypeError()
        expr.setattr('type', expr.then.getattr('type'))


    def visitIdentifier(self, ident: Identifier, ctx: ScopeStack) -> None:
        """
        1. Use ctx.lookup to find the symbol corresponding to ident.
        2. If it has not been declared, raise a DecafUndefinedVarError.
        3. Set the 'symbol' attribute of ident.
        """
        symbol = ctx.lookup(ident.value) # 同样需要注意查找value而不是name
        if not symbol:
            raise DecafUndefinedVarError(ident.value)
        if symbol.isFunc:
            raise DecafBadFuncCallError(ident.value)
        ident.setattr('symbol', symbol)
        ident.setattr('type', symbol.type)

    def visitIntLiteral(self, expr: IntLiteral, ctx: ScopeStack) -> None:
        value = expr.value
        if value > MAX_INT:
            raise DecafBadIntValueError(value)
        expr.setattr('type', INT)
