import utils.riscv as riscv
from frontend.ast import node
from frontend.ast.tree import *
from frontend.ast.visitor import Visitor
from frontend.symbol.varsymbol import VarSymbol
from frontend.type.array import ArrayType
from utils.tac import tacop
from utils.tac.funcvisitor import FuncVisitor
from utils.tac.programwriter import ProgramWriter
from utils.tac.tacprog import TACProg
from utils.tac.temp import Temp
from utils.error import *

"""
The TAC generation phase: translate the abstract syntax tree into three-address code.
"""


class TACGen(Visitor[FuncVisitor, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> TACProg:
        # mainFunc = program.mainFunc()
        # pw = ProgramWriter(["main"])
        # # The function visitor of 'main' is special.
        # mv = pw.visitMainFunc()

        # mainFunc.body.accept(self, mv)
        # # Remember to call mv.visitEnd after the translation a function.
        # mv.visitEnd()

        funcs = program.functions()
        pw = ProgramWriter([funcname for funcname in funcs])
        for funcname in funcs:
            if funcs[funcname].body is not NULL:
                fv = pw.visitFunc(funcname, len(funcs[funcname].params))
                for param in funcs[funcname].params:
                    param.ident.getattr('symbol').temp = fv.freshTemp()
                funcs[funcname].body.accept(self, fv)
                fv.visitEnd()

        # Remember to call pw.visitEnd before finishing the translation phase.
        return pw.visitEnd()

    def visitBlock(self, block: Block, mv: FuncVisitor) -> None:
        for child in block:
            child.accept(self, mv)

    def visitCall(self, call: Call, mv: FuncVisitor) -> None:
        for item in call.argu_list:
            item.accept(self, mv)
        for item in call.argu_list:
           mv.visitParam(item.getattr('val'))
        call.setattr(
            'val', mv.visitCall(mv.ctx.getFuncLabel(call.ident.value))
        )

    def visitReturn(self, stmt: Return, mv: FuncVisitor) -> None:
        stmt.expr.accept(self, mv)
        mv.visitReturn(stmt.expr.getattr("val"))

    def visitBreak(self, stmt: Break, mv: FuncVisitor) -> None:
        mv.visitBranch(mv.getBreakLabel())

    def visitContinue(self, stmt: Continue, mv: FuncVisitor) -> None:
        mv.visitBranch(mv.getContinueLabel())

    def visitIdentifier(self, ident: Identifier, mv: FuncVisitor) -> None:
        """
        1. Set the 'val' attribute of ident as the temp variable of the 'symbol' attribute of ident.
        """
        # if not hasattr(ident.getattr('symbol'), 'temp'):
        #     raise DecafGlobalVarDefinedTwiceError(ident.getattr('symbol'))
        ident.setattr('val', ident.getattr('symbol').temp)

    def visitDeclaration(self, decl: Declaration, mv: FuncVisitor) -> None:
        """
        1. Get the 'symbol' attribute of decl.
        2. Use mv.freshTemp to get a new temp variable for this symbol.
        3. If the declaration has an initial value, use mv.visitAssignment to set it.
        """
        decl.getattr('symbol').temp = mv.freshTemp()
        if decl.init_expr:
            decl.init_expr.accept(self, mv) # 别忘了！！！
            decl.setattr(
                'val', mv.visitAssignment(decl.getattr('symbol').temp, decl.init_expr.getattr('val'))
            )

    def visitAssignment(self, expr: Assignment, mv: FuncVisitor) -> None:
        """
        1. Visit the right hand side of expr, and get the temp variable of left hand side.
        2. Use mv.visitAssignment to emit an assignment instruction.
        3. Set the 'val' attribute of expr as the value of assignment instruction.
        """
        expr.rhs.accept(self, mv)
        expr.setattr(
            'val', mv.visitAssignment(expr.lhs.getattr('symbol').temp, expr.rhs.getattr('val'))
        )

    def visitIf(self, stmt: If, mv: FuncVisitor) -> None:
        stmt.cond.accept(self, mv)

        if stmt.otherwise is NULL:
            skipLabel = mv.freshLabel()
            mv.visitCondBranch(
                tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), skipLabel
            )
            stmt.then.accept(self, mv)
            mv.visitLabel(skipLabel)
        else:
            skipLabel = mv.freshLabel()
            exitLabel = mv.freshLabel()
            mv.visitCondBranch(
                tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), skipLabel
            )
            stmt.then.accept(self, mv)
            mv.visitBranch(exitLabel)
            mv.visitLabel(skipLabel)
            stmt.otherwise.accept(self, mv)
            mv.visitLabel(exitLabel)

    def visitWhile(self, stmt: While, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)

        mv.visitLabel(beginLabel)
        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)

        stmt.body.accept(self, mv)
        mv.visitLabel(loopLabel)
        mv.visitBranch(beginLabel)
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitFor(self, stmt: For, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)

        stmt.init.accept(self, mv)
        mv.visitLabel(beginLabel)
        if stmt.cond:               ###注意！！！一定要有cond才能进行BEQ
            stmt.cond.accept(self, mv)
            mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)

        stmt.body.accept(self, mv)
        mv.visitLabel(loopLabel)
        stmt.update.accept(self, mv)
        mv.visitBranch(beginLabel)
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitDoWhile(self, stmt: DoWhile, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)

        mv.visitLabel(beginLabel)
        stmt.body.accept(self, mv)
        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)
        mv.visitLabel(loopLabel)
        mv.visitBranch(beginLabel)
        
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitUnary(self, expr: Unary, mv: FuncVisitor) -> None:
        expr.operand.accept(self, mv)

        op = {
            node.UnaryOp.Neg: tacop.UnaryOp.NEG,
            node.UnaryOp.BitNot: tacop.UnaryOp.NOT,
            node.UnaryOp.LogicNot: tacop.UnaryOp.SEQZ
            # You can add unary operations here.
        }[expr.op]
        expr.setattr("val", mv.visitUnary(op, expr.operand.getattr("val")))

    def visitBinary(self, expr: Binary, mv: FuncVisitor) -> None:
        expr.lhs.accept(self, mv)
        expr.rhs.accept(self, mv)

        op = {
            node.BinaryOp.Add: tacop.BinaryOp.ADD,
            node.BinaryOp.Sub: tacop.BinaryOp.SUB,
            node.BinaryOp.Mul: tacop.BinaryOp.MUL,
            node.BinaryOp.Div: tacop.BinaryOp.DIV,
            node.BinaryOp.Mod: tacop.BinaryOp.REM,
            node.BinaryOp.LT: tacop.BinaryOp.SLT,
            node.BinaryOp.GT: tacop.BinaryOp.SGT,
            node.BinaryOp.LE: tacop.BinaryOp.LEQ,
            node.BinaryOp.GE: tacop.BinaryOp.GEQ,
            node.BinaryOp.NE: tacop.BinaryOp.NEQ,
            node.BinaryOp.EQ: tacop.BinaryOp.EQU,
            node.BinaryOp.LogicAnd: tacop.BinaryOp.LAD,
            node.BinaryOp.LogicOr: tacop.BinaryOp.LOR,
            node.BinaryOp.BitAnd: tacop.BinaryOp.AND,
            node.BinaryOp.BitOr: tacop.BinaryOp.OR
            # You can add binary operations here.
        }[expr.op]
        expr.setattr(
            "val", mv.visitBinary(op, expr.lhs.getattr("val"), expr.rhs.getattr("val"))
        )

    def visitCondExpr(self, expr: ConditionExpression, mv: FuncVisitor) -> None:
        """
        1. Refer to the implementation of visitIf and visitBinary.
        """
        expr.cond.accept(self, mv)  # 计算条件表达式值
        skipLable = mv.freshLabel() # 跳转标签
        exitLable = mv.freshLabel() # 退出标签
        mv.visitCondBranch(
            tacop.CondBranchOp.BEQ, expr.cond.getattr('val'), skipLable
        )                           # 加入条件跳转指令
        expr.then.accept(self, mv)  # 访问then
        # 需注意条件表达式需要将表达式的整体设置一个val并赋值，这里用then的寄存器存储最终值，因此对else分支需要将else的内容存入then
        expr.setattr('val', expr.then.getattr('val')) # 将then的val寄存器赋值到表达式val
        mv.visitBranch(exitLable)   # 加入实际跳转对应分支指令
        mv.visitLabel(skipLable)    # 添加分支标签到指令
        expr.otherwise.accept(self, mv) #访问else
        mv.visitAssignment(expr.then.getattr('val'), expr.otherwise.getattr('val')) # 将else的val拷贝到then的寄存器
        mv.visitLabel(exitLable)    # 添加退出标签到指令


    def visitIntLiteral(self, expr: IntLiteral, mv: FuncVisitor) -> None:
        expr.setattr("val", mv.visitLoad(expr.value))
