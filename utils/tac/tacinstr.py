from enum import Enum, auto, unique
from typing import Any, Optional, Union
from utils.error import *

from utils.label.label import Label
from utils.tac.nativeinstr import NativeInstr
from utils.tac.reg import Reg

from .tacop import *
from .tacvisitor import TACVisitor
from .temp import Temp


class TACInstr:
    def __init__(
        self,
        kind: InstrKind,
        dsts: list[Temp],
        srcs: list[Temp],
        label: Optional[Label],
    ) -> None:
        self.kind = kind
        self.dsts = dsts.copy()
        self.srcs = srcs.copy()
        self.label = label

    def getRead(self) -> list[int]:
        for item in self.srcs:
            if not hasattr(item, "index"):
                raise DecafBadIndexError(self)
        return [src.index for src in self.srcs]

    def getWritten(self) -> list[int]:
        return [dst.index for dst in self.dsts]

    def isLabel(self) -> bool:
        return self.kind is InstrKind.LABEL

    def isSequential(self) -> bool:
        return self.kind == InstrKind.SEQ

    def isReturn(self) -> bool:
        return self.kind == InstrKind.RET

    def toNative(self, dstRegs: list[Reg], srcRegs: list[Reg]) -> NativeInstr:
        oldDsts = dstRegs
        oldSrcs = srcRegs
        self.dsts = dstRegs
        self.srcs = srcRegs
        instrString = self.__str__()
        newInstr = NativeInstr(self.kind, dstRegs, srcRegs, self.label, instrString)
        self.dsts = oldDsts
        self.srcs = oldSrcs
        return newInstr

    def accept(self, v: TACVisitor) -> None:
        pass


# Assignment instruction.
class Assign(TACInstr):
    def __init__(self, dst: Temp, src: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [src], None)
        self.dst = dst
        self.src = src

    def __str__(self) -> str:
        return "%s = %s" % (self.dst, self.src)

    def accept(self, v: TACVisitor) -> None:
        v.visitAssign(self)

class Param(TACInstr):
    def __init__(self, T0: Temp) -> None:
        super().__init__(InstrKind.SEQ, [T0], [], None)
        self.T0 = T0

    def __str__(self) -> str:
        return "param %s" % (self.T0)
    
    def accept(self, v: TACVisitor) -> None:
        v.visitParam(self)

class Call(TACInstr):
    def __init__(self, dst: Temp, label: Label) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], label)
        self.dst = dst

    def __str__(self) -> str:
        return "%s = call %s" % (self.dst, self.label)

    def accept(self, v: TACVisitor) -> None:
        return v.visitCall(self)

# Loading an immediate 32-bit constant.
class LoadImm4(TACInstr):
    def __init__(self, dst: Temp, value: int) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], None)
        self.dst = dst
        self.value = value

    def __str__(self) -> str:
        return "%s = %d" % (self.dst, self.value)

    def accept(self, v: TACVisitor) -> None:
        v.visitLoadImm4(self)

class Load(TACInstr):
    def __init__(self, dst: Temp, addr_temp: Temp, offset: int) -> None:
        super().__init__(InstrKind.SEQ, [dst], [addr_temp], None)
        self.addr_temp = addr_temp
        self.offset = offset
        self.dst = dst
    
    def __str__(self) -> str:
        return "%s = load %s, %d" % (self.dst, self.addr_temp, self.offset)

    def accept(self, v: TACVisitor) -> None:
        return v.visitLoad(self)

class Store(TACInstr):
    def __init__(self, src: Temp, addr_temp: Temp, offset: int) -> None:
        super().__init__(InstrKind.SEQ, [], [src, addr_temp], None)
        self.src = src
        self.addr_temp = addr_temp
        self.offset = offset

    def __str__(self) -> str:
        return "store %s, %s, %d" % (self.src, self.addr_temp, self.offset)

    def accept(self, v: TACVisitor) -> None:
        return v.visitStore(self)

class Alloc(TACInstr):
    def __init__(self, dst: Temp, size: int) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], None)
        self.dst = dst
        self.size = size
    
    def __str__(self) -> str:
        return "%s = alloc %d" % (self.dst, self.size)

    def accept(self, v: TACVisitor) -> None:
        return v.visitAlloc(self)

class LoadSymbol(TACInstr):
    def __init__(self, dst: Temp, symbol: str) -> None:
        super().__init__(InstrKind.SEQ, [], [], None)
        self.dst = dst
        self.symbol = symbol

    def __str__(self) -> str:
        return "%s = load_symbol %s" % (self.dst, self.symbol)

    def accept(self, v: TACVisitor) -> None:
        return v.visitLoadSymbol(self)

# Unary operations.
class Unary(TACInstr):
    def __init__(self, op: UnaryOp, dst: Temp, operand: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [operand], None)
        """
        op:操作指令
        dst:操作结果
        oprand:操作数寄存器
        """
        self.op = op
        self.dst = dst
        self.operand = operand

    def __str__(self) -> str:
        return "%s = %s %s" % (
            self.dst,
            ("-" if (self.op == UnaryOp.NEG) else "!" if (self.op == UnaryOp.SEQZ) else "~" if (self.op == UnaryOp.NOT) else "BOOL"),
            self.operand,
        )

    def accept(self, v: TACVisitor) -> None:
        v.visitUnary(self)


# Binary Operations.
class Binary(TACInstr):
    def __init__(self, op: BinaryOp, dst: Temp, lhs: Temp, rhs: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [lhs, rhs], None)
        """
        op:操作指令
        dst:操作结果值
        lhs:左边数寄存器
        rhs:右边数寄存器
        """
        self.op = op
        self.dst = dst
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self) -> str:
        opStr = {
            BinaryOp.ADD: "+",
            BinaryOp.SUB: "-",
            BinaryOp.MUL: "*",
            BinaryOp.DIV: "/",
            BinaryOp.REM: "%",
            BinaryOp.EQU: "==",
            BinaryOp.NEQ: "!=",
            BinaryOp.SLT: "<",
            BinaryOp.LEQ: "<=",
            BinaryOp.SGT: ">",
            BinaryOp.GEQ: ">=",
            BinaryOp.AND: "&",
            BinaryOp.OR: "|",
            BinaryOp.LAD: "&&",
            BinaryOp.LOR: "||"
        }[self.op]
        return "%s = (%s %s %s)" % (self.dst, self.lhs, opStr, self.rhs)

    def accept(self, v: TACVisitor) -> None:
        v.visitBinary(self)


# Branching instruction.
class Branch(TACInstr):
    def __init__(self, target: Label) -> None:
        super().__init__(InstrKind.JMP, [], [], target)
        self.target = target

    def __str__(self) -> str:
        return "branch %s" % str(self.target)

    def accept(self, v: TACVisitor) -> None:
        v.visitBranch(self)


# Branching with conditions.
class CondBranch(TACInstr):
    def __init__(self, op: CondBranchOp, cond: Temp, target: Label) -> None:
        super().__init__(InstrKind.COND_JMP, [], [cond], target)
        self.op = op
        self.cond = cond
        self.target = target

    def __str__(self) -> str:
        return "if (%s %s) branch %s" % (
            self.cond,
            "== 0" if self.op == CondBranchOp.BEQ else "!= 0",
            str(self.target),
        )

    def accept(self, v: TACVisitor) -> None:
        v.visitCondBranch(self)


# Return instruction.
class Return(TACInstr):
    def __init__(self, value: Optional[Temp]) -> None:
        if value is None:
            super().__init__(InstrKind.RET, [], [], None)
        else:
            super().__init__(InstrKind.RET, [], [value], None)
        self.value = value

    def __str__(self) -> str:
        return "return" if (self.value is None) else ("return " + str(self.value))

    def accept(self, v: TACVisitor) -> None:
        v.visitReturn(self)


# Annotation (used for debugging).
class Memo(TACInstr):
    def __init__(self, msg: str) -> None:
        super().__init__(InstrKind.SEQ, [], [], None)
        self.msg = msg

    def __str__(self) -> str:
        return "memo '%s'" % self.msg

    def accept(self, v: TACVisitor) -> None:
        v.visitMemo(self)


# Label (function entry or branching target).
class Mark(TACInstr):
    def __init__(self, label: Label) -> None:
        super().__init__(InstrKind.LABEL, [], [], label)

    def __str__(self) -> str:
        return "%s:" % str(self.label)

    def accept(self, v: TACVisitor) -> None:
        v.visitMark(self)
