from textwrap import indent
from typing import Sequence, Tuple

from backend.asmemitter import AsmEmitter
from utils.error import *
from utils.label.label import Label, LabelKind
from utils.riscv import Riscv
from utils.tac.reg import Reg
from utils.tac.tacfunc import TACFunc
from utils.tac.tacinstr import *
from utils.tac.tacvisitor import TACVisitor
from utils.tac import tacop
from frontend.scope.globalscope import GlobalScopeType, ScopeKind
from frontend.symbol.varsymbol import VarSymbol

from ..subroutineemitter import SubroutineEmitter
from ..subroutineinfo import SubroutineInfo

"""
RiscvAsmEmitter: an AsmEmitter for RiscV
"""


class RiscvAsmEmitter(AsmEmitter):
    def __init__(
        self,
        allocatableRegs: list[Reg],
        callerSaveRegs: list[Reg],
    ) -> None:
        super().__init__(allocatableRegs, callerSaveRegs)

    
        # the start of the asm code
        # int step10, you need to add the declaration of global var here

    def generateGlobal(self, globalScope: GlobalScopeType):
        data_var: list[VarSymbol] = []
        bss_var: list[VarSymbol] = []
        for symbol in globalScope.symbols:
            if isinstance(globalScope.symbols[symbol], VarSymbol):
                if globalScope.symbols[symbol].initialized:
                    data_var.append(globalScope.symbols[symbol])
                else:
                    bss_var.append(globalScope.symbols[symbol])
        if len(data_var) > 0:
            self.printer.println(".data")
            for symbol in data_var:
                self.printer.println(".global %s" % (symbol.name))
                self.printer.println("%s:" % (symbol.name))
                self.printer.println("    .word %d" % (symbol.initValue))
            self.printer.println("")

        if len(bss_var) > 0:
            self.printer.println(".bss")
            for symbol in bss_var:
                self.printer.println(".global %s" % (symbol.name))
                self.printer.println("%s:" % (symbol.name))
                self.printer.println("    .space %d" % (4))
            self.printer.println("")

        self.printer.println(".text")
        self.printer.println(".global main")
        self.printer.println("")
    # transform tac instrs to RiscV instrs
    # collect some info which is saved in SubroutineInfo for SubroutineEmitter
    def selectInstr(self, func: TACFunc) -> tuple[list[str], SubroutineInfo]:

        selector: RiscvAsmEmitter.RiscvInstrSelector = (
            RiscvAsmEmitter.RiscvInstrSelector(func.entry)
        )
        for instr in func.getInstrSeq():
            instr.accept(selector)

        info = SubroutineInfo(func.entry, func.numArgs)

        return (selector.seq, info)

    # use info to construct a RiscvSubroutineEmitter
    def emitSubroutine(self, info: SubroutineInfo):
        return RiscvSubroutineEmitter(self, info)

    # return all the string stored in asmcodeprinter
    def emitEnd(self):
        return self.printer.close()

    class RiscvInstrSelector(TACVisitor):
        def __init__(self, entry: Label) -> None:
            self.entry = entry
            self.seq = []
            self.paraNum = 0
            # self.callParaList : list[Param] = []

        # in step11, you need to think about how to deal with globalTemp in almost all the visit functions. 
        def visitReturn(self, instr: Return) -> None:
            if instr.value is not None:
                self.seq.append(Riscv.Move(Riscv.A0, instr.value))
            else:
                self.seq.append(Riscv.LoadImm(Riscv.A0, 0))
            self.seq.append(Riscv.JumpToEpilogue(self.entry))

        def visitMark(self, instr: Mark) -> None:
            self.seq.append(Riscv.RiscvLabel(instr.label))

        def visitLoadImm4(self, instr: LoadImm4) -> None:
            self.seq.append(Riscv.LoadImm(instr.dst, instr.value))

        def visitUnary(self, instr: Unary) -> None:
            self.seq.append(Riscv.Unary(instr.op, instr.dst, instr.operand))
 
        def visitBinary(self, instr: Binary) -> None:
            if instr.op == tacop.BinaryOp.EQU:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SUB, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SEQZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.GEQ:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SLT, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SEQZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.LEQ:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SGT, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SEQZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.NEQ:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SUB, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.LAD:
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.lhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.NEG, instr.dst, instr.dst))
                self.seq.append(Riscv.Binary(tacop.BinaryOp.AND, instr.dst, instr.dst, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.LOR:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.OR, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.dst))
            else:
                self.seq.append(Riscv.Binary(instr.op, instr.dst, instr.lhs, instr.rhs))
        def visitAssign(self, instr: Assign) -> None:
            self.seq.append(Riscv.Move(instr.dst, instr.src))
        def visitCondBranch(self, instr: CondBranch) -> None:
            self.seq.append(Riscv.Branch(instr.cond, instr.label))
        
        def visitBranch(self, instr: Branch) -> None:
            self.seq.append(Riscv.Jump(instr.target))
        
        def visitParam(self, instr: Param) -> None:
            self.paraNum += 1
            # if self.paraNum <= 7:
            #     self.seq.append(Riscv.Store(Riscv.ArgRegs[self.paraNum], Riscv.SP, -4 * (self.paraNum + 8)))
            #     self.seq.append(Riscv.Move(Riscv.ArgRegs[self.paraNum], instr.T0))
            # else:
            self.seq.append(Riscv.Store(instr.T0, Riscv.SP, -4 * (self.paraNum + len(Riscv.CallerSaved))))
            # self.callParaList.append(instr)

        def visitCall(self, instr: Call) -> None:
            for i in range(len(Riscv.CallerSaved)):
                # if i < 7 or i > self.paraNum + 6:
                self.seq.append(Riscv.Store(Riscv.CallerSaved[i], Riscv.SP, -4 * (i + 1)))
            for i in range(min(self.paraNum, 8)):
                self.seq.append(Riscv.Load(Riscv.ArgRegs[i], Riscv.SP, -4 * (i + len(Riscv.CallerSaved) + 1)))
            for i in range(8, self.paraNum + 1):
                self.seq.append(Riscv.Load(Riscv.T0, Riscv.SP, -4 * (i + len(Riscv.CallerSaved))))
                self.seq.append(Riscv.Store(Riscv.T0, Riscv.SP, -4 * (len(Riscv.CallerSaved) + self.paraNum * 2 - i + 1)))
            # for i in range(min(self.paraNum, 8)):
            #     self.seq.append(Riscv.Store(Riscv.ArgRegs[i], Riscv.SP, -4 * (self.paraNum + 7)))
            #     self.seq.append(Riscv.Load(Riscv.ArgRegs[i], Riscv.SP, -4 * (self.paraNum + len(Riscv.CallerSaved))))
            self.seq.append(Riscv.SPAd(-4 * (len(Riscv.CallerSaved) + self.paraNum + max(0, self.paraNum - 8))))
            # for i in range(len(self.callParaList)):
            #     self.seq.append(Riscv.Store(self.callParaList[i].T0, Riscv.SP, 4 * i))
            self.seq.append(Riscv.Call(instr.label, instr.dst))
            self.seq.append(Riscv.SPAd(4 * (len(Riscv.CallerSaved) + self.paraNum + max(0, self.paraNum - 8))))
            for i in range(len(Riscv.CallerSaved)):
                # if i < 7 or i > self.paraNum + 6:
                if i != 7:
                    self.seq.append(Riscv.Load(Riscv.CallerSaved[i], Riscv.SP, -4 * (i + 1)))
            # for i in range(1, min(self.paraNum, 8)):
            #     self.seq.append(Riscv.Load(Riscv.ArgRegs[i], Riscv.SP, -4 * (i + 8)))
            self.seq.append(Riscv.Move(instr.dsts[0], Riscv.A0))
            self.seq.append(Riscv.Load(Riscv.A0, Riscv.SP, -32))
            self.paraNum = 0
            # self.callParaList = []

        def visitLoadSymbol(self, instr: LoadSymbol) -> None:
            self.seq.append(Riscv.LoadSymbol(instr.dst, instr.symbol))

        def visitLoad(self, instr: Load) -> None:
            self.seq.append(Riscv.Load(instr.dst, instr.addr_temp, instr.offset))

        def visitStore(self, instr: Store) -> None:
            self.seq.append(Riscv.Store(instr.src, instr.addr_temp, instr.offset))


        # in step9, you need to think about how to pass the parameters and how to store and restore callerSave regs
        # in step11, you need to think about how to store the array 
"""
RiscvAsmEmitter: an SubroutineEmitter for RiscV
"""

class RiscvSubroutineEmitter(SubroutineEmitter):
    def __init__(self, emitter: RiscvAsmEmitter, info: SubroutineInfo) -> None:
        super().__init__(emitter, info)
        self.numArgs = info.numArgs
        # + 4 is for the RA reg 
        self.nextLocalOffset = 4 * len(Riscv.CalleeSaved) + 8
        
        # the buf which stored all the NativeInstrs in this function
        self.buf: list[NativeInstr] = []

        # from temp to int
        # record where a temp is stored in the stack
        self.offsets = {}

        self.printer.printLabel(info.funcLabel)

        # in step9, step11 you can compute the offset of local array and parameters here

    def emitComment(self, comment: str) -> None:
        # you can add some log here to help you debug
        self.printer.printComment("later emitted: " + comment)
    
    # store some temp to stack
    # usually happen when reaching the end of a basicblock
    # in step9, you need to think about the fuction parameters here
    def emitStoreToStack(self, src: Reg) -> None:
        if src.temp.index not in self.offsets:
            self.offsets[src.temp.index] = self.nextLocalOffset
            self.nextLocalOffset += 4
        self.buf.append(
            Riscv.NativeStoreWord(src, Riscv.SP, self.offsets[src.temp.index])
        )

    # load some temp from stack
    # usually happen when using a temp which is stored to stack before
    # in step9, you need to think about the fuction parameters here
    def emitLoadFromStack(self, dst: Reg, src: Temp):
        if src.index <  self.numArgs:
            self.buf.append(Riscv.Load(dst, Riscv.FP, (src.index - 8) * 4))
        elif src.index not in self.offsets:
            raise IllegalArgumentException
        else:
            self.buf.append(
                Riscv.Load(dst, Riscv.SP, self.offsets[src.index])
            )

    # add a NativeInstr to buf
    # when calling the fuction emitEnd, all the instr in buf will be transformed to RiscV code
    def emitNative(self, instr: NativeInstr):
        # if instr.kind == InstrKind.CALL:
        #     # used_list = []
        #     # for i in range(len(Riscv.CallerSaved)):
        #     #     if Riscv.CallerSaved[i].occupied:
        #     #         used_list.append(i)
        #     #         self.buf.append(
        #     #         Riscv.NativeStoreWord(Riscv.CallerSaved[i], Riscv.SP, -4 * (i + 1))
        #     #         )
        #     self.buf.append(Riscv.SPAdd(-len(Riscv.CallerSaved) * 4 + 4 * (max(self.numArgs, 8) - 8)))
        #     self.buf.append(instr)
        #     self.buf.append(Riscv.SPAdd(len(Riscv.CallerSaved) * 4 + 4 * (max(self.numArgs, 8) - 8)))
        #     # for i in used_list:
        #     #     self.buf.append(
        #     #     Riscv.NativeLoadWord(Riscv.CallerSaved[i], Riscv.SP, -4 * (i + 1))
        #     #     )
        # else:
        self.buf.append(instr)

    def emitLabel(self, label: Label):
        self.buf.append(Riscv.RiscvLabel(label).toNative([], []))

    
    def emitEnd(self):
        self.printer.printComment("start of prologue")
        self.printer.printInstr(Riscv.SPAdd(-self.nextLocalOffset))

        # in step9, you need to think about how to store RA here
        # you can get some ideas from how to save CalleeSaved regs
        for i in range(len(Riscv.CalleeSaved)):
            if Riscv.CalleeSaved[i].isUsed():
                self.printer.printInstr(
                    Riscv.NativeStoreWord(Riscv.CalleeSaved[i], Riscv.SP, 4 * i)
                )
        self.printer.printInstr(Riscv.NativeStoreWord(Riscv.RA, Riscv.SP, 4 * len(Riscv.CalleeSaved)))
        self.printer.printInstr(
            Riscv.NativeStoreWord(Riscv.FP, Riscv.SP, 4 * len(Riscv.CalleeSaved) + 4)
        )

        self.printer.println("addi " + Riscv.FMT3.format(str(Riscv.FP), str(Riscv.SP), str(self.nextLocalOffset)))

        self.printer.printComment("end of prologue")
        self.printer.println("")

        self.printer.printComment("start of body")

        # in step9, you need to think about how to pass the parameters here
        # you can use the stack or regs

        # using asmcodeprinter to output the RiscV code
        for instr in self.buf:
            self.printer.printInstr(instr)

        self.printer.printComment("end of body")
        self.printer.println("")

        self.printer.printLabel(
            Label(LabelKind.TEMP, self.info.funcLabel.name + Riscv.EPILOGUE_SUFFIX)
        )
        self.printer.printComment("start of epilogue")

        for i in range(len(Riscv.CalleeSaved)):
            if Riscv.CalleeSaved[i].isUsed():
                self.printer.printInstr(
                    Riscv.NativeLoadWord(Riscv.CalleeSaved[i], Riscv.SP, 4 * i)
                )
        self.printer.printInstr(Riscv.NativeLoadWord(Riscv.RA, Riscv.SP, 4 * len(Riscv.CalleeSaved)))
        self.printer.printInstr(Riscv.NativeLoadWord(Riscv.FP, Riscv.SP, 4 * len(Riscv.CalleeSaved) + 4))

        self.printer.printInstr(Riscv.SPAdd(self.nextLocalOffset))
        self.printer.printComment("end of epilogue")
        self.printer.println("")

        self.printer.printInstr(Riscv.NativeReturn())
        self.printer.println("")
