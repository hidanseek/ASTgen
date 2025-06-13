from functools import singledispatchmethod
from AstGen.Program import Program

from AstGen.EmptyDecl import EmptyDecl
from AstGen.FunDecl import FunDecl

from AstGen.VarDecl import VarDecl
from AstGen.FormalParamDecl import FormalParamDecl
from AstGen.FormalParamDeclSequence import FormalParamDeclSequence
from AstGen.EmptyFormalParamDecl import EmptyFormalParamDecl
from AstGen.DeclSequence import DeclSequence

from AstGen.AssignStmt import AssignStmt
from AstGen.IfStmt import IfStmt
from AstGen.WhileStmt import WhileStmt
from AstGen.ForStmt import ForStmt
from AstGen.ReturnStmt import ReturnStmt
from AstGen.CompoundStmt import CompoundStmt
from AstGen.EmptyCompoundStmt import EmptyCompoundStmt
from AstGen.EmptyStmt import EmptyStmt
from AstGen.StmtSequence import StmtSequence
from AstGen.CallStmt import CallStmt

from AstGen.VarExpr import VarExpr
from AstGen.AssignExpr import AssignExpr
from AstGen.IntExpr import IntExpr
from AstGen.FloatExpr import FloatExpr
from AstGen.BoolExpr import BoolExpr
from AstGen.ArrayExpr import ArrayExpr
from AstGen.StringExpr import StringExpr
from AstGen.BinaryExpr import BinaryExpr
from AstGen.UnaryExpr import UnaryExpr
from AstGen.EmptyExpr import EmptyExpr
from AstGen.ActualParam import ActualParam
from AstGen.EmptyActualParam import EmptyActualParam
from AstGen.ActualParamSequence import ActualParamSequence
from AstGen.CallExpr import CallExpr
from AstGen.ExprSequence import ExprSequence
from AstGen.ID import ID
from AstGen.Operator import Operator
from AstGen.IntLiteral import IntLiteral
from AstGen.FloatLiteral import FloatLiteral
from AstGen.BoolLiteral import BoolLiteral
from AstGen.StringLiteral import StringLiteral
from AstGen.IntType import IntType
from AstGen.FloatType import FloatType
from AstGen.BoolType import BoolType
from AstGen.VoidType import VoidType
from AstGen.StringType import StringType
from AstGen.ArrayType import ArrayType
from AstGen.ErrorType import ErrorType

from AstGen.Visitor import Visitor

from Scanner.SourcePos import *

class TreePrinterVisitor(Visitor):
    visit = Visitor.visit

    def __init__(self, out):
        self.out = out
        self.indent = 0
        self.INDENT_LEVEL = 3   #amount of indentation per level
        self.draw_pos = False
    
    def write(self, s):
        try:
            for i in range(1, self.indent * self.INDENT_LEVEL+1):
                self.out.write(" ")
            self.out.write(s)
        except Exception as e:
            print("Error: ", e, sep='')
            exit(1)
    
    def FormatPosition(self, n):
        if self.draw_pos:
            pos = n.getPosition()
            return " " \
                + str(pos.StartLine) + "(" + str(pos.StartCol) + ").." \
                + str(pos.EndLine) + "(" + str(pos.EndCol) + ")" + "\n"
        else:
            return "\n"
    
    @singledispatchmethod
    def visit(self, x):
        return super().visit(x)

    @visit.register
    def _(self, x: Program):
        self.write("Program" + self.FormatPosition(x))
        self.indent+=1
        x.D.accept(self)
    
    @visit.register
    def _(self, x: EmptyDecl):
        self.write("EmptyDecl\n")
    
    @visit.register
    def _(self, x: FunDecl):
        self.write("FunDecl" + self.FormatPosition(x))
        self.indent+=1
        x.tAST.accept(self)
        x.idAST.accept(self)
        x.paramsAST.accept(self)
        x.stmtAST.accept(self)
        self.indent-=1

    @visit.register
    def _(self, x: FormalParamDecl):
        self.write("FormalParamDecl" + self.FormatPosition(x))
        self.indent+=1
        x.astType.accept(self)
        x.astIdent.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: FormalParamDeclSequence):
        self.write("FormalParamDeclSequence\n")
        self.indent+=1
        x.lAST.accept(self)
        x.rAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: EmptyFormalParamDecl):
        self.write("EmptyFormalParamDecl\n")
        
    @visit.register
    def _(self, x: StmtSequence):
        self.write("StmtSequence\n")
        self.indent+=1
        x.s1AST.accept(self)
        x.s2AST.accept(self)
        self.indent-=1

    @visit.register
    def _(self, x: AssignStmt):
        self.write("AssignStmt" + self.FormatPosition(x))
        self.indent+=1
        x.lAST.accept(self)
        x.rAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: IfStmt):
        self.write("IfStmt" + self.FormatPosition(x))
        self.indent+=1
        x.eAST.accept(self)
        x.thenAST.accept(self)
        if x.elseAST is not None:
            x.elseAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: WhileStmt):
        self.write("WhileStmt" + self.FormatPosition(x))
        self.indent+=1
        x.eAST.accept(self)
        x.stmtAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: ForStmt):
        self.write("ForStmt" + self.FormatPosition(x))
        self.indent+=1
        x.e1AST.accept(self)
        x.e2AST.accept(self)
        x.e3AST.accept(self)
        x.stmtAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: ReturnStmt):
        self.write("ReturnStmt" + self.FormatPosition(x))
        self.indent+=1
        x.eAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: CompoundStmt):
        self.write("CompoundStmt" + self.FormatPosition(x))
        self.indent+=1
        x.astDecl.accept(self)
        x.astStmt.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: EmptyCompoundStmt):
        self.write("EmptyCompoundStmt\n")
    
    @visit.register
    def _(self, x: EmptyCompoundStmt):
        self.write("EmptyCompoundStmt\n")
    
    @visit.register
    def _(self, x: EmptyStmt):
        self.write("EmptyStmt\n")
    
    @visit.register
    def _(self, x: CallStmt):
        self.write("CallStmt" + self.FormatPosition(x))
        self.indent+=1
        x.eAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: VarDecl):
        self.write("VarDecl" + self.FormatPosition(x))
        self.indent+=1
        x.tAST.accept(self)
        x.idAST.accept(self)
        x.eAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: DeclSequence):
        self.write("DeclSequence\n")
        self.indent+=1
        x.D1.accept(self)
        x.D2.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: VarExpr):
        self.write("VarExpr" + self.FormatPosition(x))
        self.indent+=1
        x.Ident.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: AssignExpr):
        self.write("AssignExpr" + self.FormatPosition(x))
        self.indent+=1
        x.lAST.accept(self)
        x.rAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: IntExpr):
        self.write("IntExpr" + self.FormatPosition(x))
        self.indent+=1
        x.astIL.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: FloatExpr):
        self.write("FloatExpr" + self.FormatPosition(x))
        self.indent+=1
        x.astFL.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: BoolExpr):
        self.write("BoolExpr" + self.FormatPosition(x))
        self.indent+=1
        x.astBL.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: StringExpr):
        self.write("StringExpr" + self.FormatPosition(x))
        self.indent+=1
        x.astSL.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: ArrayExpr):
        self.write("ArrayExpr" + self.FormatPosition(x))
        self.indent+=1
        x.idAST.accept(self)
        x.indexAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: BinaryExpr):
        self.write("BinaryExpr" + self.FormatPosition(x))
        self.indent+=1
        x.lAST.accept(self)
        x.oAST.accept(self)
        x.rAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: UnaryExpr):
        self.write("UnaryExpr" + self.FormatPosition(x))
        self.indent+=1
        x.oAST.accept(self)
        x.eAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: EmptyExpr):
        self.write("EmptyExpr\n")
    
    @visit.register
    def _(self, x: ActualParam):
        self.write("ActualParam" + self.FormatPosition(x))
        self.indent+=1
        x.pAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: EmptyActualParam):
        self.write("EmptyActualParam\n")
    
    @visit.register
    def _(self, x: ActualParamSequence):
        self.write("ActualParamSequence\n")
        self.indent+=1
        x.lAST.accept(self)
        x.rAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: CallExpr):
        self.write("CallExpr" + self.FormatPosition(x))
        self.indent+=1
        x.idAST.accept(self)
        x.paramAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: ExprSequence):
        self.write("ExprSequence\n")
        self.indent+=1
        x.lAST.accept(self)
        x.rAST.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: ID):
        self.write("ID: " + x.Lexeme + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: Operator):
        self.write("Operator: " + x.Lexeme + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: IntLiteral):
        self.write("IntLiteral: " + x.Lexeme + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: FloatLiteral):
        self.write("FloatLiteral: " + x.Lexeme + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: BoolLiteral):
        self.write("BoolLiteral: " + x.Lexeme + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: IntType):
        self.write("IntType" + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: FloatType):
        self.write("FloatType" + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: BoolType):
        self.write("BoolType" + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: StringType):
        self.write("StringType" + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: VoidType):
        self.write("VoidType" + self.FormatPosition(x))
    
    @visit.register
    def _(self, x: ArrayType):
        self.write("ArrayType" + self.FormatPosition(x))
        self.indent+=1
        x.astType.accept(self)
        x.astExpr.accept(self)
        self.indent-=1
    
    @visit.register
    def _(self, x: ErrorType):
        self.write("ErrorType" + self.FormatPosition(x))
    