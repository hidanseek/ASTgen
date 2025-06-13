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

class UnparseVisitor(Visitor):

    INDENT_LEVEL = 3    # amount of indentation per level

    def __init__(self, out):
        self.out = out
        self.indent = 0
        self.IsGlobal = True
        self.IsFirst = True
    
    def newline(self):
        self.write('\n')
        for i in range(1, self.indent*UnparseVisitor.INDENT_LEVEL + 1):
            self.write(' ')
    
    def write(self, s):
        try:
            self.out.write(s)
        except Exception as e:
            print("Error: ", e, sep='')
            exit(1)

    @singledispatchmethod
    def visit(self, x):
        return super().visit(x)

    @visit.register
    def _(self, x: Program):
        if not (type(x.D) is EmptyDecl):
            x.D.accept(self)
        self.write('\n')
    
    @visit.register
    def _(self, x: EmptyDecl):
        assert (False)
    
    @visit.register
    def _(self, x: FunDecl):
        if not self.IsFirst:
            self.newline()
            self.newline()
        self.IsFirst = False
        self.IsGlobal = False
        x.tAST.accept(self)
        self.write(' ')
        x.idAST.accept(self)
        self.write('(')
        if type(x.paramsAST) is FormalParamDeclSequence:
            x.paramsAST.accept(self)
        self.write(')')
        x.stmtAST.accept(self)
        self.IsGlobal = True
    
    @visit.register
    def _(self, x: FormalParamDecl):
        if isinstance(x.astType, ArrayType):
            (x.astType).astType.accept(self)
            self.write(' ')
            x.astIdent.accept(self)
            self.write('[')
            (x.astType).astExpr.accept(self)
            self.write(']')
        else:
            x.astType.accept(self)
            self.write(' ')
            x.astIdent.accept(self)
    
    @visit.register
    def _(self, x: FormalParamDeclSequence):
        x.lAST.accept(self)
        if type(x.rAST) is FormalParamDeclSequence:
            self.write(', ')
            x.rAST.accept(self)
    
    @visit.register
    def _(self, x: EmptyFormalParamDecl):
        assert False
    
    @visit.register
    def _(self, x: StmtSequence):
        x.s1AST.accept(self)
        if type(x.s2AST) is StmtSequence:
            x.s2AST.accept(self)
    
    @visit.register
    def _(self, x: AssignStmt):
        self.newline()
        x.lAST.accept(self)
        self.write(' = ')
        x.rAST.accept(self)
        self.write(';')

    @visit.register
    def _(self, x: IfStmt):
        self.newline()
        self.write('if (')
        x.eAST.accept(self)
        self.write(')')
        if not isinstance(x.thenAST, CompoundStmt):
            self.indent+=1
        x.thenAST.accept(self)
        if not isinstance(x.thenAST, CompoundStmt):
            self.indent-=1
        if x.elseAST != None:
            self.newline()
            self.write('else')
            if not isinstance(x.elseAST, CompoundStmt):
                self.indent+=1
            x.elseAST.accept(self)
            if not isinstance(x.elseAST, CompoundStmt):
                self.indent-=1
    
    @visit.register
    def _(self, x: WhileStmt):
        self.newline()
        self.write('while (')
        x.eAST.accept(self)
        self.write(')')
        x.stmtAST.accept(self)
    
    @visit.register
    def _(self, x: ForStmt):
        self.newline()
        self.write('for (')
        if not(type(x.e1AST) is EmptyExpr):
            x.e1AST.accept(self)
        self.write(';')
        if not isinstance(x.e2AST, EmptyExpr):
            self.write(' ')
            x.e2AST.accept(self)
        self.write(';')
        if not isinstance(x.e3AST, EmptyExpr):
            self.write(' ')
            x.e3AST.accept(self)
        self.write(')')
        x.stmtAST.accept(self)
    
    @visit.register
    def _(self, x: ReturnStmt):
        self.newline()
        self.write('return')
        if not isinstance(x.eAST, EmptyExpr):
            self.write(' ')
            x.eAST.accept(self)
        self.write(';')
    
    @visit.register
    def _(self, x: CompoundStmt):
        self.newline()
        self.write('{')
        self.indent+=1
        if type(x.astDecl) is DeclSequence:
            x.astDecl.accept(self)
        if type(x.astStmt) is StmtSequence:
            x.astStmt.accept(self)
        self.indent-=1
        self.newline()
        self.write('}')
    
    @visit.register
    def _(self, x: EmptyCompoundStmt):
        self.newline()
        self.write('{}')
    
    @visit.register
    def _(self, x: EmptyStmt):
        assert False
    
    @visit.register
    def _(self, x: CallStmt):
        self.newline()
        x.eAST.accept(self)
        self.write(';')
    
    @visit.register
    def _(self, x: VarDecl):
        if self.IsGlobal and not self.IsFirst:
            self.newline()
            self.newline()
        elif not self.IsGlobal and not self.IsFirst:
            self.newline()
        self.IsFirst = False
        if isinstance(x.tAST, ArrayType):
            (x.tAST).astType.accept(self)
            self.write(' ')
            x.idAST.accept(self)
            self.write('[')
            (x.tAST).astExpr.accept(self)
            self.write(']')
            if type(x.eAST) is ExprSequence:
                self.write(' = { ')
                x.eAST.accept(self)
                self.write(' }')
            elif not(type(x.eAST) is EmptyExpr):
                self.write(' = ')
                x.eAST.accept(self)
        else:
            x.tAST.accept(self)
            self.write(' ')
            x.idAST.accept(self)
            if type(x.eAST) is ExprSequence:
                self.write(' = { ')
                x.eAST.accept(self)
                self.write(' }')
            elif not (type(x.eAST) is EmptyExpr):
                self.write(' = ')
                x.eAST.accept(self)
        self.write(';')
    
    @visit.register
    def _(self, x: DeclSequence):
        x.D1.accept(self)
        if type(x.D2) is DeclSequence:
            x.D2.accept(self)
    
    @visit.register
    def _(self, x: VarExpr):
        x.Ident.accept(self)
    
    @visit.register
    def _(self, x: AssignExpr):
        x.lAST.accept(self)
        self.write(' = ')
        x.rAST.accept(self)
    
    @visit.register
    def _(self, x: IntExpr):
        x.astIL.accept(self)
    
    @visit.register
    def _(self, x: FloatExpr):
        x.astFL.accept(self)
    
    @visit.register
    def _(self, x: BoolExpr):
        x.astBL.accept(self)

    @visit.register
    def _(self, x: StringExpr):
        x.astSL.accept(self)
    
    @visit.register
    def _(self, x: ArrayExpr):
        x.idAST.accept(self)
        self.write('[')
        x.indexAST.accept(self)
        self.write(']')
    
    @visit.register
    def _(self, x: BinaryExpr):
        self.write('(')
        x.lAST.accept(self)
        self.write(' ')
        x.oAST.accept(self)
        self.write(' ')
        x.rAST.accept(self)
        self.write(')')
    
    @visit.register
    def _(self, x: UnaryExpr):
        self.write('(')
        x.oAST.accept(self)
        x.eAST.accept(self)
        self.write(')')
    
    @visit.register
    def _(self, x: EmptyExpr):
        assert False
    
    @visit.register
    def _(self, x: ActualParam):
        x.pAST.accept(self)

    @visit.register
    def _(self, x: EmptyActualParam):
        assert False
    
    @visit.register
    def _(self, x: ActualParamSequence):
        x.lAST.accept(self)
        if type(x.rAST) is ActualParamSequence:
            self.write(', ')
            x.rAST.accept(self)
    
    @visit.register
    def _(self, x: CallExpr):
        x.idAST.accept(self)
        self.write('(')
        if type(x.paramAST) is ActualParamSequence:
            x.paramAST.accept(self)
        self.write(')')
    
    @visit.register
    def _(self, x: ExprSequence):
        x.lAST.accept(self)
        if type(x.rAST) is ExprSequence:
            self.write(', ')
            x.rAST.accept(self)
    
    @visit.register
    def _(self, x: ID):
        self.write(x.Lexeme)
    
    @visit.register
    def _(self, x: Operator):
        self.write(x.Lexeme)

    @visit.register
    def _(self, x: IntLiteral):
        self.write(x.Lexeme)

    @visit.register
    def _(self, x: FloatLiteral):
        self.write(x.Lexeme)

    @visit.register
    def _(self, x: BoolLiteral):
        self.write(x.Lexeme)

    @visit.register
    def _(self, x: StringLiteral):
        self.write('"' + x.Lexeme + '"')
    
    @visit.register
    def _(self, x: IntType):
        self.write('int')
    
    @visit.register
    def _(self, x: FloatType):
        self.write('float')
    
    @visit.register
    def _(self, x: BoolType):
        self.write('bool')
    
    @visit.register
    def _(self, x: StringType):
        assert False
    
    @visit.register
    def _(self, x: VoidType):
        self.write('void')
    
    @visit.register
    def _(self, x: ArrayType):
        assert False    #never called directly.
    
    @visit.register
    def _(self, x: ErrorType):
        self.write('ErrorType')
