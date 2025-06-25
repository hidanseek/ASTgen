from Scanner.SourcePos import *
from Scanner.Token import *
from Scanner.Scanner import *
from Parser.SyntaxError import *
from ErrorReporter import *

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

OPERATOR_PRECEDENCE = {
    '*': 2, '/': 2,
    '+': 3, '-': 3,
    '<': 4, '<=': 4, '>': 4, '>=': 4,
    '==': 5, '!=': 5,
    '&&': 6,
    '||': 7,
    '=': 8
}
RIGHT_ASSOCIATIVE = {'='}

class Parser:
    def __init__(self, scanner, reporter):
        self.scanner = scanner
        self.errorReporter = reporter
        self.currentToken = 0
        self.previousTokenPosition = 0

    def accept(self, tokenExpected):
        if self.currentToken.kind == tokenExpected:
            self.previousTokenPosition = self.currentToken.GetSourcePos()
            self.currentToken = self.scanner.scan()
        else:
            self.syntaxError("% expected here", Token.spell(tokenExpected))

    def acceptIt(self):
        self.previousTokenPosition = self.currentToken.GetSourcePos()
        self.currentToken = self.scanner.scan()

    def start(self, pos):
        pos.StartCol = self.currentToken.GetSourcePos().StartCol
        pos.StartLine = self.currentToken.GetSourcePos().StartLine

    def finish(self, pos):
        pos.EndCol = self.previousTokenPosition.EndCol
        pos.EndLine = self.previousTokenPosition.EndLine

    def syntaxError(self, messageTemplate, tokenQuoted):
        pos = self.currentToken.GetSourcePos()
        self.errorReporter.reportError(messageTemplate, tokenQuoted, pos)
        raise SyntaxError()

    def parse(self):
        self.previousTokenPosition = SourcePos()
        self.previousTokenPosition.StartLine = 0
        self.previousTokenPosition.StartCol = 0
        self.previousTokenPosition.EndLine = 0
        self.previousTokenPosition.EndCol = 0

        self.currentToken = self.scanner.scan()

        try:
            ProgramAST = self.parseProgram()
            if self.currentToken.kind != Token.EOF:
                self.syntaxError('% not expected after end of program', self.currentToken.GetLexeme())
        except SyntaxError:
            return None

        return ProgramAST

    def parseProgram(self):
        pos = SourcePos()
        self.start(pos)
        decls = self.parseDeclSequence()
        self.finish(pos)
        return Program(decls, pos)

    def parseDeclSequence(self):
        pos = SourcePos()
        self.start(pos)
        if self.currentToken.kind in (Token.INT, Token.FLOAT, Token.BOOL, Token.VOID):
            T = self.parseTypeSpecifier()
            decls = self.parseInitDecl(T)
            self.finish(pos)
            return decls
        else:
            self.finish(pos)
            return EmptyDecl(pos)

    def parseTypeSpecifier(self):
        pos = self.currentToken.GetSourcePos()
        if self.currentToken.kind == Token.INT:
            self.acceptIt()
            return IntType(pos)
        elif self.currentToken.kind == Token.FLOAT:
            self.acceptIt()
            return FloatType(pos)
        elif self.currentToken.kind == Token.BOOL:
            self.acceptIt()
            return BoolType(pos)
        elif self.currentToken.kind == Token.VOID:
            self.acceptIt()
            return VoidType(pos)
        else:
            self.syntaxError("Type specifier expected", self.currentToken.GetLexeme())
            return ErrorType(pos)

    def parseInitDecl(self, T):
        pos = SourcePos()
        Ident = self.parseID()
        E = EmptyExpr(pos)
        if self.currentToken.kind == Token.ASSIGN:
            self.acceptIt()
            E = self.parseInitializer()
        D = VarDecl(T, Ident, E, pos)
        self.accept(Token.SEMICOLON)
        if self.currentToken.kind in (Token.INT, Token.FLOAT, Token.BOOL, Token.VOID):
            rest = self.parseDeclSequence()
            return DeclSequence(D, rest, pos)
        else:
            return DeclSequence(D, EmptyDecl(pos), pos)

    def parseInitializer(self):
        return self.parseExpr()

    def parseID(self):
        Ident = ID(self.currentToken.GetLexeme(), self.currentToken.GetSourcePos())
        self.accept(Token.ID)
        return Ident

    def parseExpr(self):
        return self.parseAssignExpr()

    def parseAssignExpr(self):
        left = self.parseBinaryExpr(1)
        if self.currentToken.kind == Token.ASSIGN:
            pos = self.currentToken.GetSourcePos()
            op = Operator(self.currentToken.GetLexeme(), pos)
            self.acceptIt()
            right = self.parseAssignExpr()
            return AssignExpr(left, right, pos)
        return left

    def parseBinaryExpr(self, min_prec):
        left = self.parseUnaryExpr()
        while True:
            kind = self.currentToken.kind
            lexeme = self.currentToken.GetLexeme()
            if kind != Token.OPERATOR:
                break
            precedence = OPERATOR_PRECEDENCE.get(lexeme, 100)
            if precedence < min_prec:
                break
            assoc = 'RIGHT' if lexeme in RIGHT_ASSOCIATIVE else 'LEFT'
            next_min_prec = precedence + 1 if assoc == 'LEFT' else precedence
            op_pos = self.currentToken.GetSourcePos()
            op = Operator(lexeme, op_pos)
            self.acceptIt()
            right = self.parseBinaryExpr(next_min_prec)
            left = BinaryExpr(left, op, right, op_pos)
        return left

    def parseUnaryExpr(self):
        return self.parsePrimaryExpr()

    def parsePrimaryExpr(self):
        pos = self.currentToken.GetSourcePos()
        kind = self.currentToken.kind
        if kind == Token.ID:
            ident = self.parseID()
            if self.currentToken.kind == Token.LEFTPAREN:
                actualParams = self.parseArgList()
                return CallExpr(ident, actualParams, pos)
            elif self.currentToken.kind == Token.LEFTBRACKET:
                self.acceptIt()
                indexExpr = self.parseExpr()
                self.accept(Token.RIGHTBRACKET)
                return ArrayExpr(ident, indexExpr, pos)
            else:
                return VarExpr(ident, pos)
        elif kind == Token.INTLITERAL:
            lit = IntLiteral(self.currentToken.GetLexeme(), pos)
            self.acceptIt()
            return IntExpr(lit, pos)
        elif kind == Token.FLOATLITERAL:
            lit = FloatLiteral(self.currentToken.GetLexeme(), pos)
            self.acceptIt()
            return FloatExpr(lit, pos)
        elif kind == Token.BOOLLITERAL:
            lit = BoolLiteral(self.currentToken.GetLexeme(), pos)
            self.acceptIt()
            return BoolExpr(lit, pos)
        elif kind == Token.STRINGLITERAL:
            lit = StringLiteral(self.currentToken.GetLexeme(), pos)
            self.acceptIt()
            return StringExpr(lit, pos)
        elif kind == Token.LEFTPAREN:
            self.acceptIt()
            expr = self.parseExpr()
            self.accept(Token.RIGHTPAREN)
            return expr
        else:
            self.syntaxError("Primary expression expected", self.currentToken.GetLexeme())
            return EmptyExpr(pos)

    def parseCompoundStmt(self):
        pos = SourcePos()
        self.start(pos)
        self.accept(Token.LEFTBRACE)
        decls = EmptyDecl(self.previousTokenPosition)
        stmts = EmptyStmt(self.previousTokenPosition)
        self.accept(Token.RIGHTBRACE)
        self.finish(pos)
        return CompoundStmt(decls, stmts, pos)

    def parseArgList(self):
        self.accept(Token.LEFTPAREN)
        params = self.parseArgs()
        self.accept(Token.RIGHTPAREN)
        return params

    def parseArgs(self):
        if self.currentToken.kind == Token.RIGHTPAREN:
            return EmptyActualParam(self.previousTokenPosition)
        arg = ActualParam(self.parseExpr(), self.previousTokenPosition)
        if self.currentToken.kind == Token.COMMA:
            self.acceptIt()
            return ActualParamSequence(arg, self.parseArgs(), self.previousTokenPosition)
        return ActualParamSequence(arg, EmptyActualParam(self.previousTokenPosition), self.previousTokenPosition)

    def parseStmt(self):
        pos = SourcePos()
        self.start(pos)
        self.syntaxError("Statement expected", self.currentToken.GetLexeme())
        return EmptyStmt(pos)
