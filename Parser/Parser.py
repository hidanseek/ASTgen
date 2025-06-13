from Scanner.SourcePos import *
from Scanner.Token import *
from Scanner.SourcePos import *
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

from AstGen.Visitor import *

class Parser:
    def __init__(self, lexer, reporter):
        self.scanner = lexer
        self.errorReporter = reporter
        self.currentToken = 0
        self.previousTokenPosition = 0

    # accept() checks whether the current token matches tokenExpected.
    # If so, it fetches the next token
    # If not, it reports a syntax error.
    def accept(self, tokenExpected):
        if self.currentToken.kind == tokenExpected:
            self.previousTokenPosition = self.currentToken.GetSourcePos()
            self.currentToken = self.scanner.scan()
        else:
            self.syntaxError("% expected here", Token.spell(tokenExpected))
    
    # acceptIt() unconditionally accepts the current token
    # and fetches the next token from the scanner.
    def acceptIt(self):
        self.previousTokenPosition = self.currentToken.GetSourcePos()
        self.currentToken = self.scanner.scan()

    # start records the position of the start of a phrase.
    # This is defined to be the position of the first
    # character of the first token of the phrase.
    def start(self, pos):
        pos.StartCol = self.currentToken.GetSourcePos().StartCol
        pos.StartLine = self.currentToken.GetSourcePos().StartLine

    # finish records the position of the end of a phrase.
    # This is defined to be the position of the last
    # character of the last token of the phrase.  
    def finish(self, pos):
        pos.EndCol = self.previousTokenPosition.EndCol
        pos.EndLine = self.previousTokenPosition.EndLine
    
    def syntaxError(self, messageTemplate, tokenQuoted):
        pos = self.currentToken.GetSourcePos()
        self.errorReporter.reportError(messageTemplate, tokenQuoted, pos)
        raise SyntaxError()
    
    @staticmethod
    def isTypeSpecifier(token):
        if token in (Token.VOID, Token.INT, Token.BOOL, Token.FLOAT):
            return True
        else:
            return False
    
    '''
        parseArrayIndexDecl (Type T):

        Take [INTLITERAL] and generate an ArrayType

    '''
    def parseArrayIndexDecl(self, T):
        self.accept(Token.LEFTBRACKET)
        pos = self.currentToken.GetSourcePos()
        L = IntLiteral(self.currentToken.GetLexeme(), pos)
        self.accept(Token.INTLITERAL)
        self.accept(Token.RIGHTBRACKET)
        IE = IntExpr(L, pos)
        return ArrayType(T, IE, self.previousTokenPosition)
    
    # toplevel parse() routine:

    def parse(self):    # called from the MiniC driver
        ProgramAST = None

        self.previousTokenPosition = SourcePos()
        self.previousTokenPosition.StartLine = 0
        self.previousTokenPosition.StartCol = 0
        self.previousTokenPosition.EndLine = 0
        self.previousTokenPosition.EndCol = 0

        self.currentToken = self.scanner.scan() # get first token from scanner...

        try:
            ProgramAST = self.parseProgram()
            if self.currentToken.kind != Token.EOF:
                self.syntaxError('% not expected after end of program', self.currentToken.GetLexeme())
        except SyntaxError as s:
            return None

        return ProgramAST
    
    '''
        parseProgram():

        program ::= ( (VOID|INT|BOOL|FLOAT) Id ( FunPart | VarPart ) )* ";"

    '''

    # parseProgDecls: recursive helper function to facilitate AST construction.
    def parseProgDecls(self):
        if not Parser.isTypeSpecifier(self.currentToken.kind):
            return EmptyDecl(self.previousTokenPosition)
        pos = SourcePos()
        self.start(pos)
        T = self.parseTypeSpecifier()
        Ident = self.parseID()
        if self.currentToken.kind == Token.LEFTPAREN:
            newD = self.parseFunPart(T, Ident, pos)
            return DeclSequence(newD, self.parseProgDecls(), self.previousTokenPosition)
        else:
            Vars = self.parseVarPart(T, Ident, pos)
            VarsTail = Vars.GetRightmostDeclSequenceNode()
            RemainderDecls = self.parseProgDecls()
            VarsTail.SetRightSubtree(RemainderDecls)
            return Vars
    
    def parseProgram(self):
        pos = SourcePos()
        self.start(pos)
        D = self.parseProgDecls()
        self.finish(pos)
        P = Program(D, pos)
        return P
    
    '''

        parseFunPart():
    
        FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
    
    '''

    def parseFunPart(self, T, Ident, pos):
        # We already know that the current token is "(".
        # Otherwise use accept() !
        self.acceptIt()
        PDecl = self.parseParamsList()  # can also be empty...
        self.accept(Token.RIGHTPAREN)
        CStmt = self.parseCompoundStmt()
        self.finish(pos)
        return FunDecl(T, Ident, PDecl, CStmt, pos)
    
    '''
        parseParamsList():
    
        ParamsList ::= ParameterDecl ( "," ParameterDecl )*
    '''
    def parseParamsList(self):
        if not Parser.isTypeSpecifier(self.currentToken.kind):
            return EmptyFormalParamDecl(self.previousTokenPosition)
        Decl_l = self.parseParameterDecl()
        Decl_r = EmptyFormalParamDecl(self.previousTokenPosition)
        if self.currentToken.kind == Token.COMMA:
            self.acceptIt()
            Decl_r = self.parseParamsList()
            if isinstance(Decl_r, EmptyFormalParamDecl):
                self.syntaxError("Declaration after comma expected", "")
        return FormalParamDeclSequence(Decl_l, Decl_r, self.previousTokenPosition)
    
    '''
        parseParameterDecl():
    
        ParameterDecl ::= (VOID | INT | BOOL | FLOAT) Declarator
    '''
    def parseParameterDecl(self):
        T = None
        D = None

        pos = SourcePos()
        self.start(pos)
        if Parser.isTypeSpecifier(self.currentToken.kind):
            T = self.parseTypeSpecifier()
        else:
            self.syntaxError('Type specifier instead of % expected', Token.spell(self.currentToken.kind))
        D = self.parseDeclarator(T, pos)
        return D
    
    '''
        parseDeclarator():

        Declarator ::= ID ( "[" INTLITERAL "]" )?
    '''
    def parseDeclarator(self, T, pos):
        Ident = self.parseID()
        if self.currentToken.kind == Token.LEFTBRACKET:
            ArrT = self.parseArrayIndexDecl(T, pos)
            self.finish(pos)
            return FormalParamDecl(ArrT, Ident, pos)
        self.finish(pos)
        return FormalParamDecl(T, Ident, pos)
    
    '''
        parseVarPart():
    
        VarPart ::= ( "[" INTLITERAL "]" )? ( "=" initializer ) ? ( "," init_decl )* ";"
    '''
    def parseVarPart(self, T, Ident):
        theType = T
        Seq = None
        E = EmptyExpr(self.previousTokenPosition)
        if self.currentToken.kind == Token.LEFTBRACKET:
            theType = self.parseArrayIndexDecl(T)
        if self.currentToken.kind == Token.ASSIGN:
            self.acceptIt()
            # You can use the following code after you have implemented
            # parseInitializer()
            # E = parseInitializer()

        D = VarDecl(theType, Ident, E, self.previousTokenPosition)
        # You can use the following code after implementation of parseInitDecl()
        '''
        if (self.currentToken.kind == Token.COMMA):
            self.acceptIt()
            Seq = DeclSequence(D, self.parseInitDecl(T), self.previousTokenPosition)
        else:
            Seq = DeclSequence(D, EmptyDecl(self.previousTokenPosition), self.previousTokenPosition)
        '''
        self.accept(Token.SEMICOLON)
        return Seq

    '''
        parseUnaryExpr():
    
        UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
    '''
    def parseUnaryExpr(self):
        if self.currentToken.kind in (Token.PLUS, Token.MINUS, Token.NOT):
            opAST = Operator(self.currentToken.GetLexeme(), self.previousTokenPosition)
            self.acceptIt()
            return UnaryExpr(opAST, self.parseUnaryExpr(), self.previousTokenPosition)
        return self.parsePrimaryExpr()
    
    '''
        parsePrimaryExpr():
        
        PrimaryExpr ::= ID arglist?
                        | ID "[" expr "]"
                        | "(" expr ")"
                        | INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
    '''
    def parsePrimaryExpr(self):
        retExpr = None

        # your code goes here...

        return retExpr

    '''
        parseCompoundStmt():

        CompoundStmt ::= "{" VariableDef* Stmt* "}"
    '''
    def parseCompoundDecls(self):
        if not Parser.isTypeSpecifier(self.currentToken.kind):
            return EmptyDecl(self.previousTokenPosition)
        T = self.parseTypeSpecifier()
        Ident = self.parseID()
        Vars = self.parseVarPart(T, Ident)
        VarsTail = Vars.GetRightmostDeclSequenceNode()
        RemainderDecls = self.parseCompoundDecls()
        VarsTail.SetRightSubtree(RemainderDecls)
        return Vars

    def parseCompoundStmts(self):
        if not self.currentToken.kind in (Token.LEFTBRACE, Token.IF, Token.WHILE, Token.FOR, Token.RETURN, Token.ID):
            return EmptyStmt(self.previousTokenPosition)
        S = None
        # You can use the following code after implementation of parseStmt()
        # S = parseStmt()
        return StmtSequence(S, self.parseCompoundStmts(), self.previousTokenPosition)
    
    def parseCompoundStmt(self):
        pos = SourcePos()
        self.start(pos)
        self.accept(Token.LEFTBRACE)
        D = self.parseCompoundDecls()
        S = self.parseCompoundStmts()
        self.accept(Token.RIGHTBRACE)
        self.finish(pos)
        if type(D) is EmptyDecl and type(S) is EmptyStmt:
            return EmptyCompoundStmt(self.previousTokenPosition)
        else:
            return CompoundStmt(D, S, pos)
    
    '''
        parseArgList():
        
        ArgList ::= "(" ( arg ( "," arg )* )? ")"
    '''

    def parseArgs(self):
        if self.currentToken.kind == Token.RIGHTPAREN:
            return EmptyActualParam(self.previousTokenPosition)
        Params = None
        '''
            You can use the following code after you have implemented parseExpr() aso.:

            Params = ActualParam(self.parseExpr(), self.previousTokenPosition)
            if self.currentToken.kind == Token.COMMA:
                self.acceptIt()
        '''
        return ActualParamSequence(Params, self.parseArgs(), self.previousTokenPosition)
    
    def parseArgList(self):
        self.accept(Token.LEFTPAREN)
        Params = self.parseArgs()
        self.accept(Token.RIGHTPAREN)
        return Params
    
    '''
        parseID():

        ID (terminal)
    '''
    def parseID(self):
        Ident = ID(self.currentToken.GetLexeme(), self.currentToken.GetSourcePos())
        self.accept(Token.ID)
        return Ident

    '''
        parseTypeSpecifier():

        VOID | INT | FLOAT | BOOL (all terminals)
    '''

    def parseTypeSpecifier(self):
        T = None
        match self.currentToken.kind:
            case Token.INT:
                T = IntType(self.currentToken.GetSourcePos())
            case Token.FLOAT:
                T = FloatType(self.currentToken.GetSourcePos())
            case Token.BOOL:
                T = BoolType(self.currentToken.GetSourcePos())
            case Token.VOID:
                T = VoidType(self.currentToken.GetSourcePos())
            case _:
                self.syntaxError("Type specifier expected", "")
        self.acceptIt()
        return T

