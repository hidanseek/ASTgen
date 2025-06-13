from Scanner.SourcePos import *
from AstGen.Decl import *

class FunDecl(Decl):
    def __init__(self, tAST, idAST, paramsAST, stmtAST, pos):
        super().__init__(pos)
        self.tAST = tAST
        self.idAST = idAST
        self.paramsAST = paramsAST
        self.stmtAST = stmtAST
    
    def accept(self, v):
        v.visit(self)
