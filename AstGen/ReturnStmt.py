from Scanner.SourcePos import *
from AstGen.Stmt import *

class ReturnStmt(Stmt):
    def __init__(self, eAST, pos):
        super().__init__(pos)
        self.eAST = eAST
    
    def accept(self, v):
        v.visit(self)
