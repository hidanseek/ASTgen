from Scanner.SourcePos import *
from AstGen.Stmt import *

class CallStmt(Stmt):
    def __init__(self, eAST, pos):
        super().__init__(pos)
        self.eAST = eAST
    
    def accept(self, v):
        v.visit(self)
