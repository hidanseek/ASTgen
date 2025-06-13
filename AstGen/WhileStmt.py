from Scanner.SourcePos import *
from AstGen.Stmt import *

class WhileStmt(Stmt):
    def __init__(self, eAST, stmtAST, pos):
        super().__init__(pos)
        self.eAST = eAST
        self.stmtAST = stmtAST
    
    def accept(self, v):
        v.visit(self)
