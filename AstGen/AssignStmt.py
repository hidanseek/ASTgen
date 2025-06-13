from Scanner.SourcePos import *
from AstGen.Stmt import *

class AssignStmt(Stmt):
    def __init__(self, lAST, rAST, pos):
        super().__init__(pos)
        self.lAST = lAST
        self.rAST = rAST
    
    def accept(self, v):
        v.visit(self)
