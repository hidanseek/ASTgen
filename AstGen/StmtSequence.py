from Scanner.SourcePos import *
from AstGen.Stmt import *

class StmtSequence(Stmt):
    def __init__(self, s1AST, s2AST, pos):
        super().__init__(pos)
        self.s1AST = s1AST
        self.s2AST = s2AST
    
    def accept(self, v):
        v.visit(self)
