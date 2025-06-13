from Scanner.SourcePos import *
from AstGen.Stmt import *

class EmptyStmt(Stmt):
    def __init__(self, pos):
        super().__init__(pos)
    
    def accept(self, v):
        v.visit(self)
