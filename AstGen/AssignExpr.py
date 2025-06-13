from Scanner.SourcePos import *
from AstGen.Expr import *

class AssignExpr(Expr):
    def __init__(self, lAST, rAST, pos):
        super().__init__(pos)
        self.lAST = lAST
        self.rAST = rAST
    
    def accept(self, v):
        v.visit(self)
