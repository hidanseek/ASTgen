from Scanner.SourcePos import *
from AstGen.Expr import *

class BoolExpr(Expr):
    def __init__(self, astBL, pos):
        super().__init__(pos)
        self.astBL = astBL
    
    def accept(self, v):
        v.visit(self)
