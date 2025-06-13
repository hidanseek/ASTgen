from Scanner.SourcePos import *
from AstGen.Expr import *

class IntExpr(Expr):
    def __init__(self, astIL, pos):
        super().__init__(pos)
        self.astIL = astIL
    
    def accept(self, v):
        v.visit(self)
