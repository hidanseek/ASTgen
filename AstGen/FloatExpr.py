from Scanner.SourcePos import *
from AstGen.Expr import *

class FloatExpr(Expr):
    def __init__(self, astFl, pos):
        super().__init__(pos)
        self.astFL = astFl
    
    def accept(self, v):
        v.visit(self)
