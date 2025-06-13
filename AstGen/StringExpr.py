from Scanner.SourcePos import *
from AstGen.Expr import *

class StringExpr(Expr):
    def __init__(self, astSL, pos):
        super().__init__(pos)
        self.astSL = astSL
    
    def accept(self, v):
        v.visit(self)
