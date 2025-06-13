from Scanner.SourcePos import *
from AstGen.Expr import *

class VarExpr(Expr):
    def __init__(self, Ident, pos):
        super().__init__(pos)
        self.Ident = Ident
    
    def accept(self, v):
        v.visit(self)
