from Scanner.SourcePos import *
from AstGen.Expr import *

class EmptyExpr(Expr):
    def __init__(self, pos):
        super().__init__(pos)
    
    def accept(self, v):
        v.visit(self)
