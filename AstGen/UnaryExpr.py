from Scanner.SourcePos import *
from AstGen.Expr import *

class UnaryExpr(Expr):
    def __init__(self, oAST, eAST, pos):
        super().__init__(pos)
        self.oAST = oAST
        self.eAST = eAST
    
    def accept(self, v):
        v.visit(self)
