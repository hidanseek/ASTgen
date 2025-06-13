from Scanner.SourcePos import *
from AstGen.Expr import *

class CallExpr(Expr):
    def __init__(self, idAST, paramAST, pos):
        super().__init__(pos)
        self.idAST = idAST
        self.paramAST = paramAST
    
    def accept(self, v):
        v.visit(self)
