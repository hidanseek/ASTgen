from Scanner.SourcePos import *
from AstGen.Expr import *

class ArrayExpr(Expr):
    def __init__(self, idAST, indexAST, pos):
        super().__init__(pos)
        self.idAST = idAST
        self.indexAST = indexAST
    
    def accept(self, v):
        v.visit(self)
