from Scanner.SourcePos import *
from AstGen.Expr import *

class BinaryExpr(Expr):
    def __init__(self, lAST, oAST, rAST, pos):
        super().__init__(pos)
        self.lAST = lAST
        self.rAST = rAST
        self.oAST = oAST
    
    def accept(self, v):
        v.visit(self)
