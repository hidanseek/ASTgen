from AstGen import *
from Scanner.SourcePos import *
from AstGen.Expr import *

class ActualParam(Expr):
    def __init__(self, pAST, pos):
        super().__init__(pos)
        self.pAST = pAST
    
    def accept(self, v):
        v.visit(self)
