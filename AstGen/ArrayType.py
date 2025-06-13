from Scanner.SourcePos import *
from AstGen.Type import *

class ArrayType(Type):
    def __init__(self, astType, astExpr, pos):
        super().__init__(pos)
        self.astType = astType
        self.astExpr = astExpr
    
    def accept(self, v):
        v.visit(self)
