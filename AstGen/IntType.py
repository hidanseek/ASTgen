from Scanner.SourcePos import *
from AstGen.Type import *

class IntType(Type):
    def __init__(self, pos):
        super().__init__(pos)
    
    def accept(self, v):
        v.visit(self)
