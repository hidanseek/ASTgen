from Scanner.SourcePos import *
from AstGen.Decl import *

class EmptyDecl(Decl):
    def __init__(self, pos):
        super().__init__(pos)
    
    def accept(self, v):
        v.visit(self)
