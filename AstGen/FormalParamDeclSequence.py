from Scanner.SourcePos import *
from AstGen.Decl import *

class FormalParamDeclSequence(Decl):
    def __init__(self, lAST, rAST, pos):
        super().__init__(pos)
        self.lAST = lAST
        self.rAST = rAST
    
    def accept(self, v):
        v.visit(self)
