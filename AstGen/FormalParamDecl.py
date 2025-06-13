from Scanner.SourcePos import *
from AstGen.Decl import *

class FormalParamDecl(Decl):
    def __init__(self, astType, astIdent, pos):
        super().__init__(pos)
        self.astType = astType
        self.astIdent = astIdent
    
    def accept(self, v):
        v.visit(self)
