from Scanner.SourcePos import *
from AstGen.FormalParamDecl import *

class EmptyFormalParamDecl(FormalParamDecl):
    def __init__(self, pos):
        super().__init__(None, None, pos)
    
    def accept(self, v):
        v.visit(self)
