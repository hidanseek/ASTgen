from Scanner.SourcePos import *
from AstGen.Decl import *

class VarDecl(Decl):
    def __init__(self, tAST, idAST, eAST, pos):
        super().__init__(pos)
        self.tAST = tAST
        self.idAST = idAST
        self.eAST = eAST
    
    def accept(self, v):
        v.visit(self)
