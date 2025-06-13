from Scanner.SourcePos import *
from AstGen.AST import *

class Decl(AST):
    def __init__(self, pos):
        super().__init__(pos)
