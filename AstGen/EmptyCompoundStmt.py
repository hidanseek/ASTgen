from Scanner.SourcePos import *
from AstGen.CompoundStmt import *

class EmptyCompoundStmt(CompoundStmt):
    def __init__(self, pos):
        super().__init__(None, None, pos)
    
    def accept(self, v):
        v.visit(self)
