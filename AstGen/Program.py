from Scanner.SourcePos import *
from AstGen.AST import *

class Program(AST):
    def __init__(self, D, pos):
        super().__init__(pos)
        self.D = D

    def accept(self, v):
        v.visit(self)
