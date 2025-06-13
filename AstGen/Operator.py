from Scanner.SourcePos import *
from AstGen.Terminal import *

class Operator(Terminal):
    def __init__(self, Lexeme, pos):
        super().__init__(pos)
        self.Lexeme = Lexeme
    
    def accept(self, v):
        v.visit(self)