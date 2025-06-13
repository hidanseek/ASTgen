from Scanner.SourcePos import *
from abc import ABC, abstractmethod
from AstGen.Terminal import *

class ID(Terminal):
    def __init__(self, Lexeme, pos):
        super().__init__(pos)
        self.Lexeme = Lexeme
    
    def accept(self, v):
        v.visit(self)
