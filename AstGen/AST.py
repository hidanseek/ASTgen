from Scanner.SourcePos import *
from abc import ABC, abstractmethod

class AST(ABC):
    def __init__(self, pos):
        self.pos = SourcePos()
        self.pos.StartCol = pos.StartCol
        self.pos.EndCol = pos.EndCol
        self.pos.StartLine = pos.StartLine
        self.pos.EndLine = pos.EndLine
    
    def getPosition(self):
        return self.pos
    
    @abstractmethod
    def accept(self, v):
        pass