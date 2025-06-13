from Scanner.SourcePos import *
from abc import ABC, abstractmethod
from AstGen.Expr import *

class EmptyActualParam(Expr):
    def __init__(self, pos):
        super().__init__(pos)
    
    def accept(self, v):
        v.visit(self)
