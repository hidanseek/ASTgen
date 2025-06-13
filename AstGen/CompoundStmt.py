from Scanner.SourcePos import *
from abc import ABC, abstractmethod
from AstGen.Stmt import *

class CompoundStmt(Stmt):
    def __init__(self, astDecl, astStmt, pos):
        super().__init__(pos)
        self.astDecl = astDecl
        self.astStmt = astStmt
    
    def accept(self, v):
        v.visit(self)
