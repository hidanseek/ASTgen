from Scanner.SourcePos import *
from AstGen.Stmt import *

class ForStmt(Stmt):
    def __init__(self, e1AST, e2AST, e3AST, stmtAST, pos):
        super().__init__(pos)
        self.e1AST = e1AST
        self.e2AST = e2AST
        self.e3AST = e3AST
        self.stmtAST = stmtAST
    
    def accept(self, v):
        v.visit(self)
