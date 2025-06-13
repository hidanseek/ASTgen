from Scanner.SourcePos import *
from AstGen.Stmt import *

class IfStmt(Stmt):
    def __init__(self, eAST, thenAST, *args):
        if len(args) == 1:
            pos = args[0]
            elseAST = None
        else:
            elseAST, pos = args
        super().__init__(pos)
        self.eAST = eAST
        self.thenAST = thenAST
        if elseAST == None:
            self.elseAST = None
        else:
            self.elseAST = elseAST
    
    def accept(self, v):
        v.visit(self)
