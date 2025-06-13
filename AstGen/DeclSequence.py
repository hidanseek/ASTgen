from Scanner.SourcePos import *
from AstGen.Decl import *

class DeclSequence(Decl):
    def __init__(self, d1AST, d2AST, pos):
        super().__init__(pos)
        self.D1 = d1AST
        self.D2 = d2AST
    
    def GetLeftSubtree(self):
        return self.D1

    def GetRightSubtree(self):
        return self.D2
    
    def SetLeftSubtree(self, D):
        self.D1 = D
    
    def SetRightSubtree(self, D):
        self.D2 = D
    
    def GetRightmostDeclSequenceNode(self):
        assert self.D2 is not None
        if isinstance(self.D2, DeclSequence):
            return self.D2.GetRightmostDeclSequenceNode()
        else:
            return self
    
    def accept(self, v):
        v.visit(self)
