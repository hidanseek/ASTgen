from AstGen.Visitor import Visitor
from graphviz import Digraph

from AstGen.Program import Program
from TreeDrawer.LayoutVisitor import LayoutVisitor

class Drawer:
    def __init__(self):
        pass

    def draw(self, ast, fileName, draw_pos=None):
        self.AST = ast
        lv = LayoutVisitor(draw_pos)
        self.AST.accept(lv)
        lv.render(fileName, view=True)