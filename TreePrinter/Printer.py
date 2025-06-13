from AstGen.Program import *
from TreePrinter.TreePrinterVisitor import *

class Printer:
    def __init__(self):
        self.fout = 0
        self.pv = 0

    def print(self, ast, FileName):
        try:
            self.fout = open(FileName, 'w')
            self.pv = TreePrinterVisitor(self.fout)
            ast.accept(self.pv)
            self.fout.close()
        except Exception as e:
            print("Error: ", str(e), sep='')
            exit(1)
        