from AstGen.Program import *
from Unparser.UnparseVisitor import *

class Unparser:
    def unparse(self, ast, FileName):
        try:
            # Create File
            out = open(FileName, 'w')
            # Create an UnparseVisitor and visit the AST
            uv = UnparseVisitor(out)
            ast.accept(uv)
            out.close()
        except Exception as e:
            # Catch exception if any
            print("Error:",e)
            exit(1)
        
