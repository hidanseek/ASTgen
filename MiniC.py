from Scanner.Token import *
from Scanner.SourceFile import *
from Scanner.SourcePos import *
from Scanner.Scanner import *
from Parser.Parser import *
from Parser.SyntaxError import *
from TreePrinter.Printer import Printer
from TreeDrawer.Drawer import Drawer
from Unparser.Unparser import Unparser

import sys

class MiniC:
    def __init__(self):
        self.scanner = 0
        self.parser = 0
        self.reporter = 0
        self.printer = 0
        self.unparser = 0

        self.DrawTree = 0
        self.DrawTreePlusPos = 0
        self.PrintTree = 0
        self.UnparseTree = 0

        self.PrintTreeF = 0
        self.UnparseTreeF = 0

        self.sourceName = ''

        self.AST = 0

    def compileProgram(self, sourceName):
        print("********** " + "MiniC Compiler" + " **********")       
        print("Syntax Analysis ...")

        self.source = SourceFile(sourceName)
        self.scanner = Scanner(self.source)
        '''
            Enable this to observe the sequence of tokens
            delivered by the scanner:
        '''
        #scanner.enableDebugging()
        self.reporter   = ErrorReporter()
        self.parser     = Parser(self.scanner, self.reporter)
        self.drawer     = Drawer()
        self.printer    = Printer()
        self.unparser   = Unparser()
        self.AST = self.parser.parse()       # 1st pass

        successful = (self.reporter.numErrors == 0)
        if successful:
            if self.PrintTree:
                self.printer.print(self.AST, self.PrintTreeF)
            if self.UnparseTree:
                self.unparser.unparse(self.AST, self.UnparseTreeF)
            if self.DrawTree or self.DrawTreePlusPos:
                self.drawer.draw(self.AST, self.DrawTreeF, self.DrawTreePlusPos)
            print("Compilation was successful.")
        else:
            print("Compilation was unsuccessful.")

    def usage(self):
        print('Usage: MiniC filename')
        print('Options: -ast <file> to draw the AST to <file>')
        print('Options: -astp <file> to draw the AST plus source pos to <file>')
        print('Options: -t <file> to dump the AST to <file>')
        print('Options: -u <file> to unparse the AST to <file>')
        exit(1)
    
    def processCmdLine(self, args):
        self.DrawTree = False
        self.DrawTreePlusPos = False
        self.DrawTreeF = ''
        self.PrintTree = False
        self.PrintTreeF = ''
        self.UnparseTree = False
        self.UnparseTreeF = ''
        self.sourceName = ''
        
        arg_index = 1

        while arg_index < len(args):
            if args[arg_index] == '-ast':
                self.DrawTree = True
                if len(args) < arg_index + 1:
                    self.usage()
                else:
                    arg_index+=1
                    self.DrawTreeF = args[arg_index]
                    arg_index += 1
            elif args[arg_index] == '-astp':
                self.DrawTreePlusPos = True
                if len(args) < arg_index + 1:
                    self.usage()
                else:
                    arg_index+=1
                    self.DrawTreeF = args[arg_index]
                    arg_index += 1
            elif args[arg_index] == '-t':
                self.PrintTree = True
                if len(args) < arg_index + 1:
                    self.usage()
                else:
                    arg_index += 1
                    self.PrintTreeF = args[arg_index]
                    arg_index += 1
            elif args[arg_index] == '-u':
                self.UnparseTree = True
                if len(args) < arg_index + 1:
                    self.usage()
                else:
                    arg_index += 1
                    self.UnparseTreeF = args[arg_index]
                    arg_index += 1
            else:
                self.sourceName = args[arg_index]
                arg_index += 1

        if self.sourceName == '':
            self.usage()
        
miniC = MiniC()

if __name__ == '__main__':
    miniC.processCmdLine(sys.argv)
    miniC.compileProgram(miniC.sourceName)