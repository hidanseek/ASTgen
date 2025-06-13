from Scanner.SourcePos import *

class ErrorReporter:
    def __init__(self):
        self.numErrors = 0
    
    def reportError(self, message, tokenName, pos):
        print("ERROR: ", end='')

        for c in message:
            if c == '%':
                print(tokenName, end='')
            else:
                print(c, end='')
        print(" " + str(pos.StartCol) + ".." + str(pos.EndCol) + ", line " + str(pos.StartLine) + ".")
        self.numErrors += 1
