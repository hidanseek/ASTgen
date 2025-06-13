from Scanner.Token import *
from Scanner.SourceFile import *
from Scanner.SourcePos import *

class Scanner:
    '''
    NOTE:

    This is a dummy scanner implementation.
    If course participant wishes to use the provided scanner module file,
    please replace this file with provided scanner module file.
    Students who wish to use their own scanner should replace this file with
    their own scanner from Assignment 1.
    '''

    def __init__(self, source):
        self.sourceFile = source
        print("ERROR: empty scanner implementation used!")
        print("Provide your own Scanner.py or use the provided module file!")
        print("See the Assignment specification on how to use the provided module file.")
    
    def enableDebugging(self):
        return
            
    def scan(self):
        pos = SourcePos()
        ErrorToken = Token(Token.ERROR, "Empty Scanner!", pos)        
        return ErrorToken
