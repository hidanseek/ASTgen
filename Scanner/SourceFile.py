class SourceFile:
    EOL = '\n'
    EOF = ""
    def __init__(self, filename):
        self.source_file = filename
        self.source = open(filename, 'r')
    
    def readChar(self):
        try:
            c = self.source.read(1)
            if c == "":
                c = SourceFile.EOF
            return c
        except IOError:
            return SourceFile.EOF
