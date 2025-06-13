class Token:

    overall_nrtokens = 0

    def __init__(self, kind, lexeme, src_pos):
        if kind == Token.ID:
            index = Token.firstKeyword
            searching = True
            while searching:
                if Token.tokenTable[index] == lexeme:
                    self.kind = index
                    searching = False
                elif (Token.tokenTable[index] > lexeme) or (index == self.lastKeyword):
                    self.kind = Token.ID
                    searching = False
                else:
                    index += 1
        else:
            self.kind = kind

        self.src_pos = src_pos
        self.lexeme = lexeme
        Token.overall_nrtokens += 1
        self.my_tokennr = Token.overall_nrtokens

    def __repr__(self):
        return f"token{self.my_tokennr}.kind = Token.{Token.tokenTable[self.kind].upper()}\n"\
             + f"token{self.my_tokennr}.lexeme = \"{self.lexeme}\"\n"\
             + f"token{self.my_tokennr}.src_pos.StartLine = {self.src_pos.StartLine}\n"\
             + f"token{self.my_tokennr}.src_pos.EndLine = {self.src_pos.EndLine}\n"\
             + f"token{self.my_tokennr}.src_pos.StartCol = {self.src_pos.StartCol}\n"\
             + f"token{self.my_tokennr}.src_pos.EndCol = {self.src_pos.EndCol}\n"

    # identifiers, operators, literals:
    ID      = 0    # identifier
    ASSIGN  = 1    # a = ...
    OR      = 2    # ||
    AND     = 3    # &&
    NOT     = 4    # !
    EQ      = 5    # ==
    NOTEQ   = 6    # !=
    LESSEQ  = 7    # <=
    LESS    = 8    # <
    GREATER = 9    # >
    GREATEREQ = 10 # >=
    PLUS    = 11   # +
    MINUS   = 12   # -
    TIMES   = 13   # *
    DIV     = 14   # /
    INTLITERAL    = 15
    FLOATLITERAL  = 16
    BOOLLITERAL   = 17
    STRINGLITERAL = 18

    # keywords:
    BOOL    = 19   # bool
    ELSE    = 20   # else
    FLOAT   = 21   # float
    FOR     = 22   # for
    IF      = 23   # if
    INT     = 24   # int
    RETURN  = 25   # return
    VOID    = 26   # void
    WHILE   = 27   # while

    # punctuation:
    LEFTBRACE     = 28	# {
    RIGHTBRACE    = 29	# }
    LEFTBRACKET   = 30	# [
    RIGHTBRACKET  = 31	# ]
    LEFTPAREN     = 32	# (
    RIGHTPAREN    = 33	# )
    COMMA         = 34	# ,
    SEMICOLON     = 35	# ;

    # special tokens:
    ERROR = 36
    EOF   = 37          # end-of-file

    tokenTable = [
        "ID",
        "ASSIGN",
        "OR",
        "AND",
        "NOT",
        "EQ",
        "NOTEQ",
        "LESSEQ",
        "LESS",
        "GREATER",
        "GREATEREQ",
        "PLUS",
        "MINUS",
        "TIMES",
        "DIV",
        "INTLITERAL",
        "FLOATLITERAL",
        "BOOLLITERAL",
        "STRINGLITERAL",
        "bool",
        "else",
        "float",
        "for",
        "if",
        "int",
        "return",
        "void",
        "while",
        "LEFTBRACE",
        "RIGHTBRACE",
        "LEFTBRACKET",
        "RIGHTBRACKET",
        "LEFTPAREN",
        "RIGHTPAREN",
        "COMMA",
        "SEMICOLON",
        "ERROR",
        "EOF"
    ]

    lexemeTable = [
        "ID",
        "=",
        "||",
        "&&",
        "!",
        "==",
        "!=",
        "<=",
        "<",
        ">",
        ">=",
        "+",
        "-",
        "*",
        "/",
        "INTLITERAL",
        "FLOATLITERAL",
        "BOOLLITERAL",
        "STRINGLITERAL",
        "bool",
        "else",
        "float",
        "for",
        "if",
        "int",
        "return",
        "void",
        "while",
        "{",
        "}",
        "[",
        "]",
        "(",
        ")",
        ",",
        ";",
        "ERROR",
        "EOF"
    ]

    firstKeyword = BOOL
    lastKeyword = WHILE
    
    @classmethod
    def spell(cls, kind):
        return cls.lexemeTable[kind]

    def GetSourcePos(self):
        return self.src_pos

    def GetLexeme(self):
        return self.lexeme
