from functools import singledispatchmethod
from AstGen.Program import Program

class Visitor:
    @singledispatchmethod
    def visit(self, x):
        raise NotImplementedError(f'No visit() handler for type {type(x).__name__}')
