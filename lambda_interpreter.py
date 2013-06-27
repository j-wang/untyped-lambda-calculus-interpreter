"""
Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

"""

import re


class Lexer(object):

    def chunk_word(self, xs):
        return self.chunk(xs, ' ', "", lambda x, y: x + y)

    def chunk_block(self, xs):
        return self.chunk(xs, ')', [], lambda x, y: x.append(y),
                          lambda x: re.match(r"[a-z]|[A-Z]", x),
                          lambda ys: self.chunk_word(ys))

    def chunk(self, xs, delim, acc, op, cond=lambda x: False, handle=None):
        first = xs[0]
        if first == delim:
            return (xs[1:], acc)
        elif cond(first):
            return handle(xs)
        else:
            return self.chunk(xs[1:], delim, op(acc, first), op, cond, handle)

    def lex(self, exp):
        lexing = exp
        lexed = []
        while lexing != []:
            print lexing
            element = lexing[0]
            chunker = self.chunk_block if element == '(' else self.chunk_word
            remaining, found_chunk = chunker(lexing)
            lexing = remaining
            lexed.append(found_chunk)


# Dictionary of regexes is probably the more promising path
class BetterLexer(object):
    """Tokenizes lambda calculus expressions. Takes string (an expression) and
    returns a list of tokens (words or lists of words, based on context).

    """
    def __init__(self):
        self.case = {re.compile('(?P<block>[\w|\s]+)'): self.lex_block,
                     re.compile('[a-z]|[A-Z]+'): self.lex_word}

    def lex_block(self, xs):
        pass

    def lex_word(self, xs):
        pass


def reduction(ast):
    ## reduce AST / evaluate
    pass


def main():
    ## loop read eval print
    pass
