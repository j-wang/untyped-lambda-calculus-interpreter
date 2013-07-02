"""
Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

"""

import re


class Lexer(object):

    def tokenize(self, string):
        """Tokenizes lambda calculus expressions. Takes string (an expression)
        and returns a list of tokens.

        """
        result = []
        temp = ''

        # nested if-else pains me, but dictionary with anonymous function is
        # just as messy and slower
        for char in string:
            if char in ['(', ')', '.', ' ']:
                if temp != '':
                    result.append(temp)
                    temp = ''
                if char != ' ':
                    result.append(char)
            else:
                temp = temp + char

        if temp != '':
            result.append(temp)

        return result


class Parser(object):

    def parse(self, tokens):
        result = []
        scope = []
        temp = []

        for token in tokens:
            if token == '(':
                pass
            if token == ')':
                scope.append(temp)
                temp = []

        return result

    def scope(self, tokens):
        result = []
        skip_if_less = None
        for index in range(len(tokens)):
            if skip_if_less and index < skip_if_less:
                continue
            else:
                current = tokens[index]
                if current == '(':
                    next_paren = tokens.index(')')
                    result.append(self.scope(tokens[index + 1: next_paren]))
                    skip_if_less = next_paren + 1
                else:
                    result.append(current)
        return result

    def match_parens(self, tokens):
        total = 0
        for token in tokens:
            if token == '(':
                total += 1
            elif token == ')':
                total -= 1

            if total < 0:
                return False

        return total == 0


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

    def lex(self, xs):
        # recusively process blocks, appending as you go, then blocks
        pass


def reduction(ast):
    ## reduce AST / evaluate
    pass


def main():
    ## loop read eval print
    pass
