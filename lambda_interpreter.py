"""
:mod:`lambda_interpreter` -- Untyped Lambda Calculus Interpreter
==========================

.. module:: lambda_interpreter
   :synopsis: Parses and evaluates strings of untyped lambda calculus terms
   :original_date: 27 Jun 2013
.. moduleauthor:: James Wang <james@j-wang.net>

"""

import re


class Lexer(object):

    def tokenize(self, string):
        """
        .. method:: tokenize(String)

        Tokenizes lambda calculus expressions. Takes string (an expression)
        and returns a list of tokens.

        """
        result = []
        temp = ''

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

    def full_parse(self, tokens):
        """
        .. method:: full_parse([tokens])

        Takes lexed (:meth:`Lexer.tokenize`) tokens and turns them into
        semantically meaningful language constructs, represented as
        dictionaries that record their types and properties.

        The same as parse, except does not require scoping.

        """
        return self.parse(self.scope(tokens))

    def parse(self, tokens):
        """
        .. method:: parse([tokens])

        Takes lexed (:meth:`Lexer.tokenize`) and scoped (:meth:`Parser.scope`)
        tokens and turns them into semantically meaningful language constructs,
        represented as dictionaries that record their types and properties.

        """
        term = {}
        if type(tokens) is not list:
            term['type'] = 'variable'
            term['value'] = tokens
            return term
        else:
            current = tokens[0]
            if current != 'lambda':
                term['type'] = 'application'
                term['left'] = self.parse(current)
                try:
                    term['right'] = self.parse(tokens[1:])
                except IndexError:
                    return term['left']
                return term
            else:
                term['type'] = 'application'
                subterm = {}
                subterm['type'] = 'lambda'
                end_binding = tokens.index('.')
                if end_binding != 2:
                    raise ValueError("Each lambda takes exactly one arg.")
                subterm['binder'] = tokens[1]
                try:
                    subterm['term'] = self.parse(tokens[3])
                except IndexError:
                    raise ValueError("Lambda-abstraction must have terms.")
                term['left'] = subterm
                try:
                    term['right'] = self.parse(tokens[4:])
                except IndexError:
                    return subterm
                return term

    def scope(self, tokens):
        """
        .. method:: scope([tokens])

        Takes lexed tokens (from :meth:`Lexer.tokenize`) and creates nested
        scopes based on parentheses.

        """
        result = []
        skip_if_less = None
        for index in range(len(tokens)):
            if skip_if_less and index < skip_if_less:
                continue
            else:
                current = tokens[index]
                if current == '(':
                    next_paren = tokens[index:].index(')') + index
                    result.append(self.scope(tokens[index + 1: next_paren]))
                    skip_if_less = next_paren + 1
                else:
                    result.append(current)
        return result

    def matched_parens(self, tokens):
        """
        .. method:: matched_parens([tokens])

        Takes lexed tokens (from :meth:`Lexer.tokenize` Returns true is all
        parenthesis are properly matched. Otherwise, returns false.

        e.g. [')', '('], ['(', ')', ')'], ['(', '(', ')'] would all return
        false.

        """
        total = 0
        for token in tokens:
            if token == '(':
                total += 1
            elif token == ')':
                total -= 1

            if total < 0:
                return False

        return total == 0


def reduction(ast):
    ## reduce AST / evaluate
    pass


def main():
    ## loop read eval print
    pass
