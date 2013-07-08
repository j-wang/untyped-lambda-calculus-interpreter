"""
:mod:`lambda_interpreter` -- Untyped Lambda Calculus Interpreter
==========================

.. module:: lambda_interpreter
   :synopsis: Parses and evaluates strings of untyped lambda calculus terms
   :original_date: 27 Jun 2013
.. moduleauthor:: James Wang <james@j-wang.net>

"""
import sys


class Lexer(object):
    """
    .. class:: Lexer

    Holds the :meth:`Lexer.tokenize` method, which breaks a lambda expression
    (given as a string) into tokens.

    """

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
    """
    .. class:: Parser

    Contains a variety of methods for parsing a list of tokens into more
    semantically useful pieces. :meth:`Parser.full_parse` fully parses tokens
    into semantically meaningful pieces (incorporating functionality of other
    functions in this class).

    """

    def full_parse(self, tokens):
        """
        .. method:: full_parse(tokens)

        Takes lexed (:meth:`Lexer.tokenize`) tokens and turns them into
        semantically meaningful language constructs, represented as
        dictionaries that record their types and properties.

        The same as parse, except does not require scoping.

        """
        return self.parse(self.scope(tokens))

    def parse(self, tokens):
        """
        .. method:: parse(tokens)

        Takes lexed (:meth:`Lexer.tokenize`) and scoped (:meth:`Parser.scope`)
        tokens and turns them into semantically meaningful language constructs,
        represented as dictionaries that record their types and properties.

        """
        term = {}
        if type(tokens) is not list:
            term['type'] = 'variable'
            term['value'] = tokens
            return term
        else:  # block could use some refactoring
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
                term['type'] = 'lambda'
                end_binding = tokens.index('.')
                if end_binding != 2:
                    raise ValueError("Each lambda takes exactly one arg.")
                term['binder'] = tokens[1]
                term['body'] = []
                for t in tokens[3:]:
                    term['body'].append(self.parse(t))
                if len(term['body']) == 0:
                    raise ValueError("Lambda-abstraction must have terms.")
                return term

    def scope(self, tokens):
        """
        .. method:: scope(tokens)

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
        .. method:: matched_parens(tokens)

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


class Evaluator(object):
    """
    .. class:: Evaluator

    Takes a dictionary of dictionaries with semantically meaningful lambda
    expression components (processed by :class:`Parser`) and evaluates the
    expression into its most reduced form.

    """

    def eval(self, exp):
        """
        .. method:: eval(exp)

        Takes dictionary of dictionaries representing lambda
        expression. Reduces the expression to its most reduced form using
        'call-by-value.' Returns the reduced form of the dictionary as a
        pretty-print formatted string.

        """
        return self._pretty_print(self.raw_eval(exp))

    def raw_eval(self, exp):
        """
        .. method raw_eval(exp)

        Same as eval, but returns reduced dictionary of dictionaries instead of
        pretty-print formatted string of the expression.

        """
        typ = exp['type']
        if typ == 'variable':
            return exp
        elif typ == 'application':
            left = self.raw_eval(exp['left'])
            right = self.raw_eval(exp['right'])
            if left['type'] == 'lambda':
                return self._apply(left, right)
            else:
                return {'type': 'application',
                        'left': left,
                        'right': right}
        elif typ == 'lambda':
            evaluated_body = []
            for term in exp['body']:
                evaluated_body.append(self.raw_eval(term))
            return {'type': 'lambda',
                    'binder': exp['binder'],
                    'body': evaluated_body}
        else:
            raise ValueError("Unknown type ({0}) passed!".format(typ))

    def _apply(self, left, right):
        # Takes a lambda expression on left and applies to right.
        if right['type'] != 'variable':
            return {'type': 'application', 'left': left, 'right': right}
        else:
            binder = left['binder']
            applied_value = right['value']
            result = []
            parser = Parser()
            for term in left['body']:
                if term['type'] == 'variable' and term['value'] == binder:
                    result.append(applied_value)
                else:
                    result.append(term['value'])
            return parser.full_parse(result)

    def _pretty_print(self, exp):
        # Helper function for printing out the contents of eval in a pretty,
        # human-pleasing format.
        pass


def main():
    ## Read, Eval, Print Loop (REPL)
    user_input = ""
    lex = Lexer()
    par = Parser()
    eva = Evaluator()
    l = lex.tokenize
    p = par.full_parse
    e = eva.eval

    while True:
        try:
            user_input = raw_input(">> ")
            if user_input == 'q':
                break
            else:
                print(e(p(l(user_input))))
        except ValueError as err:
            print("Invalid expression: {0}".format(err.message))
        except:
            print("Unexpected error: {0}".format(sys.exc_info()[0]))

    print("Goodbye!")

if __name__ == '__main__':
    main()
