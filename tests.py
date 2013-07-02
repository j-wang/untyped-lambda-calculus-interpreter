"""
Unit Tests for Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

"""

import unittest
import lambda_interpreter as lamint
import copy


class TestLexer(unittest.TestCase):

    def setUp(self):
        self.simple_exp = '(lambda x. x)'
        self.nest_exp = '(lambda x. (lambda y. y)'
        self.id_tokens = ['(', 'lambda', 'x', '.', 'x', ')']
        self.bound_and_free = ['(', 'lambda', 'x', '.', 'x', 'y', ')']
        self.application_tokens = copy.copy(self.id_tokens)
        self.application_tokens.append('t')
        self.lexer = lamint.Lexer()
        self.parser = lamint.Parser()

    def test_tokenize1(self):
        self.assertEqual(self.lexer.tokenize('(bob) bill'),
                         ['(', 'bob', ')', 'bill'])

    def test_tokenize2(self):
        self.assertEqual(self.lexer.tokenize('bill (bob)'),
                         ['bill', '(', 'bob', ')'])

    def test_tokenize3(self):
        self.assertEqual(self.lexer.tokenize('(lambda x. x)'),
                         ['(', 'lambda', 'x', '.', 'x', ')'])

    def test_match_paren1(self):
        self.assertEqual(self.parser.match_parens('((testing) 1 2 3)'), True)

    def test_match_paren2(self):
        self.assertEqual(self.parser.match_parens(')('), False)

    def test_match_paren3(self):
        self.assertEqual(self.parser.match_parens('()('), False)

    def test_scope1(self):
        self.assertEqual(self.parser.scope(self.application_tokens),
                         [['lambda', 'x', '.', 'x', 'y'], 't'])

    def test_scope2(self):
        self.assertEqual(self.parser.scope(self.id_tokens),
                         [['lambda', 'x', '.', 'x']])

    # def test_parse1(self):
    #     self.assertEqual(self.parser.parse(self.id_tokens),
    #                      {'type': 'lambda', 'binder': 'x', 'term': 'x'})

    # def test_parse2(self):
    #     self.assertEqual(self.parser.parse(self.bound_and_free),
    #                      {'type': 'application',
    #                       'left': {'type': 'lambda', 'binder': 'x',
    #                                'term': 'x'},
    #                       'right': {'type': 'variable', 'value': 'y'}})

    # def test_parse3(self):
    #     self.assertEqual(self.parser.parse(self.application_tokens),
    #                      {'type': 'application',
    #                       'left':
    #                          {'type': 'lambda', 'binder': 'x', 'term': 'x'},
    #                       'right':
    #                          {'type': 'variable', 'value': 't'}})


if __name__ == '__main__':
    unittest.main()
