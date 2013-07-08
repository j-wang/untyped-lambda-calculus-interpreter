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
        self.eval = lamint.Evaluator()
        semantic = lambda x: self.parser.full_parse(self.lexer.tokenize(x))
        self.semantic = semantic

    def test_tokenize1(self):
        self.assertEqual(self.lexer.tokenize('(bob) bill'),
                         ['(', 'bob', ')', 'bill'])

    def test_tokenize2(self):
        self.assertEqual(self.lexer.tokenize('bill (bob)'),
                         ['bill', '(', 'bob', ')'])

    def test_tokenize3(self):
        self.assertEqual(self.lexer.tokenize('(lambda x. x)'),
                         ['(', 'lambda', 'x', '.', 'x', ')'])

    def test_matched_paren1(self):
        self.assertEqual(self.parser.matched_parens('((testing) 1 2 3)'), True)

    def test_matched_paren2(self):
        self.assertEqual(self.parser.matched_parens(')('), False)

    def test_matched_paren3(self):
        self.assertEqual(self.parser.matched_parens('()('), False)

    def test_scope1(self):
        self.assertEqual(self.parser.scope(self.application_tokens),
                         [['lambda', 'x', '.', 'x'], 't'])

    def test_scope2(self):
        self.assertEqual(self.parser.scope(self.id_tokens),
                         [['lambda', 'x', '.', 'x']])

    def test_scope3(self):
        newTokens = ['(', 'lambda', 'x', '.', 'x', 'y', ')', 't']
        self.assertEqual(self.parser.scope(newTokens),
                         [['lambda', 'x', '.', 'x', 'y'], 't'])

    def test_parse1(self):
        self.assertEqual(self.parser.full_parse(self.id_tokens),
                         {'type': 'lambda', 'binder': 'x',
                          'body': [{'type': 'variable', 'value': 'x'}]})

    def test_parse2(self):
        self.assertEqual(self.parser.full_parse(self.bound_and_free),
                         {'type': 'lambda', 'binder': 'x',
                          'body': [{'type': 'variable', 'value': 'x'},
                                   {'type': 'variable', 'value': 'y'}]})

    def test_parse3(self):
        self.assertEqual(self.parser.full_parse(self.application_tokens),
                         {'type': 'application',
                          'left':
                             {'type': 'lambda', 'binder': 'x',
                              'body': [{'type': 'variable', 'value': 'x'}]},
                          'right':
                             {'type': 'variable', 'value': 't'}})

    def test_parse_list_of_variables(self):
        self.assertEqual(self.parser.full_parse(self.lexer.tokenize("x y z")),
                         {'type': 'application',
                          'left': {'type': 'variable', 'value': 'x'},
                          'right': {'type': 'application',
                                    'left': {'type': 'variable',
                                             'value': 'y'},
                                    'right': {'type': 'variable',
                                              'value': 'z'}}})

    def test_raw_eval1(self):
        parsed = self.parser.full_parse(self.application_tokens)
        self.assertEqual(self.eval.raw_eval(parsed),
                         {'type': 'variable', 'value': 't'})

    def test_raw_eval2(self):
        newTokens = ['(', 'lambda', 'x', '.', 'x', 'y', ')', 't']
        parsed = self.parser.full_parse(newTokens)
        self.assertEqual(self.eval.raw_eval(parsed),
                         {'type': 'application',
                          'left': {'type': 'variable', 'value': 't'},
                          'right': {'type': 'variable', 'value': 'y'}})

if __name__ == '__main__':
    unittest.main()
