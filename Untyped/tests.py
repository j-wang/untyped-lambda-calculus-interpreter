"""
Unit Tests for Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

"""

import unittest
import copy
from lambda_interpreter import Lexer, Parser, Evaluator


class TestLexer(unittest.TestCase):

    def setUp(self):
        self.simple_exp = '(lambda x. x)'
        self.nest_exp = '(lambda x. (lambda y. y)'
        self.id_tokens = ['(', 'lambda', 'x', '.', 'x', ')']
        self.bound_and_free = ['(', 'lambda', 'x', '.', 'x', 'y', ')']
        self.application_tokens = copy.copy(self.id_tokens)
        self.application_tokens.append('t')
        semantic = lambda x: Parser.full_parse(Lexer.tokenize(x))
        self.semantic = semantic

    def test_tokenize1(self):
        self.assertEqual(Lexer.tokenize('(bob) bill'),
                         ['(', 'bob', ')', 'bill'])

    def test_tokenize2(self):
        self.assertEqual(Lexer.tokenize('bill (bob)'),
                         ['bill', '(', 'bob', ')'])

    def test_tokenize3(self):
        self.assertEqual(Lexer.tokenize('(lambda x. x)'),
                         ['(', 'lambda', 'x', '.', 'x', ')'])

    def test_nested_tokenize(self):
        string = "(lambda x. x) ((lambda x. x) (lambda z. (lambda x. x) x))"
        self.assertEqual(Lexer.tokenize(string),
                         ['(', 'lambda', 'x', '.', 'x', ')', '(', '(',
                          'lambda', 'x', '.', 'x', ')', '(', 'lambda', 'z',
                          '.', '(', 'lambda', 'x', '.', 'x', ')', 'x', ')',
                          ')'])

    def test_matched_paren1(self):
        self.assertEqual(Parser.matched_parens('((testing) 1 2 3)'), True)

    def test_matched_paren2(self):
        self.assertEqual(Parser.matched_parens(')('), False)

    def test_matched_paren3(self):
        self.assertEqual(Parser.matched_parens('()('), False)

    def test_scope1(self):
        self.assertEqual(Parser.scope(self.application_tokens),
                         [['lambda', 'x', '.', 'x'], 't'])

    def test_scope2(self):
        self.assertEqual(Parser.scope(self.id_tokens),
                         [['lambda', 'x', '.', 'x']])

    def test_scope3(self):
        newTokens = ['(', 'lambda', 'x', '.', 'x', 'y', ')', 't']
        self.assertEqual(Parser.scope(newTokens),
                         [['lambda', 'x', '.', 'x', 'y'], 't'])

    def test_nested_scope(self):
        string = "(lambda x. x) ((lambda x. x) (lambda z. (lambda x. x) x))"
        tokens = Lexer.tokenize(string)
        self.assertEqual(Parser.scope(tokens),
                         [['lambda', 'x', '.', 'x'],
                          [['lambda', 'x', '.', 'x'], ['lambda', 'z', '.',
                                                       ['lambda', 'x',
                                                        '.', 'x'], 'x']]])

    def test_parse1(self):
        self.assertEqual(Parser.full_parse(self.id_tokens),
                         {'type': 'lambda', 'binder': 'x',
                          'body': [{'type': 'variable', 'value': 'x'}]})

    def test_parse2(self):
        self.assertEqual(Parser.full_parse(self.bound_and_free),
                         {'type': 'lambda', 'binder': 'x',
                          'body': [{'type': 'variable', 'value': 'x'},
                                   {'type': 'variable', 'value': 'y'}]})

    def test_parse3(self):
        self.assertEqual(Parser.full_parse(self.application_tokens),
                         {'type': 'application',
                          'left':
                             {'type': 'lambda', 'binder': 'x',
                              'body': [{'type': 'variable', 'value': 'x'}]},
                          'right':
                             {'type': 'variable', 'value': 't'}})

    def test_parse_list_of_variables(self):
        self.assertEqual(Parser.full_parse(Lexer.tokenize("x y z")),
                         {'type': 'application',
                          'left': {'type': 'variable', 'value': 'x'},
                          'right': {'type': 'application',
                                    'left': {'type': 'variable',
                                             'value': 'y'},
                                    'right': {'type': 'variable',
                                              'value': 'z'}}})

    def test_nested_parse(self):
        string = "(lambda x. x) ((lambda x. x) (lambda z. (lambda x. x) x))"
        tokens = Lexer.tokenize(string)
        self.assertEqual(Parser.full_parse(tokens),
                         {'type': 'application',
                          'left': {'type': 'lambda', 'binder': 'x',
                                   'body': [{'type': 'variable',
                                            'value': 'x'}]},
                          'right': {'type': 'application',
                                    'left': {'type': 'lambda',
                                             'binder': 'x',
                                             'body': [{'type': 'variable',
                                                      'value': 'x'}]},
                                    'right': {'type': 'lambda',
                                              'binder': 'z',
                                              'body': [{'type': 'lambda',
                                                        'binder': 'x',
                                                        'body': [{'type': 'variable',
                                                                  'value': 'x'}]},
                                                       {'type': 'variable',
                                                        'value': 'x'}]}}})

    def test_raw_eval1(self):
        parsed = Parser.full_parse(self.application_tokens)
        self.assertEqual(Evaluator.raw_eval(parsed),
                         {'type': 'variable', 'value': 't'})

    def test_raw_eval2(self):
        newTokens = ['(', 'lambda', 'x', '.', 'x', 'y', ')', 't']
        parsed = Parser.full_parse(newTokens)
        self.assertEqual(Evaluator.raw_eval(parsed),
                         {'type': 'application',
                          'left': {'type': 'variable', 'value': 't'},
                          'right': {'type': 'variable', 'value': 'y'}})

    def test_raw_eval_nested(self):
        string = "(lambda x. x) ((lambda x. x) (lambda z. (lambda x. x) x))"
        parsed = self.semantic(string)
        print Evaluator.eval(parsed)
        self.assertEqual(Evaluator.raw_eval(parsed),
                         {'type': 'lambda',
                          'binder': 'z',
                          'body': [{'type': 'lambda',
                                    'binder': 'x',
                                    'body': [{'type': 'variable',
                                              'value': 'x'}]},
                                   {'type': 'variable',
                                    'value': 'x'}]})

    def test_eval1(self):
        parsed = Parser.full_parse(self.application_tokens)
        self.assertEqual(Evaluator.eval(parsed), "t")

    def test_eval2(self):
        newTokens = ['(', 'lambda', 'x', '.', 'x', 'y', ')', 't']
        parsed = Parser.full_parse(newTokens)
        self.assertEqual(Evaluator.eval(parsed), "t y")

    def test_eval3(self):
        parsed = self.semantic("lambda x. x y")
        self.assertEqual(Evaluator.eval(parsed), "lambda x. x y")

    def test_eval4(self):
        parsed = self.semantic("(lambda x. x z y) t")
        self.assertEqual(Evaluator.eval(parsed), "t z y")

if __name__ == '__main__':
    unittest.main()
