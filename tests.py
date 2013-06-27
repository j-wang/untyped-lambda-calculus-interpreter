"""
Unit Tests for Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

"""

import unittest
import lambda_interpreter as lamint


class TestLexer(unittest.TestCase):

    def setUp(self):
        self.simple_exp = '(lambda x x)'
        self.nest_exp = '(lambda x (lambda y y)'
        self.lexer = lamint.Lexer()

    def test_chunk_word(self):
        self.assertEqual(self.lexer.chunk_word('bill (bob)'),
                         ('(bob)', 'bill'))

    def test_chunk_block(self):
        self.assertEqual(self.lexer.chunk_block('(lambda x x) hello'),
                         (' hello', ['lambda', 'x', 'x']))

    def test_simple_parse(self):
        # Make sure the simple parsing example works
        self.assertEqual(self.lexer.parse(self.simple_exp),
                         ['lambda', 'x', 'x'])

    def test_nested_parse(self):
        # Make sure nested parsing works correctly
        self.assertEqual(self.lexer.parse(self.nest_exp),
                         ['lambda', 'x', ['lambda', 'y', 'y']])

if __name__ == '__main__':
    unittest.main()
