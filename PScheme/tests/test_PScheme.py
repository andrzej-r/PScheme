import unittest

from ..PScheme import *

class PSchemeTest(unittest.TestCase):
    def setUp(self):
        self.frame = Frame()
        self.tokenizer = Tokenizer('tests')
        
    def tearDown(self):
        pass
        
    def t(self, line):
        "Tokenize"
        return list(self.tokenizer.tokenizeLine(line))
        
    def tp(self, line):
        "Tokenize+Parse"
        return list(self.frame.parseTokens(self.tokenizer.tokenizeLine(line)))
        
    def tpe(self, line):
        "Tokenize+Parse+Evaluate"
        return list(self.frame.evaluateExpressions(self.frame.parseTokens(self.tokenizer.tokenizeLine(line))))
        
    def s(self, lst):
        "convert a list of objects into a list of strings"
        return [str(element) for element in lst]
        
    def test_010_tokenize(self):
        tokens = self.s(self.t('1"23 ;\\"(1"321)'))
        self.assertEqual(tokens, ['1', '"23 ;\\"(1"', '321', ')'])
        tokens = self.s(self.t('1;23 \\"(1"321)'))
        self.assertEqual(tokens, ['1'])
        tokens = self.s(self.t('"123\\"456" "78" "90"'))
        self.assertEqual(tokens, ['"123\\"456"', '"78"', '"90"'])
        tokens = self.s(self.t('"123" "123\\"456""789'))
        self.assertEqual(tokens, ['"123"', '"123\\"456"', '"789'])
        tokens = self.s(self.t('-12.345e-6 -.12e3 +123.e+4 -1234 +12 (+ 123.0 123)'))
        self.assertEqual(tokens, ['-12.345e-6', '-.12e3', '+123.e+4', '-1234', '+12', '(', '+', '123.0', '123', ')'])
        tokens = self.s(self.t('#t #f'))
        self.assertEqual(tokens, ['#t', '#f'])

    def test_020_parse(self):
        e = self.tp('-12.345e-6 -.12e3 +123.e+4 -1234 +12')
        self.assertEqual(e, [Number.make(-12.345e-6), Number.make(-0.12e3), Number.make(123e4), Number.make(-1234), Number.make(12)])
        e = self.tp('()')
        self.assertEqual(e, [Null.make()])
        e = self.tp('(+ 123.0 123)')
        self.assertEqual(e, [Pair.makeFromList([Symbol.make('+'), Number.make(123.), Number.make(123)])])
        e = self.tp('-12.345e-6 -.12e3 +123.e+4 -1234 +12 (+ 123.0 123)')
        self.assertEqual(e, [Number.make(-12.345e-6), Number.make(-0.12e3), Number.make(123e4), Number.make(-1234), Number.make(12), Pair.makeFromList([Symbol.make('+'), Number.make(123.), Number.make(123)])])
        e = self.tp('"asd" "asd\\"dsa"')
        self.assertEqual(e, [String.make('asd',), String.make('asd\\"dsa')])
        #self.assertRaises(ExpressionError, lambda: list(f.read('"asd" "asd\\"dsa""123', t))) #unterminated string, changed error handling
        e = self.tp('"asd" "asd\\"dsa""123')
        self.assertEqual(list(map(type, e)), [String, String, Error])
        e = self.tp('#t #f')
        self.assertEqual(e, [Boolean.make(True,), Boolean.make(False)])

    def test_021_parse(self):
        e = self.tp('(define plus +)(define (b a) (plus a a))') #[0].eval(f)
        #e = []
        #for n in e2:
        #    e.append(n.eval(f))
        self.assertEqual(e, [Pair.makeFromList([DefineForm.make(), Symbol.make('plus'), Symbol.make('+')]), \
            Pair.makeFromList([DefineForm.make(), Pair.makeFromList([Symbol.make('b'), Symbol.make('a')]),Pair.makeFromList([Symbol.make('plus'), Symbol.make('a'), Symbol.make('a')])])])
        
    def test_030_evalDefineAndLookup(self):
        e = self.tpe('(define a 1)')
        self.assertEqual(e, [Nil.make()])
        self.assertEqual(self.frame.symbols, {'a': Number.make(1)})
        self.assertEqual(self.tpe('a'), [Number.make(1)])

    def test_031_evalDefineAndLookup(self):
        e = self.tpe('(define (b a) (+ a a))') #[0].eval(f, None).eval(f, None)
        self.assertEqual(e, [Nil.make()])
        self.assertTrue(isinstance(self.frame.symbols['b'], CompoundProcedure))
        self.assertTrue(isinstance(self.tpe('b')[0], CompoundProcedure))
        self.assertTrue(isinstance(self.tpe('a')[0], Error))

        self.assertEqual(self.tpe('(b 3)'), [Number.make(6)])

    def test_032_evalDefineAndLookup(self):
        exp = self.tpe('(define plus +)')
        self.assertEqual(list(self.frame.symbols.keys()), ['plus'])
        exp = self.tpe('(define (b a) (plus a a))')
        self.assertEqual(set(self.frame.symbols.keys()), set(['plus', 'b']))
        self.assertEqual(self.tpe('(b 3)'), [Number.make(6)])
        
