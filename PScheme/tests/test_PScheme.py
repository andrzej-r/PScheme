import unittest

from ..PScheme import *

class PSchemeTest(unittest.TestCase):
    def setUp(self):
        pass
        
    def tearDown(self):
        pass
        
    def test_010_tokenize(self):
        tokenizer = Tokenizer('tests')
        tokens = [str(t) for t in tokenizer.tokenizeLine('1"23 ;\\"(1"321)')]
        self.assertEqual(tokens, ['1', '"23 ;\\"(1"', '321', ')'])
        tokens = [str(t) for t in tokenizer.tokenizeLine('1;23 \\"(1"321)')]
        self.assertEqual(tokens, ['1'])
        tokens = [str(t) for t in tokenizer.tokenizeLine('"123\\"456" "78" "90"')]
        self.assertEqual(tokens, ['"123\\"456"', '"78"', '"90"'])
        tokens = [str(t) for t in tokenizer.tokenizeLine('"123" "123\\"456""789')]
        self.assertEqual(tokens, ['"123"', '"123\\"456"', '"789'])
        tokens = [str(t) for t in tokenizer.tokenizeLine('-12.345e-6 -.12e3 +123.e+4 -1234 +12 (+ 123.0 123)')]
        self.assertEqual(tokens, ['-12.345e-6', '-.12e3', '+123.e+4', '-1234', '+12', '(', '+', '123.0', '123', ')'])
        tokens = [str(t) for t in tokenizer.tokenizeLine('#t #f')]
        self.assertEqual(tokens, ['#t', '#f'])

    def test_020_parse(self):
        f = Frame()
        t = Tokenizer('tests')
        e = list(f.read('-12.345e-6 -.12e3 +123.e+4 -1234 +12', t))
        self.assertEqual(e, [Number.make(-12.345e-6), Number.make(-0.12e3), Number.make(123e4), Number.make(-1234), Number.make(12)])
        e = list(f.read('()', t))
        self.assertEqual(e, [Null.make()])
        e = list(f.read('(+ 123.0 123)', t))
        self.assertEqual(e, [Pair.makeFromList([Symbol.make('+'), Number.make(123.), Number.make(123)])])
        e = list(f.read('-12.345e-6 -.12e3 +123.e+4 -1234 +12 (+ 123.0 123)', t))
        self.assertEqual(e, [Number.make(-12.345e-6), Number.make(-0.12e3), Number.make(123e4), Number.make(-1234), Number.make(12), Pair.makeFromList([Symbol.make('+'), Number.make(123.), Number.make(123)])])
        e = list(f.read('"asd" "asd\\"dsa"', t))
        self.assertEqual(e, [String.make('asd',), String.make('asd\\"dsa')])
        #self.assertRaises(ExpressionError, lambda: list(f.read('"asd" "asd\\"dsa""123', t))) #unterminated string, changed error handling
        e = list(f.read('"asd" "asd\\"dsa""123', t))
        self.assertEqual(list(map(type, e)), [String, String, Error])
        e = list(f.read('#t #f', t))
        self.assertEqual(e, [Boolean.make(True,), Boolean.make(False)])

    def test_021_parse(self):
        f = Frame()
        t = Tokenizer('tests')
        e = list(f.read('(define plus +)(define (b a) (plus a a))', t)) #[0].eval(f)
        #e = []
        #for n in e2:
        #    e.append(n.eval(f))
        self.assertEqual(e, [Pair.makeFromList([DefineForm.make(), Symbol.make('plus'), Symbol.make('+')]), \
            Pair.makeFromList([DefineForm.make(), Pair.makeFromList([Symbol.make('b'), Symbol.make('a')]),Pair.makeFromList([Symbol.make('plus'), Symbol.make('a'), Symbol.make('a')])])])
        
    def test_030_evalDefineAndLookup(self):
        f = Frame()
        t = Tokenizer('tests')
        #e = list(f.read('(define a 1)', t))[0].eval(f)
        e = list(f.read('(define a 1)', t))[0].eval(f, None).eval(f, None)
        #e = list(eval(f.read('(define a 1)', t), f)) #[0]
        self.assertEqual(e, Nil.make())
        self.assertEqual(f.symbols, {'a': Number.make(1)})
        self.assertEqual(list(f.read('a', t))[0].eval(f, None).eval(f, None), Number.make(1))

    def test_031_evalDefineAndLookup(self):
        f = Frame()
        t = Tokenizer('tests')
        e = list(f.read('(define (b a) (+ a a))', t))[0].eval(f, None).eval(f, None)
        self.assertEqual(e, Nil.make())
        self.assertTrue(isinstance(f.symbols['b'], CompoundProcedure))
        self.assertTrue(isinstance(list(f.read('b', t))[0].eval(f, None), CompoundProcedure))
        self.assertRaises(ExpressionError, lambda: list(f.read('a', t))[0].eval(f, None).eval(f, None))

        self.assertEqual(list(f.read('(b 3)', t))[0].eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None), Number.make(6))

    def test_032_evalDefineAndLookup(self):
        f = Frame()
        t = Tokenizer('tests')
        exp = list(f.read('(define plus +)(define (b a) (plus a a))', t))
        e = exp[0].eval(f, None).eval(f, None)
        self.assertEqual(list(f.symbols.keys()), ['plus'])
        e = exp[1].eval(f, None)
        self.assertEqual(set(f.symbols.keys()), set(['plus', 'b']))
        self.assertEqual(list(f.read('(b 3)', t))[0].eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None).eval(f, None), Number.make(6))
        
