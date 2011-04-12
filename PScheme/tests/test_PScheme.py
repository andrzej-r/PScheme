import unittest
import os

from ..PScheme import *

def flush(generator):
    for element in generator: pass
            
class PSchemeTest(unittest.TestCase):
    def setUp(self):
        self.frame = Frame()
        
        fname = os.path.join(os.path.dirname(__file__), '../boot.scm')
        tokenizer = Tokenizer(fname)
        with open(fname, 'r') as f:
            tokens = tokenizer.tokenizeLines(f)
            expressions = self.frame.parseTokens(tokens)
            results = self.frame.evaluateExpressions(expressions) #result generator
            flush(results) # check results to force delayed execution

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
        
    def tpes(self, line):
        "Tokenize+Parse+Evaluate+String"
        return self.s(self.frame.evaluateExpressions(self.frame.parseTokens(self.tokenizer.tokenizeLine(line))))
        
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
        self.assertEqual(e, [Boolean.make(True), Boolean.make(False)])
        self.assertEqual(list(map(type, e)), [Boolean, Boolean])

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
        self.assertEqual(self.frame.symbols['a'], Number.make(1))
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
        self.assertTrue(isinstance(self.frame.symbols['plus'], Procedure))
        exp = self.tpe('(define (b a) (plus a a))')
        self.assertTrue(isinstance(self.frame.symbols['b'], CompoundProcedure))
        self.assertEqual(self.tpe('(b 3)'), [Number.make(6)])
        
    def test_tailrec(self):
        "tests whether calls are tail recursive and if there is no stack overflow"
        prog = """
(define (fac n)
    (if (= n 1)
        1
        (* n (fac (- n 1)))))

(define (fib n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fib (- n 1)) (fib (- n 2))))))

(define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))
(define (even? n)
    (if (= n 0)
        #t
        (odd? (- n 1))))
        """
        self.assertEqual(self.tpes("(even? 1002)"), ['#t'])
        self.assertEqual(self.tpes("(odd? 1002)"), ['#f'])
        
    def test_continuations(self):
        "tests if continuations work"
        prog = """
(define (identity x) x)
        """
        self.assertEqual(self.tpes("(call/cc identity)"), ['#<continuation>'])
        self.assertEqual(self.tpes("(call-with-current-continuation identity)"), ['#<continuation>'])
        
        
    def test_procedures(self):
        "from R5RS 4.1.4"
        self.assertTrue(isinstance(self.tpe("(lambda (x) (+ x x))")[0], CompoundProcedure))
        self.assertEqual(self.tpe("((lambda (x) (+ x x)) 4)"), [Number.make(8)])
        s = """
      (define reverse-subtract
        (lambda (x y) (- y x)))
      (reverse-subtract 7 10)
        """
        self.assertEqual(self.tpe(s)[1], Number.make(3))
        s = """
      (define add4
        (let ((x 4))
          (lambda (y) (+ x y))))
      (add4 6)        """
        self.assertEqual(self.tpe(s)[1], Number.make(10))
        
        self.assertEqual(self.tpes("((lambda x x) 3 4 5 6)"), ["(3 4 5 6)"])
        self.assertEqual(self.tpes("((lambda (x y . z) z) 3 4 5 6)"), ["(5 6)"])
        
        
    def test_bindings(self):
        "from R5RS 4.2.2"
        self.assertEqual(self.tpe("(let ((x 2) (y 3)) (* x y))"), [Number.make(6)])
        s = """
      (let ((x 2) (y 3))
        (let ((x 7)
              (z (+ x y)))
          (* z x))) 
        """
        self.assertEqual(self.tpe(s), [Number.make(35)])
        s = """
      (let ((x 2) (y 3))
        (let* ((x 7)
               (z (+ x y)))
          (* z x)))
        """
        self.assertEqual(self.tpe(s), [Number.make(70)])
        s = """
      (letrec ((even?
                (lambda (n)
                  (if (zero? n)
                      #t
                      (odd? (- n 1)))))
               (odd?
                (lambda (n)
                  (if (zero? n)
                      #f
                      (even? (- n 1))))))
        (even? 88))        """
        self.assertEqual(self.tpes(s), ['#t'])
        

    def test_eqv(self):
        "from R5RS 6.1"
        self.assertEqual(self.tpes("(eqv? 'a 'a)"), ['#t'])
        self.assertEqual(self.tpes("(eqv? 'a 'b)"), ['#f'])
        self.assertEqual(self.tpes("(eqv? 2 2)"), ['#t'])
        self.assertEqual(self.tpes("(eqv? '() '())"), ['#t'])
        self.assertEqual(self.tpes("(eqv? 100000000 100000000)"), ['#t'])
        self.assertEqual(self.tpes("(eqv? (cons 1 2) (cons 1 2))"), ['#f'])
        self.assertEqual(self.tpes("(eqv? (lambda () 1) (lambda () 2))"), ['#f'])
        self.assertEqual(self.tpes("(eqv? #f 'nil)"), ['#f'])
        self.assertEqual(self.tpes("(let ((p (lambda (x) x))) (eqv? p p))"), ['#t'])
        
        #unspecified in R5RS, testing against design values
        self.assertEqual(self.tpes('(eqv? "" "")'), ['#f'])
        #self.assertEqual(self.tpes("(eqv? '#() '#())"), ['#f'])
        self.assertEqual(self.tpes("(eqv? (lambda (x) x) (lambda (x) x))"), ['#f'])
        self.assertEqual(self.tpes("(eqv? (lambda (x) x) (lambda (y) y))"), ['#f'])
        
        prog = """
      (define gen-counter
        (lambda ()
          (let ((n 0))
            (lambda () (set! n (+ n 1)) n))))
      
      (define gen-loser
        (lambda ()
          (let ((n 0))
            (lambda () (set! n (+ n 1)) 27))))
        """
        
        self.tpe(prog)
        self.assertEqual(self.tpes("(let ((g (gen-counter))) (eqv? g g))"), ['#t'])
        self.assertEqual(self.tpes("(eqv? (gen-counter) (gen-counter))"), ['#f'])
        self.assertEqual(self.tpes("(let ((g (gen-loser))) (eqv? g g))"), ['#t'])
        self.assertEqual(self.tpes("(eqv? (gen-loser) (gen-loser))"), ['#f'])  # unspecified
        self.assertEqual(self.tpes("(letrec ((f (lambda () (if (eqv? f g) 'both 'f))) (g (lambda () (if (eqv? f g) 'both 'g)))) (eqv? f g))"), ['#f'])  # unspecified
        self.assertEqual(self.tpes("(letrec ((f (lambda () (if (eqv? f g) 'f 'both))) (g (lambda () (if (eqv? f g) 'g 'both)))) (eqv? f g))"), ['#f'])
        
        self.assertEqual(self.tpes("(eqv? '(a) '(a))"), ['#f']) # unspecified
        self.assertEqual(self.tpes('(eqv? "a" "a")'), ['#f']) # unspecified
        self.assertEqual(self.tpes("(eqv? '(b) (cdr '(a b)))"), ['#f']) # unspecified
        self.assertEqual(self.tpes("(let ((x '(a))) (eqv? x x))"), ['#t']) # unspecified

    def test_eq(self):
        "from R5RS 6.1"
        self.assertEqual(self.tpes("(eq? 'a 'a)"), ['#t'])
        self.assertEqual(self.tpes("(eq? '(a) '(a))"), ['#f']) # unspecified
        #self.assertEqual(self.tpes("(eq? (list 'a) (list 'a))"), ['#f'])
        self.assertEqual(self.tpes('(eq? "a" "a")'), ['#f']) # unspecified
        self.assertEqual(self.tpes('(eq? "" "")'), ['#f']) # unspecified
        self.assertEqual(self.tpes("(eq? '() '())"), ['#t'])
        self.assertEqual(self.tpes("(eq? 2 2)"), ['#t']) # unspecified
        self.assertEqual(self.tpes("(eq? #\A #\A)"), ['#t']) # unspecified
        self.assertEqual(self.tpes("(eq? car car)"), ['#t'])
        self.assertEqual(self.tpes("(let ((n (+ 2 3))) (eq? n n))"), ['#t']) # unspecified
        self.assertEqual(self.tpes("(let ((x '(a))) (eq? x x))"), ['#t'])
        #self.assertEqual(self.tpes("(let ((x '#())) (eq? x x))"), ['#t'])
        self.assertEqual(self.tpes("(let ((p (lambda (x) x))) (eq? p p))"), ['#t'])
        
    def test_equal(self):
        "from R5RS 6.1"
        self.assertEqual(self.tpes("(equal? 'a 'a)"), ['#t'])
        self.assertEqual(self.tpes("(equal? '(a) '(a))"), ['#t'])
        self.assertEqual(self.tpes("(equal? '(a (b) c) '(a (b) c))"), ['#t'])
        self.assertEqual(self.tpes('(equal? "abc" "abc")'), ['#t'])
        self.assertEqual(self.tpes("(equal? 2 2)"), ['#t'])
        #self.assertEqual(self.tpes("(equal? (make-vector 5 'a) (make-vector 5 'a))"), ['#t'])
        self.assertEqual(self.tpes("(equal? (lambda (x) x) (lambda (y) y))"), ['#f']) # unspecified
        
    def test_numbers(self):
        "from R5RS 6.2.5"
        #self.assertEqual(self.tpes("(complex? 3+4i)"), ['#t'])
        #self.assertEqual(self.tpes("(complex? 3)"), ['#t'])
        #self.assertEqual(self.tpes("(real? 3)"), ['#t'])
        #self.assertEqual(self.tpes("(real? -2.5+0.0i)"), ['#t'])
        #self.assertEqual(self.tpes("(real? #e1e10)"), ['#t'])
        #self.assertEqual(self.tpes("(rational? 6/10)"), ['#t'])
        #self.assertEqual(self.tpes("(rational? 6/3)"), ['#t'])
        #self.assertEqual(self.tpes("(integer? 3+0i)"), ['#t'])
        #self.assertEqual(self.tpes("(integer? 3.0)"), ['#t'])
        #self.assertEqual(self.tpes("(integer? 8/4)"), ['#t'])
        
        #self.assertEqual(self.tpe("(max 3 4)"), [Number.make(4)])
        #self.assertEqual(self.tpe("(max 3.9 4)"), [Number.make(4.0)])
        
        self.assertEqual(self.tpe("(+ 3 4)"), [Number.make(7)])
        self.assertEqual(self.tpe("(+ 3)"), [Number.make(3)])
        self.assertEqual(self.tpe("(+)"), [Number.make(0)])
        self.assertEqual(self.tpe("(* 4)"), [Number.make(4)])
        self.assertEqual(self.tpe("(*)"), [Number.make(1)])
        
        self.assertEqual(self.tpe("(- 3 4)"), [Number.make(-1)])
        self.assertEqual(self.tpe("(- 3 4 5)"), [Number.make(-6)])
        self.assertEqual(self.tpe("(- 3)"), [Number.make(-3)])
        self.assertEqual(self.tpe("(/ 3 4 5)"), [Number.make(3/20.0)]) # orig.: 3/20
        self.assertEqual(self.tpe("(/ 3)"), [Number.make(1/3.0)]) # orig.: 1/3

        self.assertEqual(self.tpe("(abs -7)"), [Number.make(7)])
        
        #self.assertEqual(self.tpes("(= 3 (+ (* 5 (quotient 3 5)) (remainder 3 5)))"), ['#t'])
        
        self.assertEqual(self.tpe("(modulo 13 4)"), [Number.make(1)])
        #self.assertEqual(self.tpe("(remainder 13 4)"), [Number.make(1)])
        self.assertEqual(self.tpe("(modulo -13 4)"), [Number.make(3)])
        #self.assertEqual(self.tpe("(remainder -13 4)"), [Number.make(-1)])
        self.assertEqual(self.tpe("(modulo 13 -4)"), [Number.make(-3)])
        #self.assertEqual(self.tpe("(remainder 13 -4)"), [Number.make(1)])
        self.assertEqual(self.tpe("(modulo -13 -4)"), [Number.make(-1)])
        #self.assertEqual(self.tpe("(remainder -13 -4)"), [Number.make(-1)])
        #self.assertEqual(self.tpe("(remainder -13 -4.0)"), [Number.make(-1.0)])
        
        #self.assertEqual(self.tpe("(gcd 32 -36)"), [Number.make(4)])
        #self.assertEqual(self.tpe("(gcd)"), [Number.make(0)])
        #self.assertEqual(self.tpe("(lcm 32 -36)"), [Number.make(288)])
        #self.assertEqual(self.tpe("(lcm 32.0 -36)"), [Number.make(288.0)])
        #self.assertEqual(self.tpe("(lcm)"), [Number.make(1)])
        
        #self.assertEqual(self.tpe("(numerator (/ 6 4))"), [Number.make(3)])
        #self.assertEqual(self.tpe("(denominator (/ 6 4))"), [Number.make(2)])
        #self.assertEqual(self.tpe("(denominator (exact->inexact (/ 6 4)))"), [Number.make(2.0)])
        
        #self.assertEqual(self.tpe("(floor -4.3)"), [Number.make(-5.0)])
        #self.assertEqual(self.tpe("(ceiling -4.3)"), [Number.make(-4.0)])
        #self.assertEqual(self.tpe("(truncate -4.3)"), [Number.make(-4.0)])
        #self.assertEqual(self.tpe("(round -4.3)"), [Number.make(-4.0)])
        
        #self.assertEqual(self.tpe("(floor 3.5)"), [Number.make(3.0)])
        #self.assertEqual(self.tpe("(ceiling 3.5)"), [Number.make(4.0)])
        #self.assertEqual(self.tpe("(truncate 3.5)"), [Number.make(3.0)])
        #self.assertEqual(self.tpe("(round 3.5)"), [Number.make(4.0)])
        
        #self.assertEqual(self.tpe("(round 7/2)"), [Number.make(4)])
        #self.assertEqual(self.tpe("(round 7)"), [Number.make(7)])
        
        #self.assertEqual(self.tpe("(rationalize (inexact->exact .3) 1/10)"), [Number.make(1/3)]) # orig.: 1/3
        #self.assertEqual(self.tpe("(rationalize .3 1/10)"), [Number.make(1/3)]) # orig.: #i1/3
        
        
        
        