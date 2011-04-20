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
        
    def t(self, txt):
        "Tokenize"
        return list(self.tokenizer.tokenizeText(txt))
        
    def tp(self, txt):
        "Tokenize+Parse"
        return list(self.frame.parseTokens(self.tokenizer.tokenizeText(txt)))
        
    def tpe(self, txt):
        "Tokenize+Parse+Evaluate"
        return list(self.frame.evaluateExpressions(self.frame.parseTokens(self.tokenizer.tokenizeText(txt))))
        
    def tpes(self, txt):
        "Tokenize+Parse+Evaluate+String"
        return self.s(self.frame.evaluateExpressions(self.frame.parseTokens(self.tokenizer.tokenizeText(txt))))
        
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
        self.assertEqual(list(map(type, e)), [String, String, ExpressionError])
        e = self.tp('#t #f')
        self.assertEqual(e, [Boolean.make(True), Boolean.make(False)])
        self.assertEqual(list(map(type, e)), [Boolean, Boolean])

    def test_021_parse(self):
        e = self.tp('(define plus +)(define (b a) (plus a a))') #[0].eval(f)
        #e = []
        #for n in e2:
        #    e.append(n.eval(f))
        self.assertEqual(e, [Pair.makeFromList([Symbol.make('define'), Symbol.make('plus'), Symbol.make('+')]), \
            Pair.makeFromList([Symbol.make('define'), Pair.makeFromList([Symbol.make('b'), Symbol.make('a')]),Pair.makeFromList([Symbol.make('plus'), Symbol.make('a'), Symbol.make('a')])])])

    def test_parseList(self):
        self.assertEqual(self.tpes("'()"), ["()"])
        self.assertEqual(self.tpes("'(1)"), ["(1)"])
        self.assertEqual(self.tpes("'(1 2)"), ["(1 2)"])
        self.assertEqual(self.tpes("'(1 2 . ())"), ["(1 2)"])
        self.assertEqual(self.tpes("'(1 . 2)"), ["(1 . 2)"])
        self.assertEqual(type(self.tpe("'(1 2 . )")[0]), ExpressionError)
        self.assertEqual(type(self.tpe("'(1 2 .)")[0]), ExpressionError)
        self.assertEqual(type(self.tpe("'(. 1)")[0]), ExpressionError)
        self.assertEqual(type(self.tpe("'( . 1)")[0]), ExpressionError)
        self.assertEqual(type(self.tpe("'( . )")[0]), ExpressionError)
        self.assertEqual(type(self.tpe("'(1 . 2 3)")[0]), ExpressionError)
        self.assertEqual(type(self.tpe("'(1 .a)")[0]), ExpressionError)
        self.assertEqual(self.tpes("'(1 2 . (+ 1 2))"), ["(1 2 + 1 2)"])
        self.assertEqual(self.tpes("'(1 2 (+ 1 2))"), ["(1 2 (+ 1 2))"])
        self.assertEqual(self.tpes("`(1 2 . ,(+ 1 2))"), ["(1 2 . 3)"])
        self.assertEqual(self.tpes("`(1 2 ,(+ 1 2))"), ["(1 2 3)"])
        self.assertEqual(type(self.tpe("'(1 2 . (+ 1 2) 3)")[0]), ExpressionError) # could work but made illegal
        self.assertEqual(type(self.tpe("'(1 2 . (+ 1 2) . 3)")[0]), ExpressionError) # could work but made illegal
        
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
        self.assertTrue(isinstance(self.tpe('a')[0], ExpressionError))

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
    (cond ((= n 0) 0)
	  ((= n 1) 1)
	  (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))
(define (even? n)
    (if (= n 0)
        #t
        (odd? (- n 1))))
        """
        self.assertEqual(self.tpes("(fac 12)"), ['479001600'])
        #broken in python 2.x (python 3 uses 'ints', python 2 requires 'longs')
        #self.assertEqual(self.tpes("(fac 200)"), ['788657867364790503552363213932185062295135977687173263294742533244359449963403342920304284011984623904177212138919638830257642790242637105061926624952829931113462857270763317237396988943922445621451664240254033291864131227428294853277524242407573903240321257405579568660226031904170324062351700858796178922222789623703897374720000000000000000000000000000000000000000000000000'])
        self.assertEqual(self.tpes("(fib 0)"), ['0'])
        self.assertEqual(self.tpes("(fib 13)"), ['233'])
        self.assertEqual(self.tpes("(even? 1002)"), ['#t'])
        self.assertEqual(self.tpes("(odd? 1002)"), ['#f'])
        
    def test_continuations(self):
        "tests if continuations work"
        prog = """
(define (identity x) x)
        """
        self.assertEqual(type(self.tpe("(call/cc identity)")[0]), Continuation)
        self.assertEqual(type(self.tpe("(call-with-current-continuation identity)")[0]), Continuation)
        
        
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
        
        
    def test_conditionals(self):
        "from R5RS 4.2.1"
        s = """
  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less))
        """
        self.assertEqual(self.tpe(s), [Symbol.make('greater')])
        s = """
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal))
        """
        self.assertEqual(self.tpe(s), [Symbol.make('equal')])
        s = """
  (cond ((assv 'b '((a 1) (b 2))) => cadr)
        (else #f))
        """
        #self.assertEqual(self.tpe(s), [Number.make(2)])
        s = """
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite))
        """
        #self.assertEqual(self.tpe(s), [Symbol.make('composite')])
        s = """
  (case (car '(c d))
    ((a) 'a)
    ((b) 'b))        """
        #self.assertEqual(self.tpe(s), [Symbol.make('composite')]) # unspecified
        s = """
  (case (car '(c d))
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else 'consonant))        """
        #self.assertEqual(self.tpe(s), [Symbol.make('consonant')])
        self.assertEqual(self.tpes("(and (= 2 2) (> 2 1))"), ['#t'])
        self.assertEqual(self.tpes("(and (= 2 2) (< 2 1))"), ['#f'])
        self.assertEqual(self.tpes("(and 1 2 'c '(f g))"), ['(f g)'])
        self.assertEqual(self.tpes("(and)"), ['#t'])

        self.assertEqual(self.tpes("(or (= 2 2) (> 2 1))"), ['#t'])
        self.assertEqual(self.tpes("(or (= 2 2) (< 2 1))"), ['#t'])
        self.assertEqual(self.tpes("(or #f #f #f)"), ['#f'])
        #self.assertEqual(self.tpes("(or (memq 'b '(a b c)) (/ 3 0))"), ['(b c)'])

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
        
    def test_quasiquotation(self):
        "from R5RS 4.2.6"
        self.assertEqual(self.tpes("`(list ,(+ 1 2) 4)"), ['(list 3 4)'])
        self.assertEqual(self.tpes("(let ((name 'a)) `(list ,name ',name))"), ["(list a 'a)"]) #'(list a (quote a))'
        self.assertEqual(self.tpes("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"), ['(a 3 4 5 6 b)'])
        #self.assertEqual(self.tpes("`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))"), ['((foo 7) . cons)'])
        #self.assertEqual(self.tpes("`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"), ['#(10 5 2 4 3 8)'])

        #self.assertEqual(self.tpes("`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"), ["(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"])
        s = """
(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))           
        """
        #self.assertEqual(self.tpes(s), ["(a `(b ,x ,'y d) e)"])
        self.assertEqual(self.tpes("(quasiquote (list (unquote (+ 1 2)) 4))"), ["(list 3 4)"])
        self.assertEqual(self.tpes("'(quasiquote (list (unquote (+ 1 2)) 4))"), ["`(list ,(+ 1 2) 4)"]) #(quasiquote (list (unquote (+ 1 2)) 4))
        
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
        self.assertEqual(self.tpes("(eq? (list 'a) (list 'a))"), ['#f'])
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
        
        self.assertEqual(self.tpes("(= 3 (+ (* 5 (quotient 3 5)) (remainder 3 5)))"), ['#t'])
        
        self.assertEqual(self.tpe("(modulo 13 4)"), [Number.make(1)])
        self.assertEqual(self.tpe("(remainder 13 4)"), [Number.make(1)])
        self.assertEqual(self.tpe("(modulo -13 4)"), [Number.make(3)])
        self.assertEqual(self.tpe("(remainder -13 4)"), [Number.make(-1)])
        self.assertEqual(self.tpe("(modulo 13 -4)"), [Number.make(-3)])
        self.assertEqual(self.tpe("(remainder 13 -4)"), [Number.make(1)])
        self.assertEqual(self.tpe("(modulo -13 -4)"), [Number.make(-1)])
        self.assertEqual(self.tpe("(remainder -13 -4)"), [Number.make(-1)])
        self.assertEqual(self.tpe("(remainder -13 -4.0)"), [Number.make(-1.0)])
        
        self.assertEqual(self.tpe("(gcd 32 -36)"), [Number.make(4)])
        self.assertEqual(self.tpe("(gcd)"), [Number.make(0)])
        self.assertEqual(self.tpe("(lcm 32 -36)"), [Number.make(288)])
        self.assertEqual(self.tpe("(lcm 32.0 -36)"), [Number.make(288.0)])
        self.assertEqual(self.tpe("(lcm)"), [Number.make(1)])
        
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
        
        
        
        
