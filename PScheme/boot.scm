1
(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (zero? x)
    (= x 0))

(define (positive? x)
    (> x 0))
    
(define (negative? x)
    (< x 0))
    
(define (even? x)
    (= (modulo x 2) 0))
    
(define (odd? x)
    (= (modulo x 2) 1))

(define (gcd m n)
    ( cond ( ( < m n ) ( gcd n m) )
        ( ( = (remainder m n ) 0) n)
           (#t (gcd n (remainder m n)))))

(define (caar l)   (car   (car l)))
(define (cadr l)   (car   (cdr l)))
(define (cdar l)   (cdr   (car l)))
(define (cddr l)   (cdr   (cdr l)))

(define (caaar l)  (caar  (car l)))
(define (caadr l)  (caar  (cdr l)))
(define (cadar l)  (cadr  (car l)))
(define (caddr l)  (cadr  (cdr l)))
(define (cdaar l)  (cdar  (car l)))
(define (cdadr l)  (cdar  (cdr l)))
(define (cddar l)  (cddr  (car l)))
(define (cdddr l)  (cddr  (cdr l)))

(define (caaaar l) (caaar (car l)))
(define (caaadr l) (caaar (cdr l)))
(define (caadar l) (caadr (car l)))
(define (caaddr l) (caadr (cdr l)))
(define (cadaar l) (cadar (car l)))
(define (cadadr l) (cadar (cdr l)))
(define (caddar l) (caddr (car l)))
(define (cadddr l) (caddr (cdr l)))
(define (cdaaar l) (cdaar (car l)))
(define (cdaadr l) (cdaar (cdr l)))
(define (cdadar l) (cdadr (car l)))
(define (cdaddr l) (cdadr (cdr l)))
(define (cddaar l) (cddar (car l)))
(define (cddadr l) (cddar (cdr l)))
(define (cdddar l) (cdddr (car l)))
(define (cddddr l) (cdddr (cdr l)))

(define ( newline) (write-char #\newline))

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

(define zero (- 1 1))
(define one (+ zero 1))
(define two (+ one one))

(define (fib2 n)
    (if (= n zero)
        zero
        (if (= n one)
            one
            (+ (fib2 (- n one)) (fib2 (- n two))))))

;(define mylist '( ( (a b) (c d) (e f) )   ; make a 3-level list
;                ( (g h) (i j) (k l) )
;                ( (m n) (o p) (q r) )
;                ( (s t) (u v) (w x) )
;                ) )

(define (odd n)
    (if (= n 0)
        #f
        (even (- n 1))))
(define (even n)
    (if (= n 0)
        #t
        (odd (- n 1))))

(define (identity x) x)

(define (append list1 . lists)
  (define (helper list1 lists)
    (if (null? lists)
	list1
	(append2 list1
		 (helper (car lists) (cdr lists)))))
  (helper list1 lists))
        
(define (reverse l)
  (define (helper result l)
    (if (null? l)
	result
	(helper (cons (car l) result) (cdr l))))
  (helper '() l))

(define (list-slow . l)
  (if (null? l)
      l
      (cons (car l)
	    (apply list-slow (cdr l)))))

(define (list . l)
  (if (null? l)
      l
      (reverse (reverse l))))

(define list-tail
  (lambda (x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1)))))
