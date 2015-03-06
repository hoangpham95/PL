#lang pl 10
;; TODO Multiple Arguments part:
;;  - Flesh out rewrite
;;  - Write type definition for define/rec ackerman
;;
;;      Mutual Recursion part:
;;  - um, all of it :)


;; Plain Racket Y Combinator
(define (Y f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (z) ((x x) z))))))

(rewrite (define/rec (f x ...) E)
;; TODO :: typed definition
         => (define f
              (let ([g (Y (lambda (f)
                            (lambda (_)
                              (lambda (x ...)
                                (let ([f (f #f)])
                                  E)))))])
                (g #f))))

(define/rec (ackermann m n)
;; TODO :: type definition
  (cond [(zero? m) (+ n 1)]
        [(zero? n) (ackermann (- m 1) 1)]
        [else      (ackermann (- m 1) (ackermann m (- n 1)))]))

(define/rec (fib a b count)
  (if (zero? count)
      b
      (fib (+ a b) a (- count 1))))

(rewrite (letfuns ([(f x ...) E] ...) B)
;; TODO :: type definition
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            (match name
                             ['f
                               (lambda (x ...)
                                 (let ([f (funs 'f)] ...)
                                   E))] ...))))])
              (let ([f (g 'f)] ...)
                B)))

;; tests
(test (ackermann 3 3) => 61)
(test (fib 1 0 5) => 5)

;; full coverage for even? odd?
(test (letfuns ([(even? n) (if (= n 0) #t (odd?  (- n 1)))]
                [(odd?  n) (if (= n 0) #f (even? (- n 1)))])
               (and (even? 122) (even? 123)))
               => #f)

;; an extended example
(define scan
  (letfuns ([(start str)  (loop (explode-string str) 0)]
            [(loop l n)   (match l
                            [(list)
                             (zero? n)]
                            [(cons 'open more)
                             (loop more (add1 n))]
                            [(cons 'close more)
                             (and (> n 0) (loop more (sub1 n)))]
                            [(cons (number: m) more)
                             (nums more m n)]
                            [(cons _ more)
                             (loop more n)])]
            [(nums l m n) (match l
                            [(cons (number: m1) more)
                             (and (< m m1) (nums more m1 n))]
                            [else (loop l n)])])
    start))
(test (scan "(()123(246x12)) (blah)"))
(test (not (scan "(1232)")))
(test (not (scan "()(")))
(test (not (scan "())")))