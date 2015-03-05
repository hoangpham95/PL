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

;; Don't know if these will be helpful, but i figured I'd copy them in
;;; First try
;(define ackermann
;  (Y (lambda (ackermann)
;       (lambda (m n)
;         (cond [(zero? m) (+ n 1)]
;               [(zero? n) (ackermann (- m 1) 1)]
;               [else      (ackermann (- m 1)
;                                     (ackermann m (- n 1)))])))))
;;; Second try
;(define ackermann
;  (let ([g (Y (lambda (ackermann) 
;                (lambda (m n)
;                  (cond [(zero? m) (+ n 1)]
;                        [(zero? n) (ackermann (- m 1) 1)]
;                        [else      (ackermann (- m 1)
;                                              (ackermann m (- n 1)))]))))])
;    (lambda (x y) ((g x) y))))
;
;;; Third try
;(define ackermann
;  (let ([g (Y (lambda (ackermann)
;                (lambda (_) ; we ignore this argument
;                  (lambda (m n)
;                    (let ([ackermann (ackermann #f)])
;                      (lambda (m n)
;                        (cond [(zero? m) (+ n 1)]
;                              [(zero? n) (ackermann (- m 1) 1)]
;                              [else      (ackermann (- m 1)
;                                                    (ackermann m (- n 1)))])))))))])
;    (g #f)))

;; TODO: This function need to be finished
(rewrite (define/rec (f x ...) E)
         => (define f
              (let ([g (Y (lambda (f)
                            ...finish this part...))])
                (g #f))))

;; TODO: Put in type definition
(define/rec (ackermann m n)
  (cond [(zero? m) (+ n 1)]
        [(zero? n) (ackermann (- m 1) 1)]
        [else      (ackermann (- m 1) (ackermann m (- n 1)))]))

(test (ackermann 3 3) => 61)