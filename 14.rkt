#lang pl 14

;;; ==================================================================
;;; Syntax

#| The BNF:
   <TOY> ::= <num>
           | <id>
           | { set! <id> <TOY> }
           | { bind {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { bindrec {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { fun { <id> ... } <TOY> <TOY> ... }
           | { rfun { <id> ... } <TOY> <TOY> ... }
           | { if <TOY> <TOY> <TOY> }
           | { <TOY> <TOY> ... }
|#

;; A matching abstract syntax tree datatype:
(define-type TOY
  [Num  Number]
  [Id   Symbol]
  [Set  Symbol TOY]
  [Bind (Listof Symbol) (Listof TOY) (Listof TOY)]
  [BindRec (Listof Symbol) (Listof TOY) (Listof TOY)]
  [Fun  (Listof Symbol) (Listof TOY)]
  [RFun (Listof Symbol) (Listof TOY)]
  [Call TOY (Listof TOY)]
  [If   TOY TOY TOY])

(: unique-list? : (Listof Any) -> Boolean)
;; Tests whether a list is unique, guards Bind and Fun values.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

(: parse-sexpr : Sexpr -> TOY)
;; to convert s-expressions into TOYs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'set! more)
     (match sexpr
       [(list 'set! (symbol: name) new) (Set name (parse-sexpr new))]
       [else (error 'parse-sexpr "bad `set!' syntax in ~s" sexpr)])]
    [(cons (and binder (or 'bind 'bindrec)) more)
     (match sexpr
       [(list _ (list (list (symbol: names) (sexpr: nameds)) ...)
              (sexpr: body0) (sexpr: body) ...)
        (if (unique-list? names)
            ((if (eq? 'bind binder) Bind BindRec)
             names
             (map parse-sexpr nameds)
             (map parse-sexpr (cons body0 body)))
            (error 'parse-sexpr "duplicate `~s' names: ~s" binder names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s"
                    binder sexpr)])]
    [(cons (and funner (or 'fun 'rfun)) more)
     (match sexpr
       [(list _ (list (symbol: names) ...)
              (sexpr: body0) (sexpr: body) ...)
        (if (unique-list? names)
            ((if (eq? 'fun funner) Fun RFun)
             names
             (map parse-sexpr (cons body0 body)))
            (error 'parse-sexpr "duplicate `~s' names: ~s" funner names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s"
                    funner sexpr)])]
    [(cons 'if more)
     (match sexpr
       [(list 'if cond then else)
        (If (parse-sexpr cond) (parse-sexpr then) (parse-sexpr else))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(list fun (sexpr: args) ...) ; other lists are applications
     (Call (parse-sexpr fun)
           (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> TOY)
;; Parses a string containing an TOY expression to a TOY AST.
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;; ==================================================================
;;; Values and environments

(define-type ENV = (Listof (Listof (Boxof VAL))))

(define-type VAL
  [BogusV]
  [RktV  Any]
  [FunV  Natural (ENV -> VAL) ENV Boolean] ; `byref?' flag
  [PrimV ((Listof VAL) -> VAL)])


;; a single bogus value to use wherever needed
(define the-bogus-value (BogusV))

;;(: raw-extend : (Listof (Boxof VAL)) ENV -> ENV)
;; extends an environment with a new frame, given names and value
;; boxes
;;(define (raw-extend boxed-values env)
;;  (cons (map (lambda ([boxed-val : (Boxof VAL)])
;;               boxed-val)
;;             boxed-values)
;;        env))

;;(: extend : (Listof VAL) ENV -> ENV)
;;;; extends an environment with a new frame (given plain values).
;;(define (extend values env)
;;  (cons (map (inst box VAL) values) env))

(: extend-rec : (Listof (ENV -> VAL)) ENV -> ENV)
;; extends an environment with a new recursive frame (given compiled
;; expressions).
(define (extend-rec compiled-exprs env)
  (define new-env
    (cons (map (inst box VAL)
               (map (lambda (_) the-bogus-value) compiled-exprs))
          env))
  ;; note: no need to check the lengths here, since this is only
  ;; called for `bindrec', and the syntax make it impossible to have
  ;; different lengths
  (for-each (lambda ([env : (Boxof VAL)]
                     [compiled : (ENV -> VAL)])
              (set-box! env (compiled new-env)))
            (car new-env) compiled-exprs)
  new-env)

(: global-lookup : Symbol -> VAL)
;; looks for a name in an environment, searching through each frame.
(define (global-lookup name)
  (: lookup-helper : Symbol (Listof (List Symbol VAL)) -> VAL)
  (define (lookup-helper name env)
    (if (null? env)
        (error 'global-lookup "no binding for ~s" name)
        (let ([cell (assq name env)])
          (if cell
              (second cell)
              (lookup-helper name (cdr env))))))
  (lookup-helper name global-environment))

(: find-index : Symbol BINDINGS -> (U Boolean (List Natural Natural)))
;; to get the index that corresponds to some given name
(define (find-index x bindings)
  (: index : Symbol (Listof Symbol) Natural -> (U Boolean Natural))
  ;; helper function to find the index of a symbol in a list
  ;; assuming that the symbol is in the list
  (define (index sym l count)
    (cond [(null? l) #f]
          [(equal? sym (first l)) count]
          [else (index sym (rest l) (+ 1 count))]))
  (: find-index-helper : Symbol BINDINGS Natural ->
     (U Boolean (List Natural Natural)))
  ;; helper function for find-index
  (define (find-index-helper x bindings count)
    (if (null? bindings)
        #f
        (let ([1st (index x (car bindings) 0)])
          (if (number? 1st)
              (list count 1st)
              (find-index-helper x (cdr bindings) (+ 1 count))))))
  (find-index-helper x bindings 0))

(: unwrap-rktv : VAL -> Any)
;; helper for `racket-func->prim-val': unwrap a RktV wrapper in
;; preparation to be sent to the primitive function
(define (unwrap-rktv x)
  (cases x
    [(RktV v) v]
    [else (error 'racket-func "bad input: ~s" x)]))

(: racket-func->prim-val : Function -> VAL)
;; converts a racket function to a primitive evaluator function which
;; is a PrimV holding a ((Listof VAL) -> VAL) function.  (the
;; resulting function will use the list function as is, and it is the
;; list function's responsibility to throw an error if it's given a
;; bad number of arguments or bad input types.)
(define (racket-func->prim-val racket-func)
  (define list-func (make-untyped-list-function racket-func))
  (PrimV (lambda (args)
           (RktV (list-func (map unwrap-rktv args))))))

;; The global environment has a few primitives:
(: global-environment : (Listof (List Symbol VAL)))
(define global-environment
  (list (list '+ (racket-func->prim-val +))
        (list '- (racket-func->prim-val -))
        (list '* (racket-func->prim-val *))
        (list '/ (racket-func->prim-val /))
        (list '< (racket-func->prim-val <))
        (list '> (racket-func->prim-val >))
        (list '= (racket-func->prim-val =))
        ;; values
        (list 'true  (RktV #t))
        (list 'false (RktV #f))))

;;; ==================================================================
;;; Compilation
(define-type BINDINGS = (Listof (Listof Symbol)))

(: compiler-enabled? : (Boxof Boolean))
;; a global flag that can disable the compiler
(define compiler-enabled? (box #t))

(: compile-body : (Listof TOY) BINDINGS -> (ENV -> VAL))
;; compiles a list of expressions to a single Racket function.
(define (compile-body exprs bindings)
  (unless (unbox compiler-enabled?)
    (error 'compile-body "compiler disabled"))
  ;; this is the third option mentioned in the homework -- compile the
  ;; list of expressions into a single racket function.  (Note: relies
  ;; on the fact that the body is never empty.)
  (let ([compiled-1st (compile (first exprs) bindings)]
        [rest         (rest exprs)])
    (if (null? rest)
        compiled-1st
        (let ([compiled-rest (compile-body rest bindings)])
          (lambda (env)
            (let ([ignored (compiled-1st env)])
              (compiled-rest env))))))
  ;; the same thing, but using `foldl' to do the loop
  ;; (foldl (lambda ([expr : TOY] [compiled-prev : (ENV -> VAL)])
  ;;          (let ([compiled (compile expr)])
  ;;            (lambda ([env : ENV])
  ;;              (let ([ignored (compiled-prev env)])
  ;;                (compiled env)))))
  ;;        (lambda (env) the-bogus-value)
  ;;        exprs)
  ;; a different approach (the second one listed in the homework):
  ;; (let ([compiled-exprs (map compile exprs)])
  ;;   (lambda (env)
  ;;     (: run-all : (Listof (ENV -> VAL)) -> VAL)
  ;;     (define (run-all c-exprs)
  ;;       (let ([1st  ((first c-exprs) env)]
  ;;             [rest (rest c-exprs)])
  ;;         (if (null? rest)
  ;;           1st
  ;;           (run-all rest))))
  ;;     (run-all compiled-exprs)))
  )

(: compile-get-boxes : (Listof TOY) BINDINGS ->
   (ENV -> (Listof (Boxof VAL))))
;; utility for applying rfun
(define (compile-get-boxes exprs bindings)
  (: compile-getter : TOY -> (ENV -> (Boxof VAL)))
  (define (compile-getter expr)
    (cases expr
      [(Id name)
       (let ([pos (find-index name bindings)])
         (lambda ([env : ENV])
           (if (list? pos)
               (list-ref (list-ref env (first pos))
                         (second pos))
               (let ([found? (global-lookup name)])
                 (when found? (error 'run-time 
                                     "trying to mutating global"))))))]
      [else
       (lambda ([env : ENV])
         (error 'call "rfun application with a non-identifier ~s"
                expr))]))
  (unless (unbox compiler-enabled?)
    (error 'compile-get-boxes "compiler disabled"))
  (let ([getters (map compile-getter exprs)])
    (lambda (env)
      (map (lambda ([get-box : (ENV -> (Boxof VAL))]) (get-box env))
           getters))))

(: compile : TOY BINDINGS -> (ENV -> VAL))
;; compiles TOY expressions to Racket functions.
(define (compile expr bindings)
  ;; convenient helper for running compiled code
  (: compile* :  TOY -> (ENV -> VAL))
  ;; helper that passes along the same bindings to compile
  (define (compile* expr) (compile expr bindings))
  (: boxed-runner : ENV -> ((ENV -> VAL) -> (Boxof VAL)))
  (define (boxed-runner env)
    (lambda (compiled) ((inst box VAL) (compiled env))))
  (unless (unbox compiler-enabled?)
    (error 'compile "compiler disabled"))
  (cases expr
    [(Num n)   (lambda ([env : ENV]) (RktV n))]
    [(Id name) (let ([pos (find-index name bindings)])
                 (lambda ([env : ENV])
                   (if (list? pos)
                       (unbox (list-ref (list-ref env
                                                  (first pos))
                                        (second pos)))
                       (global-lookup name))))]
    [(Set name new)
     (let ([compiled-new (compile* new)]
           [pos (find-index name bindings)])
       (lambda ([env : ENV])
         (if (list? pos)
             (set-box! (list-ref (list-ref env (first pos))
                                 (second pos))
                       (compiled-new env))
             (when (global-lookup name)
               (error 'compile "Trying to mutating a global")))
         the-bogus-value))]
    [(Bind names exprs bound-body)
     (let ([compiled-exprs (map compile* exprs)]
           [compiled-body  (compile-body bound-body (cons names bindings))])
       (lambda ([env : ENV])
         (compiled-body
          (cons (map (boxed-runner env) compiled-exprs) env))))]
    [(BindRec names exprs bound-body)
     (let* ([new-bindings (cons names bindings)]
            [compiled-exprs (map (lambda ([expr : TOY])
                                   (compile expr new-bindings)) exprs)]
            [compiled-body  (compile-body bound-body new-bindings)])
       (lambda ([env : ENV])
         (compiled-body (extend-rec compiled-exprs env))))]
    [(Fun names bound-body)
     (let ([compiled-body (compile-body bound-body (cons names bindings))]
           [body-length (length names)])
       (lambda ([env : ENV]) (FunV body-length compiled-body env #f)))]
    [(RFun names bound-body)
     (let ([compiled-body (compile-body bound-body (cons names bindings))]
           [body-length (length names)])
       (lambda ([env : ENV]) (FunV body-length compiled-body env #t)))]
    [(Call fun-expr arg-exprs)
     (let ([compiled-fun  (compile fun-expr bindings)]
           [compiled-args (map compile* arg-exprs)]
           [compiled-boxes-getter (compile-get-boxes arg-exprs bindings)]
           [arg-length (length arg-exprs)])
       (lambda ([env : ENV])
         (let ([fval (compiled-fun env)]
               ;; delay evaluating the arguments
               [arg-vals (lambda ()
                           (map (lambda ([compiled : (ENV -> VAL)]) 
                                  (compiled env)) 
                                compiled-args))]
               [boxed-arg-vals (lambda ()
                                 (map (boxed-runner env) compiled-args))])
           (cases fval
             [(PrimV proc) (proc (arg-vals))]
             [(FunV body-length compiled-body fun-env byref?)
              (if (= arg-length body-length)
                  (compiled-body (if byref?
                                     (cons (compiled-boxes-getter env)
                                           fun-env)
                                     (cons (boxed-arg-vals) fun-env)))
                  (error 'FunV "arity mismatch"))]
             [else (error 'call "function call with a non-function: ~s"
                          fval)]))))]
    [(If cond-expr then-expr else-expr)
     (let ([compiled-cond (compile* cond-expr)]
           [compiled-then (compile* then-expr)]
           [compiled-else (compile* else-expr)])
       (lambda ([env : ENV])
         ((if (cases (compiled-cond env)
                [(RktV v) v] ; Racket value => use as boolean
                [else #t])   ; other values are always true
              compiled-then
              compiled-else)
          env)))]))

(: run : String -> Any)
;; compiles and runs a TOY program contained in a string
(define (run str)
  (set-box! compiler-enabled? #t)
  (let ([compiled (compile (parse str) null)])
    (set-box! compiler-enabled? #f)
    (let ([result (compiled null)])
      (cases result
        [(RktV v) v]
        [else (error 'run "the program returned a bad value: ~s"
                     result)]))))

;;; ==================================================================
;;; Tests

(test (run "{{fun {x} {+ x 1}} 4}")
      => 5)

(test (run "{bind {{add3 {fun {x} {+ x 3}}}} {add3 1}}")
      => 4)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                   {add1 {fun {x} {+ x 1}}}}
              {bind {{x 3}} {add1 {add3 x}}}}")
      => 7)
(test (run "{bind {{identity {fun {x} x}}
                   {foo {fun {x} {+ x 1}}}}
              {{identity foo} 123}}")
      => 124)
(test (run "{bind {{x 3}}
              {bind {{f {fun {y} {+ x y}}}}
                {bind {{x 5}}
                  {f 4}}}}")
      => 7)
(test (run "{{{fun {x} {x 1}}
              {fun {x} {fun {y} {+ x y}}}}
             123}")
      => 124)

;; More tests for complete coverage
(test (run "{bind x 5 x}")      =error> "bad `bind' syntax")
(test (run "{fun x x}")         =error> "bad `fun' syntax")
(test (run "{if x}")            =error> "bad `if' syntax")
(test (run "{}")                =error> "bad syntax")
(test (run "{bind {{x 5} {x 5}} x}") =error> "duplicate*bind*names")
(test (run "{fun {x x} x}")     =error> "duplicate*fun*names")
(test (run "{+ x 1}")           =error> "no binding for")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{1 2}")             =error> "with a non-function")
(test (run "{{fun {x} x}}")     =error> "arity mismatch")
(test (run "{if {< 4 5} 6 7}")  => 6)
(test (run "{if {< 5 4} 6 7}")  => 7)
(test (run "{if + 6 7}")        => 6)
(test (run "{fun {x} x}")       =error> "returned a bad value")
(test (run "{set! + /}")        =error> "Trying") ;;compile-time
(test (run "{{rfun {x} {+ x 1}} +}") =error> "trying") ;;run-time
(test (run "{{rfun {x} {+ x 1}} y}") =error> "no binding")

;; assignment tests
(test (run "{set! {+ x 1} x}")  =error> "bad `set!' syntax")
(test (run "{bind {{x 1}} {set! x {+ x 1}} x}") => 2)

;; `bindrec' tests
(test (run "{bindrec {x 6} x}") =error> "bad `bindrec' syntax")
(test (run "{bindrec {{frac {fun {n}
                            {if {= 0 n}
                                1
                                {* n {frac {- n 1}}}}}}}
              {frac 5}}") => 120)

;; tests for multiple expressions and assignment
(test (run "{bind {{make-counter
                     {fun {}
                       {bind {{c 0}}
                         {fun {}
                           {set! c {+ 1 c}}
                           c}}}}}
              {bind {{c1 {make-counter}}
                     {c2 {make-counter}}}
                {* {c1} {c1} {c2} {c1}}}}")
      => 6)
(test (run "{bindrec {{foo {fun {}
                             {set! foo {fun {} 2}}
                             1}}}
              {+ {foo} {* 10 {foo}}}}")
      => 21)

;; `rfun' tests
(test (run "{{rfun {x} x} 4}") =error> "non-identifier")
(test (run "{bind {{swap! {rfun {x y}
                            {bind {{tmp x}}
                              {set! x y}
                              {set! y tmp}}}}
                   {a 1}
                   {b 2}}
              {swap! a b}
              {+ a {* 10 b}}}")
      => 12)

;; test that argument are not evaluated redundantly
(test (run "{{rfun {x} x} {/ 4 0}}") =error> "non-identifier")
(test (run "{5 {/ 6 0}}") =error> "non-function")

;; test compiler-disabled flag, for complete coverage
;; (these tests must use the functions instead of the toplevel `run',
;; since there is no way to get this error otherwise, this indicates
;; that this error should not occur outside of our code -- it is an
;; internal error check)
(test (compile (Num 1) null) =error> "compiler disabled")
(test (compile-body (list (Num 1)) null) =error> "compiler disabled")
(test (compile-get-boxes (list (Num 1)) null) =error> "compiler disabled")

;; test for find-index
;;(test (find-index 'a '((a b c) () (c d e))) => '(0 0))
;;(test (find-index 'e '((a b c) () (c d e))) => '(2 2))
;;(test (find-index 'c '((a b c) () (c d e))) => '(0 2))
;;(test (find-index 'x '((a b c) () (c d e))) => #f)


#|
(time (run "{bindrec {{fib {fun {n}
                            {if {< n 2}
                              n
                              {+ {fib {- n 1}}
                                  {fib {- n 2}}}}}}}
              {fib 27}}"))

;; original TOY 1589ms
;; basic compiler 1135ms
;; This version 694ms
|#
;;; ==================================================================

(define minutes-spent 720)