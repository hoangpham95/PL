#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | { < <ALGAE> <ALGAE> }
               | { = <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
               | <id>
               | True
               | False
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { not <ALGAE> }
               | { and <ALGAE> <ALGAE> }
               | { or  <ALGAE> <ALGAE> }
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num  Number]
  [Bool Boolean]
  [Add  (Listof ALGAE)]
  [Mul  (Listof ALGAE)]
  [Sub  ALGAE (Listof ALGAE)]
  [Div  ALGAE (Listof ALGAE)]
  [Less ALGAE ALGAE]
  [Equal ALGAE ALGAE]
  [LessEq ALGAE ALGAE]
  [If   ALGAE ALGAE ALGAE]
  [Id   Symbol]
  [With Symbol ALGAE ALGAE])

(: parse-sexpr : Sexpr -> ALGAE)
;; to convert s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    ['True (Bool #t)]
    ['False (Bool #f)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ (sexpr: args) ...) (Add (parse-sexprs args))]
    [(list '* (sexpr: args) ...) (Mul (parse-sexprs args))]
    [(list '- (sexpr: args) ...) 
     (if (null? args)
         (error 'parse-sexpr "need at least one argument for -")
         (Sub (parse-sexpr (first args)) (parse-sexprs (rest args))))]
    [(list '/ (sexpr: args) ...) 
     (if (null? args)
         (error 'parse-sexpr "need at least one argument for /")
          (Div (parse-sexpr (first args)) (parse-sexprs (rest args))))]
    [(list '< lhs rhs) (Less (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '<= lhs rhs) (LessEq (parse-sexpr lhs) (parse-sexpr rhs))]
    [(cons 'if more)
     (match sexpr
       [(list 'if con rest resf) (If (parse-sexpr con)
                                     (parse-sexpr rest)
                                     (parse-sexpr resf))]
       [else (error `parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(list 'not arg) (Not (parse-sexpr arg))]
    [(list 'and arg1 arg2) (And (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'or arg1 arg2) (Or (parse-sexpr arg1) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E', `E1', `E2', etc. are <ALGAE>s, `x' is some <id>,
   `y' is a *different* <id>, `B' is a True/False)
      N[v/x]                = N
      B[v/x]                = B
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      {< E1 E2}[v/x]        = {< E1[v/x] E2[v/x]}
      {= E1 E2}[v/x]        = {= E1[v/x] E2[v/x]}
      {<= E1 E2}[v/x]       = {<= E1[v/x] E2[v/x]}
      {if E1 E2 E3}[v/x]    = E2[v/x] if E1[v/x] is True
                            = else E3[v/x]
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)        expr]
    [(Bool b)       expr]
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Less lhs rhs) (Less (subst* lhs) (subst* rhs))]
    [(Equal lhs rhs) (Equal (subst* lhs) (subst* rhs))]
    [(LessEq lhs rhs) (LessEq (subst* lhs) (subst* rhs))]
    [(If con rest resf) (If (subst* con)
                            (subst* rest)
                            (subst* resf))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
               bound-body
               (subst* bound-body)))]))

#| Formal specs for `eval':
   (`N' is a <num>, `E', `E1', `E2', etc. are <ALGAE>s, `x' is some <id>,
   `B' is a True/False)
     eval(N)                = N
     eval(B)                = B
     eval({+ E ...})        = evalN(E) + ...
     eval({* E ...})        = evalN(E) * ...
     eval({- E})            = -evalN(E)
     eval({/ E})            = 1/evalN(E)
     eval({- E1 E ...})     = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...})     = evalN(E1) / (evalN(E) * ...)
     eval({< E1 E2})        = error if either evalN(E1) or evalN(E2) is not a 
                              number
                            = true if evalN(E1) < evalN(E2)
                            = else false
     eval({= E1 E2})        = error if either evalN(E1) or evalN(E2) is not a 
                              number
                            = true if evalN(E1) = evalN(E2)
                            = else false
     eval({<= E1 E2})       = error if either evalN(E1) or evalN(E2) is not a 
                              number
                            = true if evalN(E1) <= evalN(E2)
                            = else false
     eval(id)               = error!
     eval({if E1 E2 E3})    = eval(E1) if evalB(E1) is true
                            = eval(E2) if evalB(E1) is false
                            = else error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E)               = eval(E) if it is a number, error otherwise
     evalB(E)               = eval(E) if it is a boolean, error otherwise
     eval({not E})          = eval({if E False True})
     eval({and E1 E2})      = eval({if {not E1} True E2})
     eval({or E1 E2})       = eval({if E1 True E2)
|#

(: eval-number : ALGAE -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr)
  (let ([result (eval expr)])
    (if (number? result)
        result
        (error 'eval-number "need a number when evaluating ~s, but got ~s"
               expr result))))

(: eval-boolean : ALGAE -> Boolean)
;; helper for `eval': verifies that the result is a boolean
(define (eval-boolean expr)
  (let ([result (eval expr)])
    (if (boolean? result)
        result
        (error 'eval-boolean "need a boolean when evaluating ~s, but got ~s"
               expr result))))

(: Not : ALGAE -> ALGAE)
;; fake binding for Not: translate to actual syntax
(define (Not expr)
  (If expr (Bool #f) (Bool #t)))

(: And : ALGAE ALGAE -> ALGAE)
;; fake binding for And
(define (And expr1 expr2)
  (If (Not expr1)
      (Bool #f)
      expr2))

(: Or : ALGAE ALGAE -> ALGAE)
;; fake binding for Or
(define (Or expr1 expr2)
  (If expr1
      (Bool #t)
      expr2))

(: value->algae : (U Number Boolean) -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number? val) (Num val)]
        [(boolean? val) (Bool val)]
        ;; Note: since we use Typed Racket, the type checker makes sure
        ;; that this function is never called with something that is not
        ;; in its type, so there's no need for an `else' branch.
        ;; (Strictly speaking, there's no need for the last predicate
        ;; (which is the only one here until you extend this), but it's
        ;; left in for clarity)
        ;; [else (error 'value->algae "unexpected value: ~s" val)]
        ))
;; The following test is also not needed.  In the untyped version, it
;; was needed because the error could not be achieved through `eval' --
;; which is exactly why the above type works.
;; ;; test for an otherwise unreachable error:
;; (test (value->algae null) =error> "unexpected value")

(: eval : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Bool b) b]
    [(Add args) (foldl + 0 (map eval-number args))]
    [(Mul args) (foldl * 1 (map eval-number args))]
    [(Sub fst args) (if (null? args)
                        (- (eval-number fst))
                        (- (eval-number fst)
                           (foldl + 0 (map eval-number args))))]
    [(Div fst args) (let ([eargs (map eval-number args)]
                          [efst (eval-number fst)])
                      (cond
                        [(ormap zero? (cons efst eargs))
                         (error 'eval "cannot divide by zero")]
                        [(null? args)
                         (/ efst)]
                        [else (/ efst (foldl * 1 eargs))]))]
    [(Less lhs rhs) (< (eval-number lhs) (eval-number rhs))]
    [(Equal lhs rhs) (= (eval-number lhs) (eval-number rhs))]
    [(LessEq lhs rhs) (<= (eval-number lhs) (eval-number rhs))]
    [(If con rest resf) (if (eval-boolean con)
                            (eval rest)
                            (eval resf))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (U Number Boolean))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)


;; test for complete coverage
(test (run "{with {x 20} {with {y 10}{/ x {* y 2}}}}") => 1)
(test (run "{with {x 20} {* x y}}")
      =error> "free identifier: y")
(test (run "{with x 5 {+ x x}}")
      =error> "bad `with' syntax in (with x 5 (+ x x))")
(test (run "{bleh}")
      =error> "bad syntax in (bleh)")

;; test for fixing arithmetics
;; may have some cornor cases untested
(test (run "{+ 1 2 3 4}") => 10)
(test (run "{- 10 1 2 3}") => 4)
(test (run "{* 1 2 3 4}") => 24)
(test (run "{/ 20 2 5}") => 2)
(test (run "{+}") => 0)
(test (run "{+ 1}") => 1)
(test (run "{*}") => 1)
(test (run "{* 5}") => 5)
(test (run "{- 5}") => -5)
(test (run "{/ 10}") => 1/10)
(test (run "{-}") =error> "need at least one argument for -")
(test (run "{/}") =error> "need at least one argument for /")
(test (run "{/ 0}") =error> "cannot divide by zero")
(test (run "{/ 100 2 4 5 0}") =error> "cannot divide by zero")

;; test for boolean
(test (run "True") => #t)
(test (run "False") => #f)
(test (run "{< 5 3}") => #f)
(test (run "{< 1 2}") => #t)
(test (run "{= {with {x 5} x} {with {x 6} {- x 1}}}") => #t)
(test (run "{if False {with {x 5} {< x 10}} {with {x 10} {<= x 10}}}") => #t)
(test (run "{with {x True} {if x True False}}") => #t)
(test (run "{with {x 10}
                  {if {= x {+ 3 9}}
                      {< x 20}
                      {= x 11}}}") => #f)
(test (run "{and True 123}") => 123)
(test (run "{and {with {x 1} {= x {/ 5 2}}} {/ 5 0}}") => #F)
(test (run "{or {not False} {/ 5 0}}") => #t)
(test (run "{and True {< 1 0}}") => #f)
(test (run "{or {not True} {and True {< 1 0}}}") => #f)
(test (run "{if {+ 5 2} 10 20}")
      =error> (string-append "need a boolean when evaluating (Add ((Num 5) " 
                             "(Num 2))), but got 7"))
(test (run "{< 5 {< 1 2}}")
      =error> (string-append "need a number when evaluating (Less (Num 1) " 
                             "(Num 2)), but got #t"))
(test (run "{if bleh}") =error> "bad `if' syntax in (if bleh)")


(define minutes-spent 300)
