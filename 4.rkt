;; Skeleton of Algae
;; Complete Coverage: finished, cannot cover 'eval-number' error now
;; Fixing Arithmetics
;;            operators behave like they do in racket
;;            made them robust
;;            TODO :: add more cornor tests
;; In Progress :: Adding Booleans and Conditionals
;;                Entend the Algea BNF
;;                Add new variants to the ALGAE type definition
;;                Extend parse-sexpr
;;                Update subst and eval
;;                Change eval type
;;                Update value-algae
;;                Extend BNF
;;                Change subst and eval again
;;                Add eval-boolean
;; TODO :: Further Extensions

#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | { < <ALGAE>}
               | { = <ALGAE>}
               | { <= <ALGAE>}
               | <id>
               | True
               | False
               | { if <ALGEA> <ALGEA> <ALGAE>}
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
    [(list '- fst (sexpr: args) ...)
     (Sub (parse-sexpr fst) (parse-sexprs args))]
    [(list '/ fst (sexpr: args) ...)
     (Div (parse-sexpr fst) (parse-sexprs args))]
    [(list '< lhs rhs) (Less (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '<= lhs rhs) (LessEq (parse-sexpr lhs) (parse-sexpr rhs))]
    [(cons 'if more)
     (match sexpr
       [(list 'if con rest resf) (If (parse-sexpr con)
                                     (parse-sexpr rest)
                                     (parse-sexpr resf))]
       [else (error `parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <ALGAE>s, `x' is some <id>, `y' is a
   *different* <id>, `B')
      N[v/x]                = N
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      {< E1 E2}[v/x]        = {< E1 E2}
      {= E1 E2}[v/x]        = {= E1 E2}
      {<= E1 E2}[v/x]       = {<= E1 E2}
      {if B E1 E2}[v/x]     = E1 if B is True
                            = E2 else
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
     eval(N)            = N
     eval({+ E ...})    = evalN(E) + ...
     eval({* E ...})    = evalN(E) * ...
     eval({- E})        = -evalN(E)
     eval({/ E})        = 1/evalN(E)
     eval({- E1 E ...}) = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...}) = evalN(E1) / (evalN(E) * ...)
     eval({< E1 E2})    = true if eval(E1) < eval(E2)
                        = false else
     eval({= E1 E2})    = true if eval(E1) = eval(E2)
                        = false else
     eval({<= E1 E2})   = true if eval(E1) <= eval(E2)
                        = false else
     eval(id)           = error!
     eval({if B E1 E2}) = E1 if eval(B) is true
                        = E2 if eval(B) is false
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E) = eval(E) if it is a number, error otherwise
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
    [(Add args) (if (null? args)
                    (error 'eval "no arguments for addition")
                    (foldl + 0 (map eval-number args)))]
    [(Mul args) (if (null? args)
                    (error 'eval "no arguments for multiplication")
                    (foldl * 1 (map eval-number args)))]
    [(Sub fst args) (if (null? args)
                        (error 'eval "one arguments for subtraction")
                        (- (eval-number fst)
                           (foldl + 0 (map eval-number args))))]
    [(Div fst args) (let ([largs (map eval-number args)])
                      (cond
                       [(null? args)
                        (error 'eval "one arguments for division")]
                       [(ormap zero? largs)
                       (error 'eval "cannot divide by zero")]
                      [else (/ (eval-number fst)
                               (foldl * 1 largs))]))]
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
(test (run "{+}") =error> "no arguments for addition")
(test (run "{*}") =error> "no arguments for multiplication")
(test (run "{- 5}") =error> "one arguments for subtraction")
(test (run "{/ 10}") =error> "one arguments for division")
(test (run "{/ 100 2 4 5 0}") =error> "cannot divide by zero")


;; test for boolean
(test (run "{< 5 3}") => #f)
(test (run "{< 1 2}") => #t)
(test (run "{= {with {x 5} x} {with {x 6} {- x 1}}}") => #t)
(test (run "{if True {with {x 5} x} {with {x 10} x}}") => 5)
(test (run "{if {+ 5 2} 10 20}")
      =error> "need a boolean when evaluating (Add ((Num 5) (Num 2))), but got 7")
(test (run "{< 5 {< 1 2}}")
      =error> "need a number when evaluating (Less (Num 1) (Num 2)), but got #t")
