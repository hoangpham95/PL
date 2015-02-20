#lang pl 06

#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> } <BRANG> }
            | { call <BRANG> <BRANG> }

   <CORE> ::= <num>
            | { + <CORE> <CORE> }
            | { - <CORE> <CORE> }
            | { * <CORE> <CORE> }
            | { / <CORE> <CORE> }
            | { with { <id> <CORE> } <CORE> }
            | <id>
            | { fun { <id> ... } <CORE> }
            | { call <CORE> <CORE> ... }

Preprocess rules:
  preprocess(N, env)               = (Cnum n)
  preprocess({+ E1 E2},env)        = preprocess(E1,env) + preprocess(E2,env)
  preprocess({- E1 E2},env)        = preprocess(E1,env) - preprocess(E2,env)
  preprocess({* E1 E2},env)        = preprocess(E1,env) * preprocess(E2,env)
  preprocess({/ E1 E2},env)        = preprocess(E1,env) / preprocess(E2,env)
  preprocess(id,env)               = list-ref(env,id)
  preprocess({with {x E1} E2},env) = <{CCall{CFun
                                            preprocess(E2,
                                                        extend(env, x)))
                                            preprocess(E1,env)}}>
  preprocess({fun {x ...} E},env)  = <{CFun {x} {CFun {x_2} ...} E}
  preprocess({call E1 E2 ...},env1)
                               = <{CCall {CCall {... E1} E2_n ...} E2_1}

Evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(CRef(0),env)          = list-ref(env,N)
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)
           = eval(Ef,extend(eval(E2,env1),env2))
                             if eval(E1,env1) = <{fun {x} Ef}, env2>
           = error!          otherwise
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Fun  (Listof Symbol) BRANG]
  [Call BRANG (Listof BRANG)])

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef  Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> BRANG)
;; to convert s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name) (symbol: names) ...) body)
        (Fun (cons name names) (parse-sexpr body) )]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg more ...) (Call (parse-sexpr fun)
                                         (cons (parse-sexpr arg)
                                               (map parse-sexpr more)))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for de-Bruijin environments, values
(define-type ENV = (Listof VAL))

(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

;; Types for DE_ENV, which map identifiers to integers
(define-type DE-ENV = Symbol -> Natural)

(: de-empty-env : DE-ENV)
(define (de-empty-env id)
  (error 'de-empty-env "no binding for ~s" id))

(: de-extend : DE-ENV Symbol -> DE-ENV)
(define (de-extend env id)
  (lambda (x)
    (if (eq? x id)
        0
        (+ 1 (env x)))))

(: NumV->number : VAL -> Number)
;; convert a BRANG runtime numeric value to a Racket one
(define (NumV->number v)
  (cases v
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" v)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: preprocess : BRANG DE-ENV -> CORE)
;; convert BRANG into CORE
;; using de-Bruijin indexes environment
(define (preprocess expr env)
  (cases expr
         [(Num n)    (CNum n)]
         [(Add l r)  (CAdd (preprocess l env) (preprocess r env))]
         [(Sub l r)  (CSub (preprocess l env) (preprocess r env))]
         [(Mul l r)  (CMul (preprocess l env) (preprocess r env))]
         [(Div l r)  (CDiv (preprocess l env) (preprocess r env))]
         [(Id name)  (CRef (env name))]
         [(With bound-id name-expr bound-body)
          (CCall (CFun (preprocess bound-body
                                   (de-extend env bound-id)))
                 (preprocess name-expr env))]
         [(Fun bound-ids bound-body)
           (currify-CFun
            (preprocess bound-body
                        (foldl
                         (lambda ([id : Symbol] [e : DE-ENV])
                           (de-extend e id))
                         env
                         bound-ids))
            bound-ids)]
         [(Call fun-expr arg-exprs)
           (currify-CCall
            (preprocess fun-expr env)
            arg-exprs
            env)]))

;; currify hepler for Fun and Call
(: currify-CFun : CORE (Listof Symbol) -> CORE)
(define (currify-CFun expr ids)
  (if (null? ids)
      expr
      (CFun (currify-CFun expr (cdr ids)))))

(: currify-CCall : CORE (Listof BRANG) DE-ENV -> CORE)
(define (currify-CCall cfun-expr arg-exprs env)
  (if (null? arg-exprs)
      cfun-expr
      (let ([arg-exprs (reverse arg-exprs)])
        (CCall (currify-CCall cfun-expr (cdr arg-exprs) env)
               (preprocess (car arg-exprs) env)))))

(: eval : CORE ENV -> VAL)
;; evaluates BRANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef N) (list-ref env N)]
    [(CFun bound-body)
     (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body
                (cons (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) de-empty-env) null)])
    (cases result
      [(NumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))

;; tests
(test (run "{+ 5 5}") => 10)
(test (run "{- 9 5}") => 4)
(test (run "{* 2 5}") => 10)
(test (run "{/ 10 2}") => 5)
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)
(test (run "{call {fun {x y} {+ x y}} 1 2}")
      => 3)
(test (run "{bleh}") =error> "bad syntax in (bleh)")
(test (run "{with {x 3} {+ 10 y}}")
      =error> "no binding for y")
(test (run "{with {x 3}}")
      =error> "bad `with' syntax in (with (x 3))")
(test (run "{fun {x y} {+ x y} {+ x y}}")
      =error> "bad `fun' syntax in (fun (x y) (+ x y) (+ x y))")
(test (run "{+ 1 {fun {x} {+ 1 2}}}")
      =error> "expected a number, got: (FunV (CAdd (CNum 1) (CNum 2)) ())")
(test (run "{call {+ 1 2} 3}")
      =error> "`call' expects a function, got: (NumV 3)")
(test (run "{fun {x} {+ 1 2}}")
      =error> (string-append "evaluation returned a non-number: "
                             "(FunV (CAdd (CNum 1) (CNum 2)) ())"))

(define minutes-spent 420)
