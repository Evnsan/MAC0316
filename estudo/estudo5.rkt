#lang plai-typed
(define-type ExprC
  [numC ( n : number) ]
  [idC (s : symbol)]
  [lamC (arg : symbol) (body : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (condicao : ExprC) (sim : ExprC) (nao : ExprC)]
)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
)

(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [appC (f a)
          (local ([define f-value (interp f env)])
            (interp (closV-body f-value)
                    (extend-env
                     (bind (closV-arg f-value) (interp a env)) 
                     (closV-env f-value)
                     )
             )
           )
    ]
    [lamC (a b) (closV a b env)]
    [idC (n) (lookup n env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC (c s n) (if (numZero? (interp c env)) (interp n env)(interp s env))]
   )
  )

(define-type ExprS
  [numS ( n : number)]
  [idS (s : symbol)]
  [lamS (arg : symbol) (body : ExprS)]
  [appS (fun : ExprS) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [ifS (c : ExprS) (s : ExprS) (n : ExprS)]
)

(define (parse [ s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond 
                [(empty? (rest (rest sl)))
                 (uminusS (parse (second sl)))]
                [else
                 (bminusS (parse (second sl)) (parse (third sl)))
                ]
               )
         ]
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(func) (lamS (s-exp->symbol (second sl))
                      (parse (third sl))
                   )
         ]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]
         )
       )
     ]
    [else (error 'parse "invalid input")]
    )
  )


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [idS (s) (idC s)]
    [lamS (a b) (lamC a (desugar b))]
    [appS (fun arg) (appC (desugar fun) (desugar arg))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
   )
 )

(define-type Binding
  [bind (name : symbol) (val : Value)]
 )

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
               (bind-val (first env))]
            [else (lookup for (rest env))]
           )
    ]
  )
)

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "One of arguments isn't a number")]
   )
 )

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "One of arguments isn't a number")]
   )
 )

(define (numZero? [test : Value]) : boolean
  (cond
    [(numV? test) (if (zero? (numV-n test)) #t #f)]
    [else (error 'num* "The argument isn't a number")]
   )
 )

(define (interpS [s : s-expression]) 
  (interp (desugar (parse s)) mt-env)
)