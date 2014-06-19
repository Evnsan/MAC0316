#lang plai-typed
(define-type ExprC
  [numC ( n : number) ]
  [varC (s : symbol)]
  [lamC (arg : symbol) (body : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (condicao : ExprC) (sim : ExprC) (nao : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
)

(define-type Result
  [v*s (v : Value) (s : Store)]
)

(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [appC (f a)
          (type-case Result (interp f env sto)
            [v*s (v-f s-f)
                 (type-case Result (interp a env s-f)
                   [v*s (v-a s-a)
                        (let ([onde (new-loc)])
                             (interp (closV-body v-f)
                                     (extend-env (bind (closV-arg v-f) onde)
                                                 (closV-env v-f)
                                     )
                                     (override-store (cell onde v-a) s-a)
                             )
                        )
                   ]
                 )
            ]
          )
    ]
 
    [lamC (a b) (v*s (closV a b env) sto)]
    [varC (n) (v*s (fetch (lookup n env) sto) sto)]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r) (v*s (num+ v-l v-r) s-r)]
                         )
                   ]
                 )
    ]
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r) (v*s (num* v-l v-r) s-r)]
                        )
                   ]
                 )
    ]
    [ifC (c s n) (type-case Result (interp c env sto)
                   [v*s (v-c s-c)
                        (if (numZero? v-c) (interp n env s-c)(interp s env s-c))
                   ]
                 )
    ]
    [setC (var val) (type-case Result (interp val env sto)
                [v*s (v-val s-val) 
                     (let ([onde (lookup var env)])
                          (v*s v-val
                               (override-store (cell onde v-val) s-val)
                          )
                       )
                ]
              )
    ]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)
                    ]
                   )
    ]
  )
)

(define-type ExprS
  [numS ( n : number)]
  [varS (s : symbol)]
  [lamS (arg : symbol) (body : ExprS)]
  [appS (fun : ExprS) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [ifS (c : ExprS) (s : ExprS) (n : ExprS)]
  [setS (var : symbol) (arg : ExprS)]
  [seqS (b1 : ExprS) (b2 : ExprS)]
  [letS (s : symbol) (val : ExprS) (body : ExprS)]
)

(define (parse [ s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (varS (s-exp->symbol s))]
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
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(def) (letS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
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
    [varS (s) (varC s)]
    [lamS (a b) (lamC a (desugar b))]
    [appS (fun arg) (appC (desugar fun) (desugar arg))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [setS (s v) (setC s (desugar v))]
    [seqS (b1 b2) (seqC (desugar b1) (desugar b2))]
    [letS (s val body) (appC (lamC s (desugar body))(desugar val))]
   )
 )


(define-type-alias Location number)

;;;;;;;;;
(define-type Binding
  [bind (name : symbol) (val : Location)]
)

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

;;;;;;;;;;;
(define-type Storage
  [cell (location : Location) (val : Value)]
)

(define-type-alias Store (listof Storage))

(define mt-store empty)

(define override-store cons)

;;;;;;;;;;;
(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string for) " name not found"))]
    [else (cond
            [(symbol=? for (bind-name (first env)))
               (bind-val (first env))]
            [else (lookup for (rest env))]
           )
    ]
  )
)

;;;;;;;;;;;
(define (fetch [l : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "position not found")]
    [else (cond
            [(= l (cell-location (first sto)))
             (cell-val (first sto))]
            [else (fetch l (rest sto))]
           )
    ]
  )
)

(define new-loc
  (let ([n (box 0)])
       (lambda () (begin (set-box! n (+ 1 (unbox n)))
                         (unbox n)
                  )
        )
  )
)


;;;;;;;;;;; Operadores
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
  (interp (desugar (parse s)) mt-env mt-store)
)

