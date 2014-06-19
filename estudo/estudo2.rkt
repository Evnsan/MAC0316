#lang plai-typed
(define-type ExprC
  [numC ( n : number) ]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (condicao : ExprC) (sim : ExprC) (nao : ExprC)]
)

(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [appC (f a)
          (local ([define fd (get-fundef f fds)])
            (interp (subst a (fdC-arg fd) (fdC-body fd)) fds)
           )
    ]
    [idC (_) (error 'interp "I should have not found this!")]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [ifC (c s n) (if (zero? (interp c fds)) (interp n fds)(interp s fds))]
   )
  )

(define-type ExprS
  [numS ( n : number)]
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [ifS (c : ExprS) (s : ExprS) (n : ExprS)]
)

(define (parse [ s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond 
                [(empty? (rest (rest sl)))
                 (uminusS (parse (second sl)))]
                [else
                 (bminusS (parse (second sl)) (parse (third sl)))])]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
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
    [appS (fun arg) (appC fun (desugar arg))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
   )
 )

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)]
 )

(define (subst [valor : ExprC] [isso : symbol] [em : ExprC]) : ExprC
  (type-case ExprC em
    [numC (n) em]
    [idC (s) (cond
               [(symbol=? s isso) valor]
               [else em]
               )
    ]
    [appC (f a) (appC f (subst valor isso a))]
    [plusC (l r) (plusC (subst valor isso l) (subst valor isso r))]
    [multC (l r) (multC (subst valor isso l) (subst valor isso r))]
    [ifC (c s n) (ifC (subst valor isso c) (subst valor isso s) (subst valor isso n))]
    )
  )

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "Reference to the function not defined")]
    [ (cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))]
                    )
    ]
  )
)

(define biblioteca
  (list
   [fdC 'dobro 'x (plusC (idC 'x) (idC 'x))]
   [fdC 'quadrado 'y (multC (idC 'y) (idC 'y))]
   [fdC 'fatorial 'n
        (ifC (idC 'n)
             (multC (appC 'fatorial (plusC (idC 'n) (numC -1))) (idC 'n))
             (numC 1)
         )
    ]
   [fdC 'narciso 'narciso (multC (idC 'narciso) (numC 1000))]
   )
  )
