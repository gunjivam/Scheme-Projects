#lang eopl

(require test-engine/racket-tests)

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    
    (comment ("%" (arbno (not #\newline))) skip)
    
    (identifier
     (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    
    (number (digit (arbno digit)) number)
    
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    
    (expression("-" "(" expression "," expression ")")diff-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    
    (expression (identifier) var-exp)
    
    (expression 
     ("let" identifier "=" expression "in" expression) let-exp)
    
    (expression
     ("letrec" identifier "(" (arbno identifier) ")" "=" expression 
               "in" expression) letrec-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    
    (expression ("(" expression (arbno expression) ")") call-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;    ENVIRONMENT

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvars (list-of symbol?))
   (body expression?)
   (saved-env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val saved-env)
                (if (eqv? search-sym var)
                    val
                    (apply-env saved-env search-sym)))
    (extend-env-rec (p-name b-vars p-body saved-env)
                    (if (eqv? search-sym p-name)
                        (proc-val (procedure b-vars p-body env))          
                        (apply-env saved-env search-sym)))))



(define (init-env)
  (extend-env 
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))


;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?)))

;;; extractors:

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (body expression?)
   (env environment?)))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
               (value-of exp1 (init-env)))))

;; value-of : Exp * Env -> ExpVal
(define (value-of exp env)
  (cases expression exp
    
    (const-exp (num) (num-val num))
    
    (var-exp (var) (apply-env env var))
    
    (diff-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (num-val (- num1 num2)))))
    
    (zero?-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((num1 (expval->num val1)))
                   (if (zero? num1)
                       (bool-val #t)
                       (bool-val #f)))))
    
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
              (if (expval->bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    
    (let-exp (var exp1 body)       
             (let ((val1 (value-of exp1 env)))
               (value-of body
                         (extend-env var val1 env))))
    
    (letrec-exp (p-name params p-body letrec-body)
                (value-of letrec-body (extend-env-rec p-name
                                                      params
                                                      p-body
                                                      env)))
    
    (proc-exp (vars body)
              (proc-val (procedure vars body env)))
    
    (call-exp (rator rands)
              (let ((proc (expval->proc (value-of rator env)))
                    (args (map (lambda (e) (value-of e env)) rands)))
                (apply-procedure proc args)))))


;; apply-procedure : Proc * ExpVal -> ExpVal
(define (apply-procedure proc1 vals)
  (define (add-to-env vars vals e)
    (cond [(null? vars) e]
          [else (extend-env (car vars)
                            (car vals)
                            (add-to-env (cdr vars)
                                        (cdr vals)
                                        e))]))
  (cases proc proc1
    (procedure (vars body saved-env)
               (value-of body
                         (add-to-env vars vals saved-env)))))

;;;;;;   UNPARSE

(define (unparse-program p)
  (cases program p
    (a-program (e) (unparse-exp e))))

(define (unparse-exp exp)
  (define (string-map lst)
    (cond [(null? (cdr lst)) (car lst)]
          [else (string-append (car lst) " " (string-map (cdr lst)))]))
  (cases expression exp
    (const-exp (num) (number->string num))
    (var-exp (var) (symbol->string var))
    (diff-exp (exp1 exp2)
              (string-append "-("
                             (unparse-exp exp1)
                             ", "
                             (unparse-exp  exp2)
                             ")"))
    (zero?-exp (exp1)
               (string-append "zero?(" (unparse-exp exp1) ")"))
    (if-exp (exp1 exp2 exp3)
            (string-append
             "if "
             (unparse-exp  exp1)
             " then "
             (unparse-exp exp2)
             " else "
             (unparse-exp exp3)))   
    (let-exp (var exp1 body)
             (string-append
              "let "
              (symbol->string var)
              " = "
              (unparse-exp exp1)
              " in "
              (unparse-exp body)))    
    (letrec-exp (p-name params p-body letrec-body)
                (string-append 
                 "letrec "
                 (symbol->string p-name)
                 "("
                 (string-map (map (lambda (v) (symbol->string v)) params))
                 ") = "
                 (unparse-exp p-body)
                 " in "
                 (unparse-exp letrec-body)))
    (proc-exp (vars body)
              (string-append
               "proc("
               (string-map (map (lambda (v) (symbol->string v)) vars))
               ") "
               (unparse-exp body)))
    (call-exp (rator rands)
              (string-append
               "("
               (unparse-exp rator)
               " "
               (string-map (map (lambda (e) (unparse-exp e)) rands))
               ")"))))


;;;;;;   CPS
(define (apply-cont cont val)
  (cont val))

(define pnum (vector 0))

(define (gen-param-symb)
  (let ((newp (string->symbol
               (string-append
                (symbol->string 'v)
                (number->string (vector-ref pnum 0))))))
    (begin
      (vector-set! pnum 0 (+ (vector-ref pnum 0) 1))
      newp)))

; expression --> Boolean
; Purpose: To determine if the given expr is a nontail call or has a nontail call
(define (has-nontail? exp)
  (cases expression exp
    
    (const-exp (num) #f)
    
    (var-exp (var) #f)
    
    (diff-exp (exp1 exp2)
              (or (has-nontail? exp1) (has-nontail? exp2)))
    (zero?-exp (exp1)
               (has-nontail? exp1))
    (if-exp (exp1 exp2 exp3)
            (or (has-nontail? exp1)
                (has-nontail? exp2)
                (has-nontail? exp3)))    
    (let-exp (var exp1 body)   
             (or (has-nontail? exp1) (has-nontail? body)))    
    (letrec-exp (p-name params p-body letrec-body)
                (has-nontail? letrec-body))
    (proc-exp (vars body) #false)
    (call-exp (rator rands) #t)))

; (listof expr) --> expr or false
; Purpose: To determine if the given list of args has a nontail element
(define (has-nontail-in-rands? lst)
  (if (null? lst)
      #f
      (let [(fst (has-nontail? (car lst)))]
        (if fst
            (car lst)
            (has-nontail-in-rands? (cdr lst))))))

; expr expr (listof expr) --> (listof expr)
; Purpose: Substitute the first expr with the second in the given loe
(define (subst-loe old new rands)
  (if (eq? old (car rands))
      (cons new (cdr rands))
      (cons (car rands) (subst-loe old new (cdr rands)))))
              
; program --> program
; Purpose: To CPS the given program 
(define (program-cps p)
  (cases program p
    (a-program (exp1)
               (a-program (exp-cps exp1
                                   (let ((param (gen-param-symb)))
                                     (proc-exp (list param) (var-exp param))))))))

;#(struct:proc-exp (v1) #(struct:let-exp x #(struct:var-exp v1)
 ;                                       #(struct:call-exp #(struct:proc-exp (v0) #(struct:var-exp v0))
  ;                                                        (#(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 4))))))
;; exp-cps : Exp Cont -> Exp
(define (exp-cps exp k)

  (cases expression exp
    ;apply end-cont
    (const-exp (num)  exp)
    ;apply end-cont
    (var-exp (var)  exp)
    ;diff-exp apply cond-cont 
    (diff-exp (exp1 exp2)
              (let ((fnt1 (has-nontail? exp1))
                    (fnt2 (has-nontail? exp2)))
                (cond [(and (not fnt1) (not fnt2))
                       exp]
                      [else
                       (call-exp k (list (diff-exp (exp-cps exp1 k) (exp-cps exp2 k))))])))
                    
    (zero?-exp (exp1)
               (let ((fnt1 (has-nontail? exp1)))
                 (cond [(not fnt1)
                        exp]
                       [else
                        (call-exp k
                          (list (zero?-exp 
                                 (exp-cps exp1 k))))])))
    (if-exp (exp1 exp2 exp3)
            (let ((fnt1 (has-nontail? exp1)))
              (cond [fnt1
                     (cases expression exp1
                       (zero?-exp (e)
                                   (if-exp (zero?-exp (exp-cps e k))
                                                     (exp-cps exp2 k)
                                                     (exp-cps exp3 k)))
                                  ;(exp-cps e
                                   ;        (let ((newparam (gen-param-symb)))
                                    ;         (proc-exp (list newparam)
                                     ;                  (if-exp (zero?-exp (var-exp newparam))
                                      ;                         (exp-cps exp2 k)
                                       ;                        (exp-cps exp3 k))))))
                       (else
                        (exp-cps exp1
                                 (let ((newparam (gen-param-symb)))
                                   (proc-exp (list newparam)
                                             (if-exp (var-exp newparam)
                                                     (exp-cps exp2 k)
                                                     (exp-cps exp3 k)))))))]
                    [else
                     (if-exp exp1
                             (exp-cps exp2 k)
                             (exp-cps exp3 k))])))
    
    (let-exp (var exp1 body)
             (let-exp var (exp-cps exp1 k) (exp-cps body k)))
    
    (letrec-exp (p-name params p-body letrec-body)
                (letrec-exp p-name
                            (append params (list 'k))
                            (exp-cps p-body (var-exp 'k))
                            (exp-cps letrec-body k)))
    
    (proc-exp (vars body)
              (call-exp k
                        (list (proc-exp (append vars (list 'k))
                                        (exp-cps body (var-exp 'k))))))
    (call-exp (rator rands)
              (let ((fnt1 (has-nontail? rator)))
                (cond [fnt1
                       (exp-cps rator
                                (let ((newparam (gen-param-symb)))
                                  (proc-exp (list newparam)
                                            (exp-cps
                                             (call-exp (var-exp newparam)
                                                       rands)
                                             k))))]
                      [else
                       (let ((fnt1 (has-nontail-in-rands? rands)))
                         (cond [fnt1
                                (exp-cps
                                 fnt1
                                 (let ((newparam (gen-param-symb)))
                                   (proc-exp (list newparam)
                                             (exp-cps
                                              (call-exp rator
                                                        (subst-loe fnt1 (var-exp newparam) rands))
                                              k))))]
                               
                               [else
                                (call-exp rator (append rands (list k)))]))])))))



;;;;;;   EVALUATION WRAPPERS

;; parse: String -> a-program
(define (parse p) (scan&parse p))

;; eval : String -> ExpVal
(define (eval string)
  (value-of-program (parse string)))

;;;;; EXAMPLES OF CPS TRANSFORMATION

(check-expect (unparse-program
               (program-cps (parse "-(x, v)")))
              "-(x, v)")

(check-expect (unparse-program
               (program-cps (parse "if zero?(-(x, v)) then x else 2")))
              "if zero?(-(x, v)) then x else 2")

(check-expect (unparse-program (program-cps (parse "if (f x) then x else 2")))
              "(f x proc(v3) if v3 then x else 2)")

(check-expect (unparse-program (program-cps (parse "proc (x) proc (y) -(x, -(0, y))")))
              "(proc(v4) v4 proc(x k) (k proc(y k) -(x, -(0, y))))")

(check-expect (unparse-program
               (program-cps
                (parse "let sum = proc (x) proc (y) -(x, -(0, y))
                        in ((sum 3) 4)")))
              "let sum = (proc(v5) v5 proc(x k) (k proc(y k) -(x, -(0, y)))) in (sum 3 proc(v6) (v6 4 proc(v5) v5))")

(check-expect (unparse-program (program-cps
                                (parse "letrec d(n) = if zero?(n)
                                                      then 1
                                                      else -(n, (d -(n, 1)))
                                        in (d 5)")))
              "letrec d(n k) = if zero?(n) then 1 else (k -(n, (d -(n, 1) k))) in (d 5 proc(v7) v7)")

(check-expect (unparse-program (program-cps (parse "if zero?((f x)) then 1 else 2")))
              "if zero?((f x proc(v8) v8)) then 1 else 2")


(check-expect (unparse-program (program-cps (parse "(f x (g x))")))
              "(g x proc(v10) (f x v10 proc(v9) v9))")

(check-expect (unparse-program (program-cps (parse "if zero?((f x (g x) x)) then (g 2 (h i)) else 3")))
              "if zero?((g x proc(v12) (f x v12 x proc(v11) v11))) then (h i proc(v13) (g 2 v13 proc(v11) v11)) else 3")

(check-expect (unparse-program
               (program-cps
                (parse "-((f x), (f x))")))
              "(proc(v14) v14 -((f x proc(v14) v14), (f x proc(v14) v14)))")

(check-expect (unparse-program
               (program-cps
                (parse "let f = proc(x) -(x, 5) in if zero?((f 5)) then (f 10) else (f 11)")))
               "let f = (proc(v15) v15 proc(x k) -(x, 5)) in if zero?((f 5 proc(v15) v15)) then (f 10 proc(v15) v15) else (f 11 proc(v15) v15)")

(check-expect (eval (unparse-program
               (program-cps
                (parse "let f = proc(x) -(x, 5) in if zero?((f 5)) then (f 10) else (f 11)")))) (num-val 5))

(check-expect (unparse-program (program-cps (parse "let f = proc(x) x in (f 1)")))
              "let f = (proc(v17) v17 proc(x k) x) in (f 1 proc(v17) v17)")

(check-expect (unparse-program (program-cps (parse "let f = proc(x) x in zero?((f 1))")))
              "let f = (proc(v18) v18 proc(x k) x) in (proc(v18) v18 zero?((f 1 proc(v18) v18)))")

(check-expect (eval "let f = (proc(v21) v21 proc(x k) x) in (proc(v21) v21 zero?((f 1 proc(v21) v21)))") (bool-val #false))

(check-expect (eval (unparse-program (program-cps (parse "let f = proc(x) x in if zero?((f -1)) then 1 else -1")))) (num-val -1))
(check-expect (unparse-program (program-cps (parse "let f = proc(x) x in if zero?((f -1)) then 1 else -1")))
              "let f = (proc(v20) v20 proc(x k) x) in if zero?((f -1 proc(v20) v20)) then 1 else -1")
(check-expect (unparse-program (program-cps (parse "let f = proc(x) x in if zero?((f -1)) then -((f 5), 1) else -((f 5), -1)")))
              "let f = (proc(v21) v21 proc(x k) x) in if zero?((f -1 proc(v21) v21)) then (proc(v21) v21 -((f 5 proc(v21) v21), 1)) else (proc(v21) v21 -((f 5 proc(v21) v21), -1))")
(check-expect  (eval (unparse-program (program-cps (parse "let f = proc(x) x in if zero?((f -1)) then -((f 5), 1) else -((f 5), -1)"))))
               (num-val 6))

(test)
