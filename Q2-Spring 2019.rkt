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
     ("minus" "(" expression ")") negate-exp)

    ;;add-expr: num-val, num_val -> num_val
    ;;Purpose: returns the sum of two numbers
    ;;the expression arguments evaluate to num-vals
    (expression
     ("+" "(" expression "," expression ")") sum-exp)
    ;;mult-expr: num-val, num_val -> num_val
    ;;Purpose: returns the product of two numbers
    ;;the expression arguments evaluate to num-vals
    (expression
     ("*" "(" expression "," expression ")") mult-exp)
    ;;div-expr: num-val, num_val -> num_val
    ;;Purpose: returns the quotient of the division of two numbers
    ;;the expression arguments evaluate to num-vals
    (expression
     ("/" "(" expression "," expression ")") div-exp)
    ;;eq-expr: num-val, num_val -> bool_val
    ;;Purpose: returns whether the two numbers are equal
    ;;the expression arguments evaluate to num-vals
    (expression
     ("equal?" "(" expression "," expression ")") eq-exp)
    ;;bgr-expr: num-val, num_val -> bool_val
    ;;Purpose: returns whether the first number is greater than the second
    ;;the expression arguments evaluate to num-vals
    (expression
     ("greater?" "(" expression "," expression ")") bgr-exp)
    ;;lss-expr: num-val, num_val -> bool_val
    ;;Purpose: returns whether the first number is smaller than the second
    ;;the expression arguments evaluate to num-vals
    (expression
     ("less?" "(" expression "," expression ")") lss-exp)
    ;;list-exp: an expression to create a list with variable number of elements
    (expression
     ("list" "(" (separated-list expression ",") ")") list-exp)
    ;;cons: num-val||list-val , list-val -> list-val
    ;;Purpose: adds the element to the head of the list
    (expression
     ("cons" "(" expression "," expression")") cons-exp)
    ;;car: list-val -> num-val||list-val
    ;;Purpose: returns the first element of the list
    (expression
     ("car" "(" expression ")") car-exp)
    ;;cdr: list-val -> list-val
    ;;Purpose: returns the rest of the list
    (expression
     ("cdr" "(" expression ")") cdr-exp)
    ;;null?: list-val -> bool-val
    ;;Purpose: returns if list-val is empty
    (expression
     ("null?" "(" expression ")") null?-exp)
    ;;to create an emptylist
    (expression
     ("emptylist") emptylist-exp)
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

;; example of a data type built without using define-datatype

(define (empty-env-record) '())

(define (extended-env-record sym val old-env)
    (cons (list sym val) old-env))

(define empty-env-record? null?)

(define (environment? x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x)))))

(define (extended-env-record->sym r) (car (car r)))

(define (extended-env-record->val r) (cadr (car r)))

(define (extended-env-record->old-env r) (cdr r))

;; end of example of a data type built without define-datatype

;;;;; Implementation of environment interface
(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

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
  (list-val
   (lst list?)))

;;; observers:

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

;;expval ->list : ExpVal -> List
(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (list) list)
      (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-observers "Looking for a ~s, found ~s"
                variant value)))

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

    ;;expval -> list-val
    (list-exp (exp)
              ;;to recursively process the expression until it evaluates to a list
                  (letrec
                      ;func: list of num-vals or list-val -> list-val 
                      ([func (lambda (lst)
                               ;Termination argument. (cdr lst) will eventually become null.
                               (cond [(null? lst) '()]
                                     [(let ((v (value-of (car lst) env)))
                                       (cons v (func (cdr lst))))]))])
               ;creates a list-val of the evaluated expression
              (list-val (func exp))))

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
    
    (negate-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                  (let ((num1 (expval->num val1)))
                    (num-val (* -1 num1)))))
    ;sum-exp: expval expval -> num-val
    (sum-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (num-val (+ num1 num2)))))
    ;mult-exp: expval expval -> num-val
    (mult-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (num-val (* num1 num2)))))
    ;div-exp: expval expval -> num-val
    (div-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (num-val (quotient num1 num2)))))
    ;eq-exp: expval expval -> bool-val
    (eq-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (bool-val (eqv? num1 num2)))))
    ;bgr-exp: expval expval -> bool-val
    (bgr-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (bool-val (> num1 num2)))))
    ;lss-exp: expval expval -> bool-val
    (lss-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (bool-val (< num1 num2)))))
    ;cons-exp: expval (num-val|list-val) expval(list-val) -> list-val
    (cons-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let
                     ((l (expval->list val2)))
                  (list-val (cons val1 l)))))
    ;car-exp: exp-val (list-val) -> num-val | list-val
    (car-exp (exp)
             (let ((val1 (value-of exp env)))
                   (let ((lst (expval->list val1)))
                     (car lst))))
    ;cdr-exp: exp-val (list-val) -> list-val
    (cdr-exp (exp)
             (let ((val1 (value-of exp env)))
                   (let ((lst (expval->list val1)))
                     (list-val (cdr lst)))))
    ;null-exp: exp-val (list-val) -> bool-val
    (null?-exp (exp)
             (let ((val1 (value-of exp env)))
                   (let ((lst (expval->list val1)))
                     (bool-val (null? lst)))))
    ;emptylist-exp:  -> list-val
    (emptylist-exp ()
               (list-val '()))
             
    ))

;;;;;;   EVALUATION WRAPPERS

;; eval : String -> ExpVal
(define (eval string)
  (value-of-program (scan&parse string)))

;;;;; EXAMPLES OF EVALUATION

; (eval "if zero?(1) then 1 else 2")
; (eval "-(x, v)")
; (eval "if zero?(-(x, x)) then x else 2")
; (eval "if zero?(-(x, v)) then x else 2")
; (eval "let x = 2 in -(x, 2)")

(check-expect (eval "if zero?(1) then 1 else 2")
              (num-val 2))
(check-expect (eval "-(x, v)")
              (num-val 5))
(check-expect (eval "if zero?(-(x, x)) then x else 2")
              (num-val 10))
(check-expect (eval "if zero?(-(x, v)) then x else 2")
              (num-val 2))
(check-expect (eval "let x = 2 in -(x, 2)")
              (num-val 0))

(check-expect (eval "+(4, 3)") (num-val 7))
(check-expect (eval "let x = 2 in +(x, 2)") (num-val 4))
(check-expect (eval "*(4, 3)") (num-val 12))
(check-expect (eval "let x = 2 in *(+(x, x), 2)") (num-val 8))
(check-expect (eval "/(5, 2)") (num-val 2))
(check-expect (eval "let x = 2 in *(/(x, x), 2)") (num-val 2))
(check-expect (eval "equal?(4, 4)") (bool-val #t))
(check-expect (eval "equal?(4, 2)") (bool-val #f))
(check-expect (eval "let x = 4 in equal?(x, 4)") (bool-val #t))
(check-expect (eval "greater?(4, 4)") (bool-val #f))
(check-expect (eval "greater?(4, 2)") (bool-val #t))
(check-expect (eval "greater?(4, 6)") (bool-val #f))
(check-expect (eval "let x = 4 in greater?(x, 2)") (bool-val #t))
(check-expect (eval "less?(4, 4)") (bool-val #f))
(check-expect (eval "less?(4, 2)") (bool-val #f))
(check-expect (eval "less?(4, 6)") (bool-val #t))
(check-expect (eval "let x = 4 in less?(x, 2)") (bool-val #f))
(check-expect (eval "emptylist") (list-val '()))
(check-expect (eval "null?(emptylist)") (bool-val #t))
(check-expect (eval "cons(4, emptylist)") (list-val (list (num-val 4))))
(check-expect (eval "let x = 4 in cons(x, cons(cons(-(x, 1), emptylist),emptylist))")
                    (list-val (list (num-val 4) (list-val (list (num-val 3))))))

(check-expect (eval "list(1, 2, 3, 4)") (list-val (list (num-val 1) (num-val 2) (num-val 3) (num-val 4))))
(check-expect (eval "list(emptylist)") (list-val (list (list-val'()))))

(check-expect (eval "car(list(1, 2, 3, 4))") (num-val 1))
(check-expect (eval "cdr(list(1, 2, 3, 4))") (list-val (list (num-val 2) (num-val 3) (num-val 4))))
(check-expect (eval "let x = 4 in list(x, -(x, 1), -(x, 3))")
              (list-val (list (num-val 4) (num-val 3) (num-val 1))))
              


(test)

