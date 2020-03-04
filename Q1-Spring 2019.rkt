#lang racket
(require test-engine/racket-tests)



;(a 1 2)
;(a (b 1 2) 1)
;(a (b 1 2) (c 1 2))
;

;bintree -> number | (symbol bintree bintree);
;if the tree isn't fully filled the unfilled leaves are void

(define get-last-number
  (lambda (tree)
    (cond [(number? tree) tree]
          [(null? (cddr tree)) (get-last-number (cadr tree))]
          [else (get-last-number (caddr tree))])))


(define number-leaves
 (lambda (tree)
  (local [(define (aux n tree)
            
          (if (number? tree) (+ n 1)
              (let* [(left (aux n (cadr tree)))]
                (begin
                  (if (number? left)
                      (let* [(right (aux left (caddr tree)))]
                        (begin
                          
                          (cons (car tree) (list left right))))
                      
                      (let* [(right (aux (caddr left) (caddr tree)))]
                        (begin
                          
                          (cons (car tree) (list left right)))))))))
                       ]
    (aux -1 tree))))



(number-leaves '(a (b 5 4) 3))
(number-leaves '(a (b 5 4) (c 3 2)))
(number-leaves '(a (b (d 5 4) 5) (c 3 2)))

(get-last-number '(a (b (d 1 2) (e 3 4)) (c (f 5 6) 7)))


(check-expect (get-last-number '(a (b (d 1 2) (e 3 4)) (c (f 5 6) 7))) 5)





(define (path n bst)
  (cond [(or (and (number? bst) (eqv? n bst))(eqv? n (car bst))) '()]
        [(< n (car bst)) (cons 'left (path n (cadr bst)))]
        [else (cons 'right (path n (caddr bst)))]))

(path 4 '(5 (3 2 4) (7 6 10)))
(path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
        

