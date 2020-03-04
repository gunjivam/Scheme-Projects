;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quiz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")) #f)))
; create-matrix: number list-of-n^2-numbers -> n-lists-of-n-numbers
(define (create-matrix n n-lon)
  (cond [(or (empty? n-lon) (= n 0)) empty]
        [else
         (local [
        (define (get-list n a-list)
          (cond [(or (zero? n) (empty? a-list)) empty]
                [else (cons (first a-list) (get-list (sub1 n) (rest a-list)))]))]
           
         (cond [(= (remainder (length n-lon) n) 0) (cons (get-list n  n-lon) (create-matrix  n (rest n-lon)))]
                    [else (create-matrix n (rest n-lon))]))]))
    
(check-expect (create-matrix 2 (list 1 2 3 4)) (list (list 1 2) (list 3 4)))
(check-expect (create-matrix 3 (list 1 2 3 4 5 6 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(check-expect (create-matrix 0 empty) empty)
(check-expect (create-matrix 4 (list 1 2 3 4 5 6 8 5 7 9 0 4 2 6 8 21)) (list (list 1 2 3 4) (list 5 6 8 5) (list 7 9 0 4) (list 2 6 8 21)))