;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |6th oct|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; length-los: los -> number
; Purpose: to compute the number of elements in the given los
(define (length-los a-los)
; INVENTORY
; (empty? a-los) true if a-los is empty
; (cons? a-los) true if a-los is a cons list (i.e. not empty) 
; If (cons? a-los)
;   (first a-los) the first symbol of a-los
;   (rest a-los) the los w/ rest of the symbols of a-los
;   (length-los (rest a-los) ) the length for the los that is rest
  (cond [(empty? a-los) 0]
        [else (+ 1 (length-los (rest a-los)))]))
(check-expect (length-los empty) 0)
(check-expect (length-los (cons 'foo (cons 'bar empty))) 2)
(define L1 (cons 'a (cons 'b (cons 'c (cons 'd empty)))))
(check-expect (length-los L1) 4)

(define (length-lon a-lon)
; INVENTORY
; (empty? a-lon) true if a-los is empty
; (cons? a-lon) true if a-los is a cons list (i.e. not empty) 
; If (cons? a-lon)
;   (first a-lon) the first symbol of a-los
;   (rest a-lon) the los w/ rest of the symbols of a-los
;   (length-lon (rest a-lon) ) the length for the los that is rest
  (cond [(empty? a-lon) 0]
        [else (+ 1 (length-lon (rest a-lon)))]))
(check-expect (length-lon empty) 0)
(check-expect (length-lon (cons 'foo (cons 'bar empty))) 2)

; sum-lon: lon -> number
; Purpose: to add all the elements of the given lon
(define (sum-lon a-lon)
; INVENTORY
; (empty? a-lon) true if a-lon is empty
; (cons? a-lon) true if a-lom is a cons list (i.e. not empty)
; if (cons? a-lon)
;   (first a-lon) the first number of a-lon
;   (rest a-lon) the lon w/ the rest of the numbers of a-lon
;   (sum-lon (rest a-lon) ) the sum for the lon that is rest
  (cond [(empty? a-lon) 0]
        [else (+ (first a-lon) (sum-lon (rest a-lon)))]))
(check-expect (sum-lon empty) 0)
(check-expect (sum-lon (cons 1 (cons 2 (cons 3 empty)))) 6)
; quiz-avg: lon  number
; Purpose: Compute the quiz average from the given list of quiz grades
(define (quiz-avg a-lon)
  (/ (sum-lon a-lon) (length-lon a-lon)))
(check-expect (quiz-avg (cons 89 empty)) 89)
(check-expect (quiz-avg (cons 80 (cons 85 empty))) 82.5)


; commission-lon: lon  lon
; Purpose: To compute the sales commission at 3% for the sales in the lon
(define (commission-lon a-lon)
; INVENTORY
; (empty? a-lon) true if a-lon is empty
; (cons? a-lon) true if a-lom is a cons list (i.e. not empty)
; if (cons? a-lon)
;   (first a-lon) the first symbol of a-lon
;   (rest a-lon) the lon w/ the rest of the numbers of a-lon
;   (commission-lon (rest a-los) ) the list of sales for the lon that is rest
  (cond [(empty? a-lon) empty]
        [else (cons (* 0.03 (first a-lon))
                    (commission-lon (rest a-lon)))]))
(check-expect (commission-lon empty) empty)
(check-expect (commission-lon (cons 1 (cons 100 empty))) (cons 0.03 (cons 3 empty)))