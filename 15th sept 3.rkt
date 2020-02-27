;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |15th sept 3|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define Rate1 0.04)
(define Rate2 0.045)
(define Rate3 0.055)
(define Rate4 0.06)
;;; interest-rate: number → number
;;; computes the interest rate for a given amount >=0
(check-expect (interest-rate 1000) 0.04)
(check-expect (interest-rate 2467) 0.045)
(check-expect (interest-rate 31000) 0.06)
(check-expect (interest-rate 9999) 0.055)
;;; interest-rate: number → number
;;; computes the interest rate for a given amount >=0
(define (interest-rate amt)
;;;INVENTORY
;;; amt is the amount deposited DrRacket
(cond
[(<= amt 1000) 0.04]
[(<= amt 5000) 0.045]
[(<= amt 10000) 0.055]
[else 0.06]))
