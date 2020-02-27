;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4th oct|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define avinash
  (rectangle 20 30 "solid" "red"))
;;; distance-to-origin: posn → number
;;; computes the distance of a posn from the origin
(define (distance-to-origin a-posn a-pixel)
;;; INVENTORY
;;; a-posn is a posn
;;; (posn-x a-posn) is the number x of a-posn
;;; (posn-y a-posn) is the number y of a-posn
(sqrt (+ (sqr (posn-x a-posn))
(sqr (posn-y a-posn))))
(cond [(number? a-pixel)(distance-to-origin-num-pixel a-pixel)]
        [(posn? a-pixel)(distance-to-origin-posn-pixel a-pixel)]))
(check-expect(distance-to-origin 0)0)
(check-expect(distance-to-origin 2)2)
(check-expect(distance-to-origin (make-posn 1 0))1)

(check-expect (distance-to-origin (make-posn 0 0)) 0)
(check-expect (distance-to-origin (make-posn 3 4)) 5)

(define (perimeter a-shape)
  (cond [(mysquare? a-shape) (perimeter-for-square a-shape)]
        [(mycircle? a-shape)   (perimeter-for-circle a-shape)]))

(define (perimeter-for-square a-square)
  (* (mysquare-length a-square) 4) )
(define (perimeter-for-circle a-circle)
  (* (* 2 (mycircle-radius a-circle)) pi) )

