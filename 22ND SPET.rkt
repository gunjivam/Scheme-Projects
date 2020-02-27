;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |22ND SPET|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Data Definition:
; A world is a structure: (make-world image number).
(define W 220)
(define L 220)
(define STOPLIMIT 40)
(define-struct world (img ticks))
(define RED-ON 
(overlay (above (circle 30 'solid 'red)
(square 10 "outline" "white")
(circle 30 'outline 'yellow)
(square 10 "outline" "white")
(circle 30 'outline 'green))
(rectangle 70 220 "outline" "black")))
(define YELLOW-ON 
(overlay (above (circle 30 'outline 'red)
(square 10 "outline" "white")
(circle 30 'solid 'yellow)
(square 10 "outline" "white")
(circle 30 'outline 'green))
(rectangle 70 220 "outline" "black")))
(define GREEN-ON 
(overlay (above (circle 30 'outline 'red)
(square 10 "outline" "white")
(circle 30 'outline 'yellow)
(square 10 "outline" "white")
(circle 30 'solid 'green))
(rectangle 70 220 "outline" "black")))
(define INIT-WORLD (make-world RED-ON 0))
; TEMPLATE
;; f-on-world: world --> ???
;; Purpose:
;(define (f-on-world w)
;  ; INVENTORY
;  ; (world-img w) is the image of the traffic light that
;  ; (world-ticks w) is the number of ticks counted
; 
;  ...)
; run: symbol  world
(define (start n)
(big-bang INIT-WORLD
(on-draw draw-world)
(on-tick update-world 1)
(stop-when tick-limit?)
(name n)))

;; draw-world: world --> scene
;; Purpose: To draw the given world in the empty scene
;(define (draw-world w)
;  ; INVENTORY
;  ; (world-img w) is the image of the traffic light that
;  ; (world-ticks w) is the number of ticks counted
; 
;  ...)
;; draw-world: world --> scene
;; Purpose: To draw the given world in the empty scene
(define (draw-world w)
;  ; INVENTORY
;  ; (world-img w) is the image of the traffic light that
;  ; (world-ticks w) is the number of ticks counted
;  
(world-img w))
(check-expect (draw-world INIT-WORLD) RED-ON)
; update-world: world -> world
; PURPOSE: Compute the world that follows the given world
;(define (update-world w)
(check-expect (update-world INIT-WORLD) (make-world GREEN-ON 1))
(check-expect (update-world (make-world GREEN-ON 1)) (make-world YELLOW-ON 2))
(check-expect (update-world (make-world YELLOW-ON 1)) (make-world RED-ON 2))
; update-world: world -> world
; PURPOSE: Compute the world that follows the given world
(define (update-world w)
;; INVENTORY
; ...
(cond [(image=? (world-img w) RED-ON)
       (make-world GREEN-ON (+ (world-ticks w) 1))]
      [(image=? (world-img w) YELLOW-ON)
       (make-world RED-ON (+ (world-ticks w) 1))]
      [else (make-world YELLOW-ON (+ (world-ticks w) 1))]))
; tick-limit?: world -> boolean
; PURPOSE: To determine if maximum number of ticks reached
(define (tick-limit? w)
; INVENTORY
; (world-color w) is the color of the light that is on
; (world-ticks w) is the number of ticks counted
; (= (world-ticks w) STOPLIMIT) is a boolean
(= (world-ticks w) STOPLIMIT))
(check-expect (tick-limit? INIT-WORLD) false)
(check-expect (tick-limit? (make-world GREEN-ON STOPLIMIT)) true)