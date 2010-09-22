;;; Fractals
;;; Barnsley's Fern
;;; Andrew Buntine, 2010
;;;
;;; The Barnsley Fern is a fractal named after the British mathematician
;;; Michael Barnsley. The fern is a basic example of a mathematically
;;; generated pattern that can be reproducible at any magnification or reduction.
;;;
;;; Hopefully I can impress my Maths tutor with this.
;;;
;;; Usage:
;;;   (aaaaaa)

(require (lib "graphics.ss" "graphics"))

; Colours to represent each function with.
(define colours '("green" "green" "green" "green"))

; Matrix values for each function.
; Formula:
;   f(x,y) = [a b][x] + [e]
;            [c d][y]   [f]
;
; Format (where 'p' is the propability factor): 
;   (a b c d e f p)
(define matrix
  '(#(0 0 0 0.16 0 0 0.01)
    #(0.85 0.04 -0.04 0.85 0 1.6 0.85)
    #(0.2 -0.26 0.23 0.22 0 1.6 0.07)
    #(-0.15 0.28 0.26 0.24 0 0.44 0.07)))

; Returns a given value from a row of the matrix
; as fed in through 'input'.
(define (mval chr input)
  (let (ref '((#\a 0) (#\b 1) (#\c 1)
              (#\d 1) (#\e 1) (#\f 1)
              (#\p 1)))
    (vector-ref input
                (cadr (assoc chr ref)))))

; Randomly selects the correct function input 
; with non-uniform probability (given by p in the matrix).
(define (choose-function row rnd)
  (let* ((input (list-ref matrix row))
         (remaining (- (mval #\p input) rnd)))
    (if (<= remaining 0)
      input
      (choose-function (+ row 1) remaining))))

(define (find-x x y input)
  (+ (* (+ (* x
           (mval #\a input)) y)
        (mval #\b input))
     (mval #\e input)))

(define (find-y x y input)
  (+ (* (+ (* x
           (mval #\c input)) y)
        (mval #\d input))
     (mval #\f input)))

; Draws the next point on the canvas using x,y as the
; seed and i as the iteration.
(define (draw-point i x y)
  (let* ((input (choose-function 0 (random)))
         (next-x (find-x x y input))
         (next-y (find-y x y input)))
    (display next-x)
    (display next-y)
    (if (< i 100)
      (draw-point (+ i 1) next-x next-y))))
