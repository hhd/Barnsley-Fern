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
(define *COLOURS* '("green" "green" "green" "green"))

(define *ITERATIONS* 100)
(define *WIDTH* 350)
(define *HEIGHT* 350)

; Valid ranges for the plottable points.
(define *RANGE_X* '(-2.1818 2.6556))
(define *RANGE_Y* '(0 9.95851))

; Matrix constants for each function.
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
  (let ((ref '((#\a 0) (#\b 1) (#\c 2)
              (#\d 3) (#\e 4) (#\f 5)
              (#\p 6))))
    (vector-ref input
                (cadr (assoc chr ref)))))

; Randomly selects the correct function input 
; with non-uniform probability (given by p in the matrix).
(define (choose-function row rnd)
  (let* ((input (list-ref matrix row))
         (remaining (- rnd (mval #\p input))))
    (if (<= remaining 0)
      input
      (choose-function (+ row 1) remaining))))

; Finds a point to draw the next pixel given the correct
; matrix value references.
(define (find-point x y va vb vc input)
  (+ (* (+ (* x
              (mval va input))
           y)
        (mval vb input))
     (mval vc input)))

(define (find-x x y input)
  (find-point x y #\a #\b #\e input))

(define (find-y x y input)
  (find-point x y #\c #\d #\f input))

; Calculates a pixel position and draws a point on it.
(define (draw-pixel x y)
  (let ((pixel-x (* (/ (- x (car *RANGE_X*)) (- (cadr *RANGE_X*) (car *RANGE_X*))) *WIDTH*))
        (pixel-y (- (- *HEIGHT* 1) (* (/ (- y (car *RANGE_Y*)) (- (cadr *RANGE_Y*) (car *RANGE_Y*))) *HEIGHT*))))
    (display pixel-x)
    (display ", ")
    (display pixel-y)
    (display #\newline)))

; Plots the next point on the canvas using x,y as the
; seed and i as the iteration.
(define (plot-points i x y)
  (let* ((input (choose-function 0 (random)))
         (next-x (find-x x y input))
         (next-y (find-y x y input)))
    (draw-pixel next-x next-y)
    (if (< i *ITERATIONS*)
      (plot-points (+ i 1) next-x next-y))))

(define (draw-fern)
  (plot-points 0 1 1))
