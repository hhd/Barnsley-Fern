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
;;;   (make-fern)
;;;
;;; TODO:
;;;   - Add ability to change colours for each of the four functions.

(require (lib "graphics.ss" "graphics"))

(define *ITERATIONS* 200000)
(define *WIDTH* 400)
(define *HEIGHT* 400)

; Valid ranges for the plottable points.
(define *RANGE_X* '(-2.1818 2.6556))
(define *RANGE_Y* '(0 9.95851))
;(define *RANGE_X* '(-4 4))
;(define *RANGE_Y* '(-0.1 10.1))

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
    #(0.2 -0.26 0.23 0.22 0 1.6 0.08)
    #(-0.15 0.28 0.26 0.24 0 0.44 0.06)))

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
  (+ (+ (* x (mval va input))
        (* y (mval vb input)))
     (mval vc input)))

(define (find-x x y input)
  (find-point x y #\a #\b #\e input))

(define (find-y x y input)
  (find-point x y #\c #\d #\f input))

(define range-width (- (cadr *RANGE_X*) (car *RANGE_X*)))
(define range-height (- (cadr *RANGE_Y*) (car *RANGE_Y*)))

; Calculates a pixel position and draws a point on it.
; We are only interested in this functions side-effects.
(define (draw-pixel vp x y)
  (let* ((pixel-x (* (/ (- x (car *RANGE_X*))
                        range-width)
                     *WIDTH*))
         (pixel-y (- (- *HEIGHT* 1)
                     (* (/ (- y (car *RANGE_Y*)) range-height)
                        *HEIGHT*)))
         (posn (make-posn (round pixel-x) (round pixel-y))))
    (if (and (>= pixel-x 0) (>= pixel-y 0)
             (< pixel-x *WIDTH*) (< pixel-y *HEIGHT*))
      ((draw-pixel vp) posn "green"))))

; Plots the next point on the canvas using x,y as the
; seed and i as the iteration.
(define (plot-points vp i x y)
  (let* ((input (choose-function 0 (random)))
         (next-x (find-x x y input))
         (next-y (find-y x y input)))
    (draw-pixel vp next-x next-y)
    (if (< i *ITERATIONS*)
      (plot-points vp (+ i 1) next-x next-y))))

; Initialising function. This is the one you should invoke!
(define (draw-fern)
  (open-graphics)
  (let ((vp (open-viewport "Fractals - Bernley's Fern" *WIDTH* *HEIGHT*)))
    (plot-points vp 0 1 1)))
