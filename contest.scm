;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Mandel-Bro
;;;
;;; Description:
;;;   <Heard you like fractals
;;;    So I put a fractal
;;;    Inside your fractal.>

;;; Turtle movement utilities
(define (move distance direction)
  (setheading direction)
  (forward distance))

(define (up d)
  (move d 0))
(define (right d)
  (move d 90))
(define (down d)
  (move d 180))
(define (left d)
  (move d 270))

;;; Move the turtle down a line
(define (linefeed width)
  (penup)
  (down 1)
  (left width)
  (pendown))

;;; Render a function f, that outputs colors given screen coords as input
;;; to the screen using the turtle.
(define (render-function f width height)
  (let iter-xy ((x 0) (y 0))
    (cond ((= y height) 'done)
          ((= x width) (linefeed width) (iter-xy 0 (inc y)))
          (else (pendown)
                (color (f x y))
                (right 1)
                (iter-xy (inc x) y)))))

;;; Normalize screen coords to -1.0 -> 1.0 domain
(define (normalize-screencoords x y width height)
    (vec2-mul (vec2-sub (vec2-mul (vector 2 2)
                                  (vec2-div (vector x y)
                                            (vector width height)))
                        (vector 1 1))
              (vector (/ width height) -1)))

;;; Scheme Standard Collection Functions

(define (find pred lst)
  (let ((res (find-tail pred lst)))
    (if res (car res) #f)))

(define (find-tail pred lst)
  (let recur ((lst lst))
    (and (not (null? lst))
	 (if (pred (car lst)) lst
	     (recur (cdr lst))))))

(define (any-empty? lists)
  (if (find null? lists) #t #f))

(define (cdrs lists)
  (if (any-empty? lists) '()
      (map1 cdr lists)))

(define (cars lists)
  (if (any-empty? lists) '()
      (map1 car lists)))

(define (cars+ lists last-elm)
  (if (pair? lists)
      (cons (caar lists)
            (cars+ (cdr lists) last-elm))
      (list last-elm)))

(define (cars+cdrs lists)
  (if (any-empty? lists) '()
      (list (map1 car lists)
            (map1 cdr lists))))

(define (fold-left1 kons knil lst)
  (if (null? lst) knil
      (fold-left1 kons
             (kons (car lst) knil)
             (cdr lst))))

(define (fold-left-many kons knil lists)
  (if (any-empty? lists) knil
      (let ((items (cars+ lists knil))
            (rest (cdrs lists)))
        (fold-left-many kons
              (apply kons items)
              rest))))

(define (fold-left kons knil lst . lists)
  (if (pair? lists)
      (fold-left-many kons knil (cons lst lists))
      (fold-left1 kons knil lst)))

(define (fold-right kons knil lst . lists)
  (if (pair? lists)
      (let recur ((lists (cons lst lists)))
        (let ((cdrs (cdrs lists)))
             (if (null? cdrs) knil
                 (apply kons (cars+ lists (recur cdrs))))))
      (let recur ((lst lst))
        (if (null? lst) knil
            (let ((head (car lst)))
              (kons head (recur (cdr lst))))))))

(define (map1 f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define (map f lst . lists)
  (apply fold-right
         (cons (lambda (x y) (cons (f x) y)) (cons '() (cons lst lists)))))

(define (reduce f lst)
  (fold-left f (car lst) (cdr lst)))

(define (vector-map! f vec . vecs)
  (let recur ((index 0)
              (len (vector-length vec))
              (vecs (cons vec vecs)))
    (if (= index len) vec
        (begin (vector-set! vec index
                            (apply f (apply map (list (lambda (vec)
                                                        (vector-ref vec index))
                                                      vecs))))
               (recur (inc index) len vecs)))))

(define (vector-map f vec . vecs)
  (apply vector-map! (cons f (cons (vector-copy vec) vecs))))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (min . xs)
  (reduce (lambda (x y) (if (< x y) x y)) xs))

(define (max . xs)
  (reduce (lambda (x y) (if (> x y) x y)) xs))

;;; Vectors
;;; =======
;;;
;;; ; I initially implemented vector operations in a much more general
;;; ; form, such that they could take any sort of numeric parameter and
;;; ; apply to vectors.
;;; ;
;;; ; Unfortunately, it was hella slow (by almost 2 orders of magnitude),
;;; ; so I ditched that in favor of speedy, ugly, unsafe vector ops.
;;;
;;; ; Original implementation (for reference)
;;;
;;; (define (vector-op f modify)
;;;   (lambda (. vecs)
;;;     (letrec ((min-length (apply min (map vector-length (filter vector? vecs))))
;;;              (convert-vec (lambda (vec) (if (vector? vec) vec (make-vector min-length vec))))
;;;              (vecs (map convert-vec vecs)))
;;;       (if modify
;;;           (apply vector-map! (cons f vecs))
;;;           (apply vector-map (cons f vecs))))))
;;;
;;; (define vector-add (vector-op + #f))
;;; (define vector-sub (vector-op - #f))
;;; (define vector-mul (vector-op * #f))
;;; (define vector-div (vector-op / #f))
;;; (define vector-mod (vector-op modulo #f))
;;;
;;; (define vector-add! (vector-op + #t))
;;; (define vector-sub! (vector-op - #t))
;;; (define vector-mul! (vector-op * #t))
;;; (define vector-div! (vector-op / #t))
;;; (define vector-mod! (vector-op modulo #t))

;;; FAST VECTOR OPS
;;; ===============
;;;
;;; Built for minimizing function calls at the expense of space and breaking of
;;; abstraction barriers :c
;;;
;;; Unroll the loops of common vector ops 'cause they're too slow :(

(define (vec2-map1 f vec)
  (vector (f (vector-ref vec 0))
          (f (vector-ref vec 1))))
(define (vec2-map2 f vec1 vec2)
  (vector (f (vector-ref vec1 0)
             (vector-ref vec2 0))
          (f (vector-ref vec1 1)
             (vector-ref vec2 1))))

(define (vec2-map1! f vec)
  (vector-set! vec 0 (f (vector-ref vec 0)))
  (vector-set! vec 1 (f (vector-ref vec 1))))
(define (vec2-map2! f vec1 vec2)
  (vector-set! vec1 0 (f (vector-ref vec1 0)
                         (vector-ref vec2 0)))
  (vector-set! vec1 1 (f (vector-ref vec1 1)
                         (vector-ref vec2 1))))

(define (vec3-map1 f vec1)
  (vector (f (vector-ref vec1 0))
          (f (vector-ref vec1 1))
          (f (vector-ref vec1 2))))
(define (vec3-map2 f vec1 vec2)
  (vector (f (vector-ref vec1 0)
             (vector-ref vec2 0))
          (f (vector-ref vec1 1)
             (vector-ref vec2 1))
          (f (vector-ref vec1 2)
             (vector-ref vec2 2))))

;;; Shortcut constructors
(define (vec2 x) (vector x x))
(define (vec3 x) (vector x x x))

(define (vec2-add vec1 vec2) (vec2-map2 + vec1 vec2))
(define (vec2-sub vec1 vec2) (vec2-map2 - vec1 vec2))
(define (vec2-mul vec1 vec2) (vec2-map2 * vec1 vec2))
(define (vec2-div vec1 vec2) (vec2-map2 / vec1 vec2))
(define (vec2-mod vec1 vec2) (vec2-map2 modulo vec1 vec2))

(define (vec2-add! vec1 vec2) (vec2-map2! + vec1 vec2))
(define (vec2-mul! vec1 vec2) (vec2-map2! * vec1 vec2))

(define (vec3-add vec1 vec2) (vec3-map2 + vec1 vec2))
(define (vec3-mul vec1 vec2) (vec3-map2 * vec1 vec2))
(define (vec3-sin vec1) (vec3-map1 sin vec1))
(define (vec3-sqrt vec1) (vec3-map1 sqrt vec1))
(define (vec3-abs vec1) (vec3-map1 abs vec1))

(define (vector-reduce f vec)
  (let iter ((accum (vector-ref vec 0))
             (index 1)
             (len (vector-length vec)))
    (if (= index len) accum
      (iter (f accum (vector-ref vec index)) (inc index) len))))

;(define (vec2-dot a b) (vector-reduce + (vec2-mul a b)))
(define (vec2-dot a b)
  (+ (* (vector-x a) (vector-x b))
     (* (vector-y a) (vector-y b))))
(define (vec2-dot1 a) (vec2-dot a a))

(define (clamp x lower upper)
  (cond ((< x lower) lower)
        ((> x upper) upper)
        (else x)))

(define (vector-clamp vec lower upper)
  (vector-map (lambda (x) (clamp x lower upper)) vec))

(define (vector-swizzle vec indices)
  (let recur ((new-vec (make-vector (length indices)))
              (index 0)
              (indices indices))
    (if (null? indices) new-vec
        (begin (vector-set! new-vec index (vector-ref vec (car indices)))
               (recur new-vec (inc index) (cdr indices))))))

(define (vector-x vec) (vector-ref vec 0))
(define (vector-y vec) (vector-ref vec 1))
(define (vector-z vec) (vector-ref vec 2))

(define (step edge x)
  (if (< x edge) 0 1))

;;; TESTS
;(vector-fract (vector 1.23 3.33))
;; expect #(0.23 0.33)

;(vector-mul (vector 1 2 3) (vector 4 5 6) (vector 2 2 2))
;; expect #(8 20 36)
;(vector-mul (vector 1 2 3) 5)
;; expect #(5 10 15)
;(vector-mul 5 (vector 1 2 3))
;; expect #(5 10 15)
;(vector-sub (vector 1 20 8) (vector 4 5 6) (vector 2 2 2))
;; expect #(-5 13 0)
;(vector-mod (vector 50 100) (vector 30 30))
;; expect #(20 10)

;(let ((vec1 (vector 1 2 3)) (vec2 (vector 4 5 6)) (vec3 (vector 2 2 2)))
  ;(vector-mul! vec1 vec2 vec3)
  ;vec1)
;; expect #(8 20 36)

;(vec2-add (vector 3 4) (vector 5 6))
;; expect #(8 10)
;(vec2-sub (vector 3 4) (vector 5 6))
;; expect #(-2 -2)
;(vec2-mul (vector 3 4) (vector 5 6))
;; expect #(15 24)
;(vec2-div (vector 3 3) (vector 5 6))
;; expect #(0.6 0.5)
;(vec2-mod (vector 7 8) (vector 3 4))
;; expect #(1 0)

;(vec3-add (vector 1 2 3) (vector 4 5 6))
;; expect #(5 7 9)
;(vec3-sqrt (vector 4 9 16))
;; expect #(2.0 3.0 4.0)

;(vector-clamp (vector 1 2 0.4 1.1 -0.5) 0 1)
;; expect #(1 1 0.4 1 0)

;(vector-reduce + (vector 1 2 3))
;; expect 6

;(vec2-dot (vector 7 2) (vector 9 9))
;; expect 81

;(vector-swizzle (vector 1 2 3 4) '(3 1 1 2))
;; expect #(4 2 2 3)

;(normalize-screencoords 240 480 480 640)
;; expect #(0 -0.5)

;(step 0.5 1)
;; expect 0
;(step 1.5 1)
;; expect 1

;;; Mandelbrot Set
;;; ==============
;;;
;;; WARNING: This takes a while to render (~1 hour on my crappy laptop for a
;;; small 128x128 image)

(define width 512)
(define height 512)
(define center (vector -0.534 0.525))
(define zoom (/ 1 100))
(define max-iter 100)
(define escape-radius (* 8 8))
(define point-trap-pos (vector -0.5 2.0))

(define (colorize z dz dist-trap point-trap co2)
  (letrec (;;; d(c) = |Z|·log|Z|/|Z'|
           (distance (* (sqrt (max (/ (vec2-dot1 z) (+ (vec2-dot1 dz) 1e-30)) 0))
                        (log (+ (vec2-dot1 z) 1e-30))))
           (escape-scaled (expt (clamp (* 2 (/ distance zoom)) 0 1) 0.5))
           (dist-scaled (* 4 (expt (clamp (* 1.5 (/ dist-trap (+ co2 1e-30))) 0 1) 2)))
           (point-scaled (* 2 (expt (clamp (* 0.8 point-trap) 0 1) 0.25)))
           (dist-color (vec3-add
                         (vec3 0.5)
                         (vec3-mul
                           (vec3 0.5)
                           (vec3-sin (vec3-add
                                       (vector 3 3.5 4)
                                       (vec3 dist-scaled))))))
           (point-color (vec3-add
                         (vec3 0.5)
                         (vec3-mul
                           (vec3 0.5)
                           (vec3-sin (vec3-add
                                       (vector 4.2 4.7 4.2)
                                       (vec3 point-scaled)))))))
    (vector-clamp
      (vec3-mul (vec3 2)
                (vec3-sqrt (vec3-abs (vec3-mul (vec3-mul dist-color
                                                         point-color)
                                               (vec3 escape-scaled)))))
      0 1)))

(define (point-color x y)
  (print (list 'point-color 'x x 'y y))
  (let ((pos (vec2-add (vec2-mul (normalize-screencoords x y width height)
                                 (vector zoom zoom))
                                 center))
        (z (vector 0 0))
        (dz (vector 1 0))
        (dist-trap 0)
        (dist 0)
        (ff 0)
        (point-trap 1e20)
        (co2 0))
    (let iter ((i 0))
      (if (or (> (vec2-dot1 z) escape-radius) (= i max-iter))
          nil
          ;;; Hot Code Zone !!
          ;;; Semi-optimized code (may be less readable than it could be)
          (begin ;;; Z' -> 2·Z·Z' + 1
                 (set! dz (vector
                            (- (* (vector-x z) (vector-x dz))
                               (* (vector-y z) (Vector-y dz)))
                            (+ (* (vector-x z) (vector-y dz))
                               (* (vector-y z) (vector-x dz)))))
                 (vec2-mul! dz (vec2 2))
                 (vec2-add! dz (vector 1 0))
                 ;;; Z -> Z² + c
                 (set! z (vector
                           (- (* (vector-x z) (vector-x z))
                              (* (vector-y z) (vector-y z)))
                           (* 2 (vector-x z) (vector-y z))))
                 (vec2-add! z pos)
                 (set! dist (abs (vec2-dot (vec2-sub z (vector 0 1))
                                           (vec2 0.707))))
                 (set! ff (step dist 1))
                 (set! co2 (+ co2 ff))
                 (set! dist-trap (+ dist-trap (* ff dist)))
                 (set! point-trap (min point-trap
                                       (vec2-dot1 (vec2-sub z point-trap-pos))))
                 (iter (inc i)))))
    (colorize z dz dist-trap point-trap co2)))

(define (render-mandelbrot)
  (render-function point-color width height))

(define (draw)
  (speed 0)
  (setheading 90)
  (render-mandelbrot)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)
