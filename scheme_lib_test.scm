; Scheme Standard library tests

; named let, letrec syntax tests
(let ((foo 2)) (+ foo 3))
; expect 5

(letrec ((swing (lambda (t)
  (if (eq? (car t) 'tarzan)
      (cons 'vine (cons 'tarzan (cddr t)))
      (cons (car t) (swing (cdr t)))))))
  (swing '(vine tarzan vine vine)))
; expect (vine vine tarzan vine)

(letrec ((foo 5) (bar (+ 5 foo)))
  (* bar foo))
; expect 50

(letrec ((fac (lambda (n)
  (if (zero? n)
    1
    (* n (fac (- n 1)))))))
  (fac 10))
; expect 3628800

(let fac ((n 10))
  (if (zero? n)
      1
      (* n (fac (- n 1)))))
; expect 3628800

; set!, set-car!, set-cdr! syntax check
(let ((x 5))
  (set! x 10)
  x)
; expect 10
(let ((x '(1 2 3)))
  (set-car! x 10)
  x)
; expect (10 2 3)
(let ((x '(1 2 3)))
  (set-cdr! x 10)
  x)
; expect (1 . 10)

(abs -5.5)
; expect 5.5
(sqrt 4.0)
; expect 2.0
(expt 2 10)
; expect 1024.0

; vectors
(vector 1 2 3 '(1 2 3) 5 'foo)
; expect #(1 2 3 (1 2 3) 5 foo)
(vector? (vector 1 2 3))
; expect True
(vector? (list 1 2 3))
; expect False
(make-vector 5 'foo)
; expect #(foo foo foo foo foo)
(make-vector 5)
; expect #(None None None None None)
;(make-vector -1)
;; expect Error: cannot make vector of size less than 0
(vector-length (vector 1 2 3))
; expect 3
(vector-ref (vector 1 '(1 2 3) 3) 1)
; expect (1 2 3)
;(vector-ref (vector 1 '(1 2 3) 3) 3)
;; expect Error: index 3 out of vector range
(let ((vec (vector 1 2 3)))
  (vector-set! vec 1 'foo)
  vec)
; expect #(1 foo 3)
(vector-map (lambda (x) (* x 2)) (vector 1 2 3 4))
; expect #(2 4 6 8)
(vector-map + (vector 1 2 3 4) (vector 4 3 2 1))
; expect #(5 5 5 5)
(letrec ((vec (vector 1 2 3 4)) (vec2 (vector-copy vec)))
  (vector-set! vec2 2 'foo)
  (list vec vec2))
; expect (#(1 2 3 4) #(1 2 foo 4))
(let ((count 0)
      (vec (vector 1 2 3 4 5 6)))
  (vector-for-each (lambda (n) (set! count (inc count))) vec)
  count)
; expect 6

(xcons 4 5)
; expect (5 . 4)
(cons* 1 2 3 4)
; expect (1 2 3 . 4)
(cons* 1)
; expect 1

(let ((double (lambda (x) (* x 2))))
  ((compose double *) 5 10))
; expect 100

(caar (list (list 1 2) 3 4))
; expect 1
(cadr (list (list 1 2) 3 4))
; expect 3
(cdar (list (list 1 2) 3 4))
; expect (2)
(cddr (list (list 1 2) 3 4))
; expect (4)

(first '(1 2 3 4))
; expect 1
(second '(1 2 3 4))
; expect 2
(third '(1 2 3 4))
; expect 3
(fourth '(1 2 3 4))
; expect 4

(min 10 2 4 9 -10)
; expect -10
(max 10 2 4 9 -10)
; expect 10

(take '(1 2 3 4 5 6) 3)
; expect (1 2 3)
(drop '(1 2 3 4 5 6) 3)
; expect (4 5 6)

(map1 (lambda (x) (* x 2)) '(1 2 3 4 5 6))
; expect (2 4 6 8 10 12)

(find null? (list 1 2 3 4))
; expect False
(find null? (list 1 '() 3 4))
; expect ()
(any-empty? (list '(1 2 3) '(4 5 6) '()))
; expect True
(any-empty? (list '(1 2 3) '(4 5 6) '(3)))
; expect False

(cdrs (list '(1 2 3) '(2 3 4) '(10 9 8)))
; expect ((2 3) (3 4) (9 8))
(cars+ (list '(1 2 3) '(2 3 4) '(10 9 8)) 5)
; expect (1 2 10 5)
(cars+cdrs (list '(1 2 3) '(2 3 4) '(10 9 8)))
; expect ((1 2 10) ((2 3) (3 4) (9 8)))

(dotted-list? '(1 2 3 4))
; expect False
(dotted-list? '(1 2 3 . 4))
; expect True

(fold-left + 0 '(1 2 3 4 5))
; expect 15
(fold-left cons '() '(1 2 3 4 5))
; expect (5 4 3 2 1)
(fold-left + 4 '(3 3 3) '(1 1 1))
; expect 16
(fold-right cons '() '(1 2 3 4 5))
; expect (1 2 3 4 5)
(reduce + '(2 4 6 8))
; expect 20
(map (lambda (x) (+ x 5)) '(1 2 3 4 5))
; expect (6 7 8 9 10)
(filter even? '(1 2 3 4 5 6))
; expect (2 4 6)
