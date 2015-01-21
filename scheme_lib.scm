;;; Scheme standard library functions

(define (xcons d a) (cons a d))

; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
(define (cons* first . rest)
  (let recur ((first first) (rest rest))
    (if (pair? rest)
        (cons first (recur (car rest) (cdr rest)))
        first)))

(define (compose f g)
  (lambda args
    (f (apply g args))))

; Selectors
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caddr x) (car (cddr x)))
(define (cadddr x) (cadr (cddr x)))

(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (min . xs)
  (reduce (lambda (x y) (if (< x y) x y)) xs))

(define (max . xs)
  (reduce (lambda (x y) (if (> x y) x y)) xs))

(define (take lis k)
  (if (zero? k)
      '()
      (cons (car lis)
            (take (cdr lis) (dec k)))))

(define (drop lst k)
  (if (zero? k) lst
      (drop (cdr lst) (dec k))))

(define (find pred lst)
  (let ((res (find-tail pred lst)))
    (if res (car res) #f)))

(define (find-tail pred lst)
  (let lp ((lst lst))
    (and (not (null? lst))
	 (if (pred (car lst)) lst
	     (lp (cdr lst))))))

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

(define (reduce f lst)
  (fold-left f (car lst) (cdr lst)))

(define (map1 f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define (map f lst . lists)
  (apply fold-right
         (cons (lambda (x y) (cons (f x) y)) (cons '() (cons lst lists)))))

(define (for-each f lst . lists)
  (let recur ((lists (cons lst lists)))
    (if (pair? lists)
        (begin (apply f (cars lists))
               (recur (cdrs lists))))))

(define (filter f lst)
  (cond ((null? lst) nil)
        ((f (car lst)) (cons (car lst) (filter f (cdr lst))))
        (else (filter f (cdr lst)))))

; Removed in favor of faster, primitive version
;(define (vector-copy vec)
  ;(let recur ((new-vec (make-vector (vector-length vec)))
              ;(index 0)
              ;(len (vector-length vec)))
    ;(if (= len index) new-vec
        ;(begin (vector-set! new-vec index (vector-ref vec index))
               ;(recur new-vec (inc index) len)))))

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

(define (vector-for-each f vec . vecs)
  (let recur ((index 0)
              (len (vector-length vec))
              (vecs (cons vec vecs)))
    (if (= index len) nil
        (begin (apply f (apply map (list (lambda (vec)
                                           (vector-ref vec index))
                                         vecs)))
               (recur (inc index) len vecs)))))

