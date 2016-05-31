#lang racket


(define (average a b) (/ (+ a b) 2))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (abs x)
  (if (< x 0) (- x) x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (* -1 (/ n g)) (* -1 (/ d g)))
        (cons (/ n g) (/ d g))
        )))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom y) (numer x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

;(print-rat (make-rat 10 -20))

; ex 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (mindpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(print-point (mindpoint-segment (make-segment (make-point -1 2) (make-point 3 -6))))
;(print-point (mindpoint-segment (make-segment (make-point 6.4 3) (make-point -10.7 4))))

; ex 2.3
(define (make-rect a b) (cons a b))
(define (rect-width r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (rect-height r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))
(define (rect-perimeter r)
  (* 2 (+ (rect-width r) (rect-height r))))
(define (rect-area r)
  (* (rect-width r) (rect-height r)))

(define a (make-point 0 0))
(define b (make-point 4 5))
(define r (make-rect a b))
(rect-perimeter r)
(rect-area r)





(define (cons x y)
    (define (dispatch m)
        (cond ((= m 0) x)
            ((= m 1) y)
            (else (error "Аргумент не 0 или 1 -- CONS" m))))
    dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))


; ex 2.4
(define (cons x y)
    (lambda (m) (m x y)))
(define (car z)
    (z (lambda (p q) p)))
(define (cdr z)
    (z (lambda (p q) q)))

; ex 2.5
(define (expt b n)
    (define (internal-expt n result)
        (cond ((= n 0) 1)
            ((= n 1) result)
            (else (internal-expt (- n 1) (* result b)))))
    (internal-expt n b))
(define (cons a b)
    (* (expt 2 a) (expt 3 b)))
(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
        (iter (/ x d) (+ 1 result))
        result))
  (iter n 0))

(define (car z)
    (num-divs z 2))
(define (cdr z)
    (num-divs z 3))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
        (min p1 p2 p3 p4)
        (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
            (/ 1.0 (lower-bound y)))))

; 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))

; 2.8
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
        (- (upper-bound x) (lower-bound y))))

; 2.10
(define (spans-zero? y)
    (and (<= (lower-bound y) 0) (<= (upper-bound y) 0)))

(define (div-interval x y)
    (if (spans-zero? y)
        (error "Error: denom should not span 0")
        (mul-interval x
            (make-interval (/ 1.0 (upper-bound y))
                (/ 1.0 (lower-bound y))))))


(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

; ex 2.12
(define (make-center-percent c p)
    (make-center-width c (* c (/ p 100.0))))
(define (percent i)
    (* 100.0 (/ (width i) (center i))))

; 2.14
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
        (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
            (add-interval (div-interval one r1)
                (div-interval one r2)))))

(par1 (make-interval 1 2) (make-interval 2 3))
(par2 (make-interval 1 2) (make-interval 2 3))
