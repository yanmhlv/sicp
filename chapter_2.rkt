#lang racket


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

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat 10 -20))
