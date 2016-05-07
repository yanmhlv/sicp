#lang racket

;(define (make-rat n d) (cons n d))
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
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;(define one-half (make-rat 1 2))
;(print-rat one-half)

; ex 2.1
(define (make-rat n d)
  (if (< d 0)
      (cons (* n -1) (* d -1))
      (cons n d)))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
