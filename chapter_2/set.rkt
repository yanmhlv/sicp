#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
      ((= x (car set)) set)
      ((< x (car set)) (cons (x set)))
      ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(intersection-set (list 1 2 3 8) (list 1 2 8))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
    (else
     (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2) (list 1 3 4))
