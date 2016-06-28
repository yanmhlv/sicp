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





#lang racket


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (if (null? seq1)
      seq2
      (cons (car seq1) (append (cdr seq1) seq2))))
(append (list 1 2) (list 3 4))

(define (enumerate-tree t)
  (cond ((null? t) null)
        ((not (pair? t)) (list t))
        (else (append (enumerate-tree (car t))
                      (enumerate-tree (cdr t))))))


; ex 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(map (lambda (x) (* x x)) (list 1 2 3))
(map (lambda (x) (+ x 10)) (list 1 2 3))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))
(append-2 (list 1 2) (list 3 4))

(define (length sequence)
  (accumulate (lambda (x y)
                (display x)
                (display " ")
                (display y)
                (newline)
                (+ y 1)) 0 sequence))
(length (list 0 0 0 0))

; ex 2.35
(define (count-leaves t)
  (accumulate + 0 (enumerate-tree t)))

; ex 2.36
(define (accumulate-n op init seqs)
  (cond ((null? (car seqs)) null)
        (else
         (display seqs)
         (newline)
         (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))))))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))



#lang racket

(cons 1
      (cons 2
            (cons 3
                  (cons 4 '()))))
(cadr (list 1 2 3 4))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(list-ref (list 1 2 3 4 5) 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(length (list 1 2 3 4))

(define (length-iter a count)
  (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
(length-iter (list 1 2 3 4) 0)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append (list 1 2) (list 3 4 5 6))

; ex 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))
(last-pair (list 1 2 3 4 72))

; ex 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
(reverse (list 1 2 3 4 7 9))

; ex 2.20
(define (same-parity first . items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items)
              (if (= (remainder (car items) 2)
                     (remainder first 2))
                  (append answer (list (car items)))
                  answer))))
  (iter items (list first)))
(same-parity 1 3 4 5 6 99)


(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(map (lambda (x) (+ x 10)) (list 1 2 3 4))

; (define (scale-list items factor)
;   (if (null? items)
;       '()
;       (cons (* (car items) factor)
;             (scale-list (cdr items) factor))))
; (scale-list (list 1 2 3 4 5) 10)
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(scale-list (list 1 2 3 4) 10)

; ex 2.21
(define (square x) (* x x))
; (define (square-list items)
;   (if (null? items)
;       '()
;       (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4))

; ex 2.23
(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))
(for-each (lambda (x)
            (display x)
            (newline))
          (list 1 2 3 4))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(length x)

; ex 2.25
(car (cdr (car (cddr (list 1 3 (list 5 7) 9)))))
(car (car (list (list 7))))
(car
 (cdr
  (car
   (cdr
    (car
     (cdr
      (car
       (cdr
        (car
         (cdr
          (car
           (cdr
            (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

; ex 2.26
(append (list 1 2 3) (list 4 5 6))
(cons (list 1 2 3) (list 4 5 6))
(list (list 1 2 3) (list 4 5 6))

; ex 2.27
(define (deep-reverse items)
  (cond ((null? items) null)
        ((pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items)))))
        (else
         (append (deep-reverse (cdr items))
                 (list (car items))))))
(deep-reverse (list (list 1 2) (list 3 4)))

; ex 2.28
(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define f (list (list 1 2) (list 3 4)))
(fringe (list f f))

; ex 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cadr b))

(define (branch-weight b)
  (let ((struct (branch-structure b)))
    (if (pair? struct)
        (total-weight struct)
        struct)))
(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (balanced? m)
  (define (branch-balanced? b)
    (if (pair? (branch-structure b))
        (balanced? (branch-structure b))
        true))
  (define (torgue b)
    (* (branch-length b) (branch-weight b)))
  (let ((left (left-branch m))
        (right (right-branch m)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (torgue left) (torgue right)))))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define b (make-mobile (make-branch 2 3) (make-branch 4 5)))
(total-weight a)
(total-weight b)
(balanced? a)
(balanced? b)
(define c (make-mobile (make-branch 5 a) (make-branch 3 b)))
(total-weight c)
(balanced? c)


; (define (scale-tree tree factor)
;   (cond ((null? tree) null)
;         ((not (pair? tree)) (* tree factor))
;         (else (cons (scale-tree (car tree) factor)
;                     (scale-tree (cdr tree) factor)))))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor))) tree))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
; ex 2.30
(define (square-tree-1 tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree-1 (car tree)) (square-tree-1 (cdr tree))))))
(square-tree-1 (list 1 (list 2) (list 3 4 5 (list 6))))
(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree))) tree))
(square-tree-2 (list 1 (list 2) (list 3 4 5 (list 6))))

; ex 2.31
(define (tree-map f tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))
(define (square-tree-3 tree)
  (tree-map square tree))
(square-tree-3 (list 1 (list 2) (list 3 4 5 (list 6))))

; ex 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(subsets (list 1 2 3))


; (define (sum-odd-squares tree)
;   (cond (( null? tree) 0)
;         ((not (pair? tree))
;          (if (odd? tree) (square tree) 0))
;         (else (+ (sum-odd-squares (car tree))
;                  (sum-odd-squares (cdr tree))))))
; (sum-odd-squares (list 1 (list 3 (list 5) (list 2 4 6 8))))

(define (fib k)
  (cond
    ((= k 1) 1)
    ((= k 0) 0)
    (else (+ (fib (- k 1)) (fib (- k 2))))))
; (define (even-fibs n)
;   (define (next k)
;     (if (> k n)
;         null
;         (let ((f (fib k)))
;           (if (even? f)
;               (cons f (next (+ k 1)))
;               (next (+ k 1))))))
;   (next 0))
; (even-fibs 10)



(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
(filter even? (list 1 2 3 4))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons null (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate + 0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(sum-odd-squares (list 1 (list 3 (list 5) (list 2 4 6 8))))

(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
(even-fibs 10)

(define (list-fib-squares n)
  (accumulate cons
              null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; (define (salary-of-highest-paid-programmer records)
;   (accumulate max
;               0
;               (map salary
;                    (filter programmer? records))))


; ex 2.33
(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(map-2 square (list 1 2 3))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))
(append-2 (list 1 2) (list 3 4))

(define (length-2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length-2 (list 1 2 3))

; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3))
(horner-eval 2 (list 1 3 0))
(horner-eval 2 (list 1 3 0 5))
(horner-eval 2 (list 1 3 0 5 0))
(horner-eval 2 (list 1 3 0 5 0 1))

; ex 2.35
(define (count-leaves-2 t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))
(count-leaves-2 (list))
(count-leaves-2 (list 1 2 3))
(count-leaves-2 (list 1 (list 1 2 3)))
(count-leaves-2 (list 1 (list 1 2) (list 1 2 3) 1))
