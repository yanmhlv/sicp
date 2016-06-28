(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
(append (list 1 2) (list 3 4))

(define (map f sequence)
    (if (null? sequence)
        null
        (cons (f (car sequence)) (map f (cdr sequence)))))
(map (lambda (x) (* x x)) (list 1 2 3))

; (define (filter predicate sequence)
;     (cond ((null? sequence) null)
;         ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
;         (else (filter predicate (cdr sequence)))))
; (define (filter predicate sequence)
;     (define (iter next answer)
;         (if (null? next)
;             answer
;             (if (predicate (car next))
;                 (iter (cdr next) (append answer (list (car next))))
;                 (iter (cdr next) answer))))
;     (iter sequence null))
(define (filter predicate sequence)
    (cond ((null? sequence) null)
        ((predicate (car sequence))
            (cons (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter (lambda (x) (= (remainder x 2) 0)) (list 10 2 3 4 5))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (accumulate op (append initial (list (op (car sequence)))) (cdr sequence))))
(define (square x) (* x x))
(accumulate square null (list 1 2 3))

(define (reverse items)
    (if (null? items)
        null
        (append (reverse (cdr items)) (list (car items)))))
(reverse (list 1 2 99 5))

(define (deep-reverse tree)
    (cond
        ((null? tree) null)
        ((pair? (car tree))
            (append (deep-reverse (cdr tree)) (list (deep-reverse (car tree)))))
        (else
            (append (deep-reverse (cdr tree)) (list (car tree))))))
(deep-reverse (list (list 1 2) (list 3 4)))

(define (enumerate-tree tree)
    (cond
        ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else
            (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))
(enumerate-tree (list (list 3 4 (list 1 2))))

; (define (length items)
;     (if (null? items)
;         0
;         (+ 1 (length (cdr items)))))
(define (length items)
    (define (length-iter items count)
        (if (null? items)
            count
            (length-iter (cdr items) (+ 1 count))))
    (length-iter items 0))
(length (list 1 2 3 4))

(define (length-tree tree)
    (length (enumerate-tree tree)))
(length-tree (list (list 3 4 (list 1 2))))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(subsets (list 1 2 3 4))
