(define (cube x) (* x x x))

; (define (sum-integers a b)
;     (if (> a b)
;         0
;         (+ a (sum-integers (+ a 1) b))))

; (define (sum-cubes a b)
;     (if (> a b)
;         0
;         (+ (cube a) (sum-cubes (+ a 1) b))))

; (define (pi-sum a b)
;     (if (> a b)
;         0
;         (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
            (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
    (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
    (sum identity a inc b))

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(define (itersum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))

(define (factorial n)
    (define (identity x) x)
    (define (next x) (+ x 1))
    (product identity 1 next n))
(factorial 6)



; ex 1.32
#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (product-square a b)
  (product square a inc b))
(define (sum-square a b)
  (sum square a inc b))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

((lambda (a b) (accumulate-iter + 0 square a inc b)) 0 10)


((lambda (a b) (accumulate + 0 square a inc b)) 0 10)
(product-square 0 10)
(sum-square 0 10)


; ex 1.37, 1.38, 1.39
#lang racket

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                 ((negative? test-value)
                  (search f midpoint pos-point))
                 (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "У аргументов не разные знаки " a b)))))

;(half-interval-method sin 2.0 4.0)
;(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)


(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)
;(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
; (sqrt 2.0)

; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)


(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n 1) (d i))))
  (frac 1))
; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 50)

(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (if (= i 0)
        result
        (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter (- k 1) (/ (n k) (d k))))
; (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 50)

(define (d i)
   (if (not (= 0 (remainder (+ i 1) 3)))
       1
       (* 2 (/ (+ i 1) 3))))
; (cont-frac-iter (lambda (i) 1.0) d 100)


(define (square x) (* x x))
(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac-iter n d k))

; (tan (/ pi 6))
; (tan-cf (/ pi 6) 10)


(define (average-damp f x)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)) x) 1.0))
(sqrt 2)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))) x) 1.0))
(cube-root 100)


(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-2 x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))
(sqrt-2 9)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
; (sqrt-3 9)

(define (sqrt-4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
(sqrt-4 9)


; 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 3 -2.4 6) 1)

; 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(((double (double double)) inc) 5)

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

; 1.43
(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (- x 1)))))

((repeated square 2) 5)

; 1.44
(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(sin (/ pi 2))
((smooth sin 0.7) (/ pi 2))
(sin pi)
((smooth sin 0.7) pi)

(define (expt x n)
  (define (iter result count)
    (if (= count n)
        result
        (iter (* x result) (+ count 1))))
  (iter 1 0))

;(expt 4 2)

; 1.45 not worked
(define (nth-root x n)
  (fixed-point
   (average-damp
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

; (nth-root 100 2)

; 1.46
(define (iterative-improve close-enough? improve)
  (define (iter-imp guess)
    (if (close-enough? guess)
        guess
        (iter-imp (improve guess))))
  iter-imp)

(define (sqrt-5 x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess)))) 1.0))
(sqrt-5 9)
