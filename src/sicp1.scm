;; Structure and interpretation of computer programs

;; Exercise 1.12
;; Compute a procedure that returns Pascal numbers.
(define (pascal line column)
  (cond ((= column 0) 1)
        ((< line 0) 0)
        ((< column 0) 0)
        ((> column line) 0)
        (else (+ (pascal (- line 1) (- column 1)) (pascal (- line 1) column)))))

;; Exercise 1.15
(define (cube x) (* x x x))

(define counter 0)
(define (procsin x)
  (set! counter (+ counter 1)) ;; add this line to see how many times procsin is called.
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (procsin (sine (/ angle 3.0 )))))

(sine 12.15)
counter ;; Procsin was called 5 times.
;; Answer for item b is written in the book by hand.

;; Exercise 1.16 - fixed
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

;; Exercise 1.17
(define (my* a b)
  (if (= b 0)
      0
      (+ a (my* a (- b 1)))))

(define (mymult a b)
  (define (double a)
    (+ a a))
  (define (halve b)
    (/ b 2))
  (cond ((= b 0) 0)
        ((even? b) (mymult (double a) (halve b)))
        (else (+ a (mymult a (- b 1))))))

;; Exercise 1.18
;; The solution here is to change the multiplier(x) and multiplicand(y) so as
;; to reduce the problem to the multiplication of (x*y)*1. We can achieve
;; this by simultaneously halving the multiplicand and doubling the multiplier,
;; if the multiplicand is even, else, we add to the result decrementing the multiplicand.
;; Here, the result is only incremented when the the multiplier is odd.
(define (mymult multiplier multiplicand)
  (define (double a)
    (+ a a))
  (define (halve b)
    (/ b 2))
  (define (mymult-iter x y a)
    (cond ((= y 0) a)
          ((even? y) (mymult-iter (double x) (halve y) a))
          (else (mymult-iter x (- y 1) (+ a x)))))
  (mymult-iter multiplier multiplicand 0))

;; Exercise 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ ( * 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b p) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 0)
(fib 5)
(fib 10)
(fib 123)

;; Exercise 1.21 Smallest Divisor
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(smallest-divisor 199) ;; 199
(smallest-divisor 1999) ;; 1999
(smallest-divisor 19999) ;; 7


;; Exercise 1.22
(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define square (lambda (x) (* x x)))

(define (sqrt-iter guess x)
  (if good-enough? guess x)
  guess
  (sqrt-iter (improve guess x)
             x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)


;; Exercise 1.42
;; Composition of two functions

;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square sqrt) 6)


(define (double proc)
  (lambda (x) (proc (proc x))))

(define inc
  (lambda (x) (+ 1 x)))

(((double (double double)) inc) 5)
                                        ;((double double double double inc 5 -> 16inc 5 -> 21

;; 1.43
(define (repeated f n)
  (if (= 0 n) ;; it is better to use (< n 1), so that we avoid probs with neg. numb.
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define test
  (λ (a) (+ 2 a)))
