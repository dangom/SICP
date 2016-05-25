					; Symbolic Differentiation

; Is variable?
(define (variable? x)
  (symbol? x))

; Is same-variable?
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? x y)
  (and (number? x) (= x y)))
  
; Make-sum and make-product
(define (make-sum x y)
  (cond ((=number? x 0) y)
	((=number? y 0) x)
	((and (number? x) (number? y) (+ x y)))
	(list '+ x y)))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
	((=number? x 1) y)
	((=number? y 1) x)
	((and (number? x) (number? y)) (* x y))
	(else (list '* x y))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend x)
  (cadr x))
(define (augend x)
  (accumulate make-sum 0 (cddr x)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))  
(define (multiplier x)
  (cadr x))
(define (multiplicand x)
  (accumulate make-product 1 (cddr x)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))

(define (make-exponentiation x y)
  (cond ((=number? x 0) 0)
	((=number? x 1) 1)
	((=number? y 1) x)
	((and (number? x) (number? y)) (expt x y))
	(else (list '** x y))))


(define (deriv exp var)
  (cond ( (number? exp) 0)
	( (variable? exp)
	  (if (same-variable? exp var) 1 0))
	( (sum? exp)
	  (make-sum (deriv (addend exp) var)
		    (deriv (augend exp) var)))
	( (product? exp)
	  (make-sum
	   (make-product (multiplier exp)
			 (deriv (multiplicand exp) var))
	   (make-product (deriv (multiplier exp) var)
			 (multiplicand exp))))
	( (exponentiation? exp)
	  (make-product
	   (make-product (exponent exp)
			 (make-exponentiation (base exp) (- (exponent exp) 1)))
	   (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))


	
