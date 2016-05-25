;; Scheme exercises from chapter 2.
(define nil '())
;; Exercise 2.1

;; this is just a useful tool.
(define (xor a b)
  (and
   (not (and a b))
   (or a b)))

;; Greatest common denominator
(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

;; This version of make-rat produces a rational number,
;; such that, if negative, only the numerator is negative.
(define (make-rat num den)
  (let ((g (gcd num den)))
    (if (= (* num den) (* (abs num) (abs den)))
        (cons (/ num g) (/ den g))
        (cons (/ (- (abs num)) g) (/ den g)))))

(make-rat 3 4)
(make-rat -2 4)

;; Exercise 2.2
;; Representing a segment in a plane.
                                        ; Making points first. Could actually use it to build segments..
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

;; Make segment makes a segment out of two points.
(define (make-segment a b)
  (cons a b))

;; Methods to get the start and end points of the segment.
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;; Returns the mid-point of the segment
(define (mid-point-segment seg)
  (make-point
   (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
   (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define square (lambda(x) (* x x)))

(define (length seg)
  (sqrt (+
         (square (- (x-point (start-segment seg)) (x-point (end-segment seg))))
         (square (- (y-point (start-segment seg)) (y-point (end-segment seg)))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Testing. It has to be = (1,1)
(print-point (mid-point-segment (make-segment 0 0 2 2))) ;; OK
;; Testing. It has to be = (0,0)
(print-point (mid-point-segment (make-segment -1 -1 1 1))) ;; OK


;; Exercise 2.3 - Building rectangles.
;; To build a rectangle we need a segment for the origin,
;; one horizontal and one vertical. The origin will be defined
;; as the lower left corner.
(define (make-rectangle xseg yseg)
  (cons xseg yseg))
(define (xlength rect)
  (length (car rect)))
(define (ylength rect)
  (length (cdr rect)))

(define (area rect)
  (* (xlength rect) (ylength rect)))

(define (perimeter rect)
  (+
   (* 2 (xlength rect))
   (* 2 (ylength rect))))

;; Testing our make-rect
(define point-a (make-point 1 1))
(define point-b (make-point 1 3))
(define point-c (make-point 3 1))
(define point-d (make-point 3 3))

(define seg-vert (make-segment point-a point-b))
(define seg-hor (make-segment point-a point-c))

(define recttest (make-rectangle seg-hor seg-vert))

(area recttest)
(perimeter recttest)



;; Exercise 2.4
;; Different definitions of cons and car
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)0
  (z (lambda (p q) q)))
(cdr (cons 1 2))

;; Exerise 2.5
;; We can represent any pair of nonnegative integer numbers as 2^a*3^b
;; This program will need the exponential function.

;;Exerisce 2.6
;;We can implement zero and one as procedures:

(define zero (lambda (f) (lambda (x) x) ))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x) ))))


;; Exercise 2.17
;; Define a procedure that returns the list that contains only the last
;;element of a given (nonempty) list.
(define (last-pair items)
  (if (> 2 (length items))
      items
      (last-pair (cdr items))))

;; Another variant, from the SICP Solution Board - but fails for '().
;; It is faster, though, because it doesn't repeatedly call length.
(define (last-pair2 items)
  (let ((rest (cdr items)))
    (if (null? rest)
        items
        (last-pair2 rest))))

;; Third variant - fast and doesn't fail for '().
(define (last-pair3 items)
  (cond ((null? items) items)
        ((null? (cdr items)) (list (car items)))
        (else (last-pair3 (cdr items)))))

;; Exercise 2.18
;; Reversing a list
;; 1. Add first item of list to end of result list.
;; 2. Iter over cdr of original list, add first item to end.
(define (reverse-list items)
  (define (iter-reversed items result)
    (if (null? items)
        result
        (iter-reversed (cdr items) (cons (car items) result)))) ;; See how result it pushed to the back, so last in comes in front.
  (iter-reversed items '())) ;; Start the result as an empty list.

;; Exercise 2.20
;; Use the dotted-tail notation to create the procedures same-parity
(define (same-parity first . items)
  (define (speccar items)
    (cond ((null? items) 2)
          ((not (pair? items)) (modulo items 2))
          (else (modulo (car items) 2))))
  (define (iter-parity first items result)
    (let ((parityFirst (modulo first 2)) (samepar (speccar items)))
      (if (null? items)
          (reverse result) ;; Need to reverse because of tail recursion.
          (if (= parityFirst samepar)
              ;; Always cons an element to a list, not the other way around.
              (iter-parity first (cdr items) (cons (car items) result))
              (iter-parity first (cdr items) result)))))
  (iter-parity first items (list first))) ;; Start with a list.

;; Easy and short variant that makes use of build-in functions:
(define same-parity )

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
                                        ; In the example above, because of tail recursion the order of the output will be reversed.
                                        ; To correct for it we need to return (reverse answer).
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
;; In this example we are passing forming a pair of a list and an element. Bad solution.

;; Implementing a for-loop.
(define (for-each proc items)
  (if (null? items)
      nil
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; (1 2 3 4 5 6)
(cons x y); ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))


                                        ; Exercise 2.28 Defining fringe
(define (fringe object)
  (define (iter-fringe object result)
    (cond ((null? object) result)
          ((not (pair? object)) (cons object result))
          (else (append (iter-fringe (car object) '()) (iter-fringe (cdr object) '())))))
  (iter-fringe object '())) ;; Works!


;; Exercise 2.29
;;Binary modules - constructors given on exercise
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; Selectors
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile))) ; We need the car here, otherwise we'll be trapped in the list.

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch))) ; We need car here.

(define (structure-is-mobile? structure)
  (pair? structure))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (structure-is-mobile? s)
        (total-weight s)
        s)))

(require racket/trace)
(trace total-weight)

(define branch1 (make-branch 3 2))
(define branch2 (make-branch 2 3))
(define branch3 (make-branch 1 4))
(define branch4 (make-branch 3 5))
(define mobile1 (make-mobile branch1 branch2))
(define mobile2 (make-mobile branch3 branch4))

(total-weight mobile2)


;;Exercise 2.54
;; Implementing equal? from recursion

(define (equal? exp exp2)
  (if (eq? (car 'exp) (car 'exp2))
      (equal? (cdr 'exp) (cdr 'exp2))
      #false))

(equal? '+ '+)
