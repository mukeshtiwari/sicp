#lang sicp

(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

;each programming language has means to abstract the computation
;(operator operands)

(+ 21 12 35 7)
(+ (* 3 5) (- 10 6))
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

(define size 2)
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi radius radius)
(define circumference (* 2 pi radius))

(define (square x ) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 5)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

;;Exercise 1.1
10
12
8
3
6
;a is 3 and b is 4
19
#f
4
16
6
16
(/ (+ 5 1 (- 2 (- 3 (+ 6 (/ 1 5)))))
   (* 3 (- 6 2) (- 2 7)))

(define (sum-of-square-two-large x y z)
  (cond
    ((and (> x y) (> x z))
     (if (> y z)
         (+ (square x) (square y))
         (+ (square x) (square z))))
    ((and (> y x) (> y z))
     (if (> x z)
         (+ (square y) (square x))))
    (else
     (if (> x y)
         (+ (square z) (square x))
         (+ (square z) (square y))))))

;when b is positive the expression evaluated to (+ a b)
; otherwise (- a b)
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether
;; the interpreter he is faced with is using applicative-order evaluation
;; or normal-order evaluation. He defines the following two procedures:

;; (define (p) (p))

;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))

;; Then he evaluates the expression

;; (test 0 (p))

;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe with
;; an interpreter that uses normal-order evaluation? Explain your answer.
;; (Assume that the evaluation rule for the special form if is the
;; same whether the interpreter is using normal or applicative
;; order: The predicate expression is evaluated first, and the
;; result determines whether to evaluate the consequent or the
;; alternative expression.)
;;Solution
;; This alternative ``fully expand and then reduce'' evaluation method
;; is known as normal-order evaluation, in contrast to the ``evaluate
;; the arguments and then apply'' method that the interpreter actually
;; uses, which is called applicative-order evaluation.
;; If evaluator uses applicative then infinite loop because (p) evaluates
;; to (p) but if it is normalized then it will terminate.
;; (test 0 (p)) => if (= 0 0) 0 (p) => 0. No evaluation of (p)
;; Laziness is great :)

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improved-good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.001))

(define (sqrt-iter guess x)
  (if (improved-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt n)
  (sqrt-iter 1.0 n))

;; The problem with defintion is, our evaluation order is applicative
;; so all three will evaluated. Try (new-if #t 1 (/ 1 0))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Because new-if is evaluating both branches, the complexity will
;; grow exponentially with each recursive call (power of 2).
;; try (sqrt-iter-bad 1 982349247982472947294729847).
;; This input is also not working for sqrt with good-enough.
;; Improve good-enough? to improved-good-enough? (Rate of change of
;; function dy/dx). Problem 1.7
(define (sqrt-iter-bad guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; It can be abstracted very nicely by passing function.
;; but try to see how verbose and stupid code I can write :)

(define (good-enough-cube? guess n)
  (< (abs (- guess (compute-next-guess guess n))) 0.0001))

(define (compute-next-guess guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (compute-next-guess guess x) x)))

(define (cube-root n)
  (cube-root-iter 1.0 n))

;; x/y^n + (n - 1) * y / n
(define (nth-next-guess y x n)
  (/ (+ (/ x (expt y (- n 1)))
        (* (- n 1) y)) n))

(define (nth-good-enough? y x n)
  (< (abs (- y (nth-next-guess y x n))) 0.0001))

(define (nth-root-iter y x n)
  (if (nth-good-enough? y x n)
      y
      (nth-root-iter (nth-next-guess y x n) x n)))

(define (nth-root x n)
  (nth-root-iter 1.0 x n))

(nth-root 125 3)
(nth-root 100 2)
(nth-root 32 5)
