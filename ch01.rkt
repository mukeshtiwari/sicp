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
