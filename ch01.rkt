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

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (fact-iter val acc)
    (if (= val 1)
        acc
        (fact-iter (- val 1) (* acc val))))
  (fact-iter n 1))

(= (factorial 50) (factorial-iter 50))

;; writing (+ a b) is throwing error
;; (+ 4 3) => (inc (+ 3 3)) => (inc (inc (+ 2 3)))
;; +: undefined;
;;  cannot reference an identifier before its definition
;;   in module: "/Users/mukeshtiwari/Programming/Code/sicp/ch01.rkt"
;; Context:
;;  /Users/mukeshtiwari/Programming/Code/sicp/ch01.rkt:1:1 [running body]
(define ('+ a b)
  (if (= a 0)
      b
      (inc ('+ (dec a) b))))

;; => (inc (inc (inc (+ 1 3))))
;; => (inc (inc (inc (inc (+ 0 3)))))
;; => (inc (inc (inc (inc 3))))
;; => (inc (inc (inc 4)))
;; => (inc (inc 5))
;; => (inc 6)
;; => 6
;; recursive process

(define (++ a b)
  (if (= a 0)
      b
      (++ (dec a) (inc b))))

;; (+ 4 3) => (+ 3 4) => (+ 2 5) =>
;; (+ 1 6) => (+ 0 7) => 7
;; constant space. Iterative process

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;; (A 1 10) => (A 0 (A 1 9)) =>
;; (* 2 (A 1 9)) => (* 2 (A 0 (A 1 8))) =>
;; (* 2 (* 2 (A 1 8))) => will continue till
;; (*2 (* 2 (* 2 ....... (* 2 (A 1 1))))) =>
;; and (A 1 1) will evaluate to 2

;; (A 2 4) => (A 1 (A 2 3)) =>
;; (A 0 (A 1 (- (A 1 (A 2 2)) 1))) =>
;; (* 2 (A 1 (- (A 1 (A 2 2)) 1))) =>
;; (* 2 (A 0 (A 1 (- (- (A 2 3) 1) 1))))
;; and I can't keep track in my mind. Mind fuck. Even not tracking
;; inside function. It will be power of two

(define (ff n) (A 0 n)) ;; 2 * n

(define (gg n) (A 1 n)) ;; 2 ^ n

(define (hh n) (A 2 n)) ;; it is kind of tower. 2, 2 ^ 2 , 2 ^ (2 ^ 2)
;; 2 ^ (2 ^ (2 ^ 2)) (2 ^ (2 ^ (2 ^ (2 ^ 2))))
;; if nth power v then next wil 2 ^ (2 ^ v)

(define (kk n) (* 5 n n)) ;; 5 n ^ 2

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else
     (+ (fib (- n 1))
        (fib (- n 2))))))

(define (fib-improved n)
  (define (fib-iter cnt a b)
    (cond
      ((>= cnt n) a)
      (else (fib-iter (+ cnt 1) b (+ a b)))))
  (fib-iter 0 0 1))

(= (fib 20) (fib-improved 20))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kind-of-coin)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (= kind-of-coin 0)) 0)
    (else (+ (cc amount (- kind-of-coin 1))
             (cc (- amount (first-denomination kind-of-coin))
                 kind-of-coin)))))

(define (first-denomination kind-of-coin)
  (cond
    ((= kind-of-coin 1) 1)
    ((= kind-of-coin 2) 5)
    ((= kind-of-coin 3) 10)
    ((= kind-of-coin 4) 25)
    ((= kind-of-coin 5) 50)))

(define (recur-fun n)
  (if (< n 3)
      n
      (+ (recur-fun (- n 1))
         (* 2 (recur-fun (- n 2)))
         (* 3 (recur-fun (- n 3))))))
;; The given equation can be written in matrix form.
;; (f(n) f(n+1) f(n+2))^T =
;; ((1 2 3) (3 5 3) (8 9 9)) (f(n-1) f(n-2) f(n-3))^T
;; this method can be used to compute the M^n in log n.

(define (recur-fun-iter n)
  (define (inner-iter-fun cnt a b c)
    (cond
      ((< cnt 0) cnt)
      ((= cnt 0) a)
      (else (inner-iter-fun (- cnt 1) b c
                            (+ (* 3 a) (* 2 b) c)))))
  (inner-iter-fun n 0 1 2))
