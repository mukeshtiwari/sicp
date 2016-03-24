#lang sicp
(#%require math)
;; (#%require racket/base)

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

(define (v) (v))

(define (test x y)
   (if (= x 0)
       0
       y))

;; Then he evaluates the expression

;; (test 0 (v))

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


;;given the list, generates the next list
;; problem is pascal (list 1) => list 1
(define (pascal row)
  (cond
    ((null? (cdr row)) (list 1))
    ((= 1 (car row)) (cons 1
                           (cons (+ 1 (cadr row))
                                 (pascal (cdr row)))))
    (else (cons (+ (car row) (cadr row))
                (pascal (cdr row))))))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; How many times is the procedure p applied when (sine 12.15) is evaluated?
;; (sine 12.15) => (p (sine 4.05)) => (p (p (sine 1.34999)))
;; (p (p (p (sine 0.449999)))) => (p (p ( p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05)))))
;; 5 times
;;What is the order of growth in space and number of steps (as a function
;; of a) used by the process generated by the sine procedure when (sine a)
;; is evaluated?
;; Both are in order of (log n). We are going down the tree and each time
;; we divide the input by 3. n => n / 3 => n / 9 === n / 3 ^ k
;; n / 3 ^ k = Constant => 3^k = n / Constant => k = log (n / Constant)
;; base 3. The process is recursive so it takes same amount of space also.
;; recursive process space O(n) and time O(n)

(define (expt-naive b n)
  (if (= 0 n)
      1
      (* b (expt-naive b (- n 1)))))

;; time linear and space constant
(define (expt-naive-iter b n)
  (define (inner-naive-iter acc cnt)
    (if (= 0 cnt)
        acc
        (inner-naive-iter (* b acc) (- cnt 1))))
  (inner-naive-iter 1 n))

;; time and space both log n
(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((= n 1) b)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

(= (fast-expt 12 12) (expt-naive 12 12))

(define (fast-expt-iter b n)
  (define (inner-expt-iter acc b n)
    (cond
      ((= n 0) acc)
      ((even? n) (inner-expt-iter acc (* b b) (/ n 2)))
      (else (inner-expt-iter (* acc b) b (- n 1)))))
  (inner-expt-iter 1 b n))

;; we can write multiplication and exponentiation in terms of
;; this function
(define (abst-function f acc b n)
  (cond
    ((= n 0) acc)
    ((even? n) (abst-function f acc (f b b) (/ n 2)))
    (else (abst-function f (f acc b) b (- n 1)))))

;;1.17
(define (fast-mult a b)
  (abst-function + 0 a b))

;;length should be equal
(define (vect-abstract f l m)
  (cond
    ((and (null? l) (null? m)) nil)
    (else (cons (f (car l) (car m)) (vect-abstract f (cdr l) (cdr m))))))

;;sum of vector
(define (sum l)
  (apply + l))

;;transpose the matrix
(define (transpose l)
  (apply map list l))

;;matrix multiplication
(define (mat-mult l m)
  (map (lambda (y)
         (map (lambda (x) (sum (vect-abstract * y x)))
              (transpose m))) l))

;;log n fibonnaci
(define (fibo-log n)
  (cadr (car (abst-function mat-mult
                            (list (list 1 0) (list 0 1))
                            (list (list 1 1) (list 1 0)) n))))
(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

;; this is for applicative.Evaluate the arguments before calling
;; (gcd 206 40) =>
;; (if (= 0 40) 240 (gcd 40 (remainder 206 40))) =>
;; (gcd 40 6) => (if (= 0 6) 40 (gcd 6 (remainder 40 6))) =>
;; (gcd 6 4) => (if (= 0 4) 6 (gcd 4 (remainder 6 4))) =>
;; (gcd 4 2) => (if (= 0 2) 4 (gcd 2 (remainder 4 2))) =>
;; (gcd 2 0). 4 remainder calls

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next div)
  (if (= div 2)
      (+ div 1)
      (+ div 2)))

(define (find-divisor n div)
  (cond
    ((> (* div div) n) n)
    ((= 0 (remainder n div)) div)
    (else (find-divisor n (next div)))))

(define (prime? n)
  (if (<= n 1)
      #f
      (= n (smallest-divisor n))))

;;b^n mod m
(define (abst-function-mod f g acc b n m)
  (cond
    ((= n 0) (g acc m))
    ((even? n) (abst-function-mod f g acc (g (f b b) m) (/ n 2) m))
    (else (abst-function-mod f g (g (f acc b) m) b (- n 1) m))))

(abst-function-mod * remainder 1 12 9894184719487 10)

(define (expmod base exp m)
  (abst-function-mod * remainder 1 base exp m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer 2 (- n 1)))))

;;now racket and scheme is mixing. The reason I am using math library
;; because there is no random function in sicp and racket's random
;; is throwing contract voilation
;; ch01.rkt﻿> (fast-primes? 3131203812039807 12)
;; random: contract violation
;;   expected: (or/c (integer-in 1 4294967087) pseudo-random-generator?)
;;   given: 3131203812039806

(define (fast-prime? n times)
  (cond
    ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      ;;(prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (define (odd-range lodd hodd)
    (if (<= lodd hodd)
        (timed-prime-test lodd))
    (if (<= lodd hodd) (odd-range (+ lodd 2) hodd)))
  (odd-range (if (odd? low) low (+ low 1))
             (if (odd? high) high (- high 1))))



(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)
(search-for-primes 1000000 1000037)

;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))
;; Certainly not because number will grow
;; exponentially and will be out of computer
;; memory

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (* (expmod base (/ exp 2) m)
;;                        (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; (define (double x) (+ x x))
;; Because it is not saving the intermediate result and instead
;; it is calling again. Generating tree when number is even
;; Also one point to note that in Scheme the evaluation is
;; applicative so double (expmod base (/ exp 2) m) is called
;; it is completely evaluated. Had the evaluation been
;; normal, it would have no affect by calling double.

(define (charmicheal-number n)
  (define (inner-charmicheal a n)
    (if (= a n)
        #t
        (if (= (expmod a n n) a)
            (inner-charmicheal (+ a 1) n)
            false)))
  (inner-charmicheal 1 n))

;; (map charmicheal-number (list 561 1105 1729 2465 2821 6601))

;; try to solve miller-rabin problem

(define (cube-mine x)
  (* x x x))

(define (sum-integer a b)
  (if (> a b)
      0
      (+ a (sum-integer (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube-mine a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum-term term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-term term (next a) next b))))

(define (sum-cubes-abst a b)
  (sum-term cube-mine a inc b))

(define (sum-integer-abst a b)
  (sum-term identity a inc b))

(define (pi-sum-abst a b)
  (sum-term (lambda (x) (/ 1.0 (* x (+ x 2))))
            a
            (lambda (x) (+ x 4))
            b))

(define (integral f a b dx)
  (* (sum-term f
               (+ a (/ dx 2.0))
               (lambda (x) (+ x dx))
               b) dx))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (if (odd? k)
        (* 4.0 (f (+ a (* k h))))
        (* 2.0 (f (+ a (* k h))))))
  (define (inner-simpson-rule cnt acc)
    (if (>= cnt n)
        acc
        (inner-simpson-rule (+ cnt 1)
                            (+ acc (y cnt)))))
  (* (/ h 3) (inner-simpson-rule 1 (+ (f a) (f b)))))


(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral-iter f a b dx)
  (* (sum-iter f
               (+ a (/ dx 2.0))
               (lambda (x) (+ x dx))
               b) dx))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; + and * can be easily abstracted by function and passing
;; identity value. Oh next 1.32 is exactly talking about this

(define (fact n)
  (product identity 1 inc n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factt n)
  (product-iter identity 1 inc n))

(define (approx-pi b)
  (product-iter (lambda (x) (/ (* x (+ x 2.0)) (square (+ x 1)))) 2
                (lambda (x) (+ x 2)) b))

(* 4 (approx-pi 10000))

;; abstact both function. recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; ch01.rkt﻿> (accumulate + 0 identity 1 inc 10)
;; 55
;; ch01.rkt﻿> (accumulate-iter * 1 identity 1 inc 10)
;; 3628800

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

(define (sum-of-squares-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-gcd n)
  (filtered-accumulate (lambda (x) (= 1 (gcd n x)))
                       * 1 identity 1 inc n))

;; finally let-constructs. I am using [] bracket as
;; I am coming from racket background
(define (f-let x y)
  (let ([a (+ 1 (* x y))]
        [b (- 1 y)])
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; you can see () and [] are interchangeable
[define [hello x y]
  [+ x y]]

(define (f-f x)
  (let [(x 3)
        (y (+ x 2))]
    (* x y)))

(define (fff ggg)
  (ggg 2))

;; ch01.rkt﻿> (fff (lambda (x) (fff x)))
;; application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 2
;;   arguments...:
;;    2

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

;; it could have written using let* in much better way
(define (search f neg-point pos-point)
  (let ([mid-point (/ (+ neg-point pos-point) 2.0)])
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ([value-at-mid (f mid-point)])
          (cond
             [(positive? value-at-mid) (search f neg-point mid-point)]
             [(negative? value-at-mid) (search f mid-point pos-point)]
             [else mid-point])))))

(define (half-interval-method f a b)
  (let ([a-value (f a)]
        [b-value (f b)])
    (cond
      [(and (negative? a-value) (positive? b-value)) (search f a b)]
      [(and (negative? b-value) (positive? a-value)) (search f b a)]
      [else "not found"]))) ;; some thing wrong with error

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (display guess)
    (display " and value of function ")
    (display (f guess))
    (newline)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixpoint-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; phi ^ 2 = phi + 1
;; phi = 1 + 1 / phi

(define golden-ration
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

;;1.36
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
