#lang racket

;; See https://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps00/dh.pdf

;; PROBLEM 1: Modular Arithmetic
;; =============================

(define (+mod a b n)
  (modulo (+ a b) n))

(define (-mod a b n)
  (modulo (- a b) n))

(define (*mod a b n)
  (modulo (* a b) n))

(define modular
  (lambda (modulus op)
    (lambda (a1 a2)
      (modulo (op a1 a2) modulus))))

(define +m12 (modular 12 +))
(define -m12 (modular 12 -))
(define *m12 (modular 12 *))

(+mod 7 5 8) ; -> 4
(+mod 10 10 3) ; -> 2
(-mod 5 12 2) ; -> 1
(*mod 6 6 9) ; -> 0
(+mod 99 99 100) ; -> ?
(*mod 50 -3 100) ; -> ?

(-m12 (*m12 (+m12 5 8) 3) 7) ; -> 8

((modular 17 +) 13 11) ; -> 7
((modular 17 -) 13 11) ; -> 2
((modular 17 *) 13 11) ; -> 7

;; PROBLEM 2: Raising a number to a power
;; ======================================

(define (slow-exptmod n)
  (let ((*mod (modular n *)))
    (define (em a b)
      (if (= b 0)
        1
        (*mod a (em a (- b 1)))))
    em))

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
      (mod* x x))
    (define (em base exponent)
      (cond
        ((zero? exponent) 1)
        ((even? exponent) (square (em base (/ exponent 2))))
        (else (mod* base (em base (sub1 exponent))))))
    em))

((exptmod 10) 2 0) ; -> 1
((exptmod 10) 2 3) ; -> 8
((exptmod 10) 3 4) ; -> 1
((exptmod 100) 2 15) ; -> 68
((exptmod 100) -5 3) ; -> 75

;; PROBLEM 3: Large random numbers
;; ===============================

(define (random-k-digit-number k)
  (if (= k 1)
    (random 10)
    (+ (* 10 (random-k-digit-number (sub1 k))) (random 10))))

(random-k-digit-number 1)   ; -> ?       (1 digit)
(random-k-digit-number 3)   ; -> ?       (1-3 digits)
(random-k-digit-number 3)   ; -> ?       (is it different?)
(random-k-digit-number 50)  ; -> ?       (1-50 digits)

(define (count-digits n)
  (if (zero? n)
    0
    (add1 (count-digits (quotient n 10)))))

(count-digits 3)         ; -> 1
(count-digits 2007)      ; -> 4
(count-digits 123456789) ; -> 9

(define (big-random n)
  (let*
    ((c (count-digits n))
     (k (random-k-digit-number c)))
    (if (< k n)
      k
      (big-random n))))

(big-random 100)          ; -> ?? (1-2 digit-number)
(big-random 100)          ; -> ?? (is it different)
(big-random 1)            ; -> 0
(big-random 1)            ; -> 0 (should always be zero)
(big-random (expt 10 40)) ; -> ????... (roughly 40-digit number)

;; Problem 4: Prime numbers
;; ========================

(define (slow-prime? n)
  (define (test-factors n k)
    (cond
      ((>= k n) #t)
      ((= (remainder n k) 0) #f)
      (else (test-factors n (add1 k)))))
  (if (< n 2)
    #f
    (test-factors n 2)))

; Fermats little theorem:
;  If p is prime,  a^p = a (mod p) for all a
(define a (big-random (expt 10 40)))
(define p 37)
(= ((exptmod p) a p) (modulo a p))

(define prime-test-iterations 20)

(define (prime? p)
  (if (< p 2)
    #f
    (let ((ex (exptmod p)))
      (define (test-prime n)
        (if (zero? n)
            #t
            (let ((a (big-random (sub1 p))))
              (if (= (ex a p) (modulo a p))
                  (test-prime (sub1 n))
                  #f))))      
      (test-prime prime-test-iterations))))

(prime? 2) ; -> #t
(prime? 4) ; -> #f
(prime? 1) ; -> #f
(prime? 0) ; -> #f
(prime? 200) ; -> #f
(prime? 199) ; -> #t

;; PROBLEM 5: Random primes
;; ========================

(define (random-k-digit-prime k)
  (let ((n (random-k-digit-number k)))
    (if (prime? n)
      n
      (random-k-digit-prime k))))

(random-k-digit-prime 1)
(random-k-digit-prime 2)
(random-k-digit-prime 10)
(random-k-digit-prime 100)
(count-digits (random-k-digit-prime 100))
(count-digits (random-k-digit-prime 100))

;; PROBLEM 6: Multiplicative Inverses
;; ==================================

(define (ax+by=1 a b)
  (let ((q (quotient a b))
        (r (remainder a b)))
    (if (= r 1)
      (list 1 (- q))
      (let*
        ((deriv (ax+by=1 b r))
         (x2 (first deriv))
         (y2 (second deriv)))
        (list y2 (- x2 (* q y2)))))))

(ax+by=1 17 13) ; -> (-3 4)   17*-3 + 13*4 = 1
(ax+by=1 7 3)   ; -> (1 -2)    7*1  + 3*-2 = 1
(ax+by=1 10 27) ; -> (-8 3)   10*-8 + 3*27 = 1

(define (inversemod n)
  (lambda (e)
    (if (= (gcd e n) 1)
      (modulo (first (ax+by=1 e n)) n)
      (error "e and n are not co-prime"))))

((inversemod 11) 5) ; -> 9       5*9 = 45 = 1 (mod 11)
((inversemod 11) 9) ; -> 5
((inversemod 11) 7) ; -> 8       7*8 = 56 = 1 (mod 11)
((inversemod 12) 5) ; -> 5       5*5 = 25 = 1 (mod 12)
;((inversemod 12) 8) ; -> error   gcd(8,12)=4, so no inverse exists

(let*
  ((rnd-prime (random-k-digit-prime 8))
   (im ((inversemod 101) rnd-prime)))
  (*mod im rnd-prime 101)) ; -> 1

;; PROBLEM 7: The ElGamal Public-Key Cryptosystem
;; ==============================================
       