;; calculator.scm
;; 
;; The MIT License (MIT)
;; 
;; Copyright (c) 2013 Alexander Nelson
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (zero? x)
  (= 0 x))

(define (inc x)
  (+ 1 x))

(define (float= a b)
  (<= 
    (/ (abs (- a b))
       (/ (+ 1.0 (min (abs a) (abs b))) 2))
    1e-16))

(define (complex-number? z)
  (and (number? z)
       (not (real? z))))

(define (real-infinite? x)
  (and (flo:flonum? x)
       (not (flo:finite? x))))

(define (infinite? z)
  (if (complex-number? z)
    (or (real-infinite? (real-part z))
        (real-infinite? (imag-part z)))
    (real-infinite? z)))

(define (finite? z)
  (not (infinite? z)))

(define :+inf.0 (/ 1.0 0.0))
(define :-inf.0 (/ -1.0 0.0))
(define :+inf.i (/ +i 0.0))
(define :-inf.i (/ -i 0.0))

(define (even? n) 
  (= (remainder n 2) 0))
  
;; wikipedia's notation for a generalized continued fraction
(define (generalized-cont-frac a b k)
  (define (recur i) 
    (if (> i k) 
      0 
      (/ (a i) 
         (+ (b i) (recur (inc i)))))) 
  (recur 1)) 

;; a continued fraction is a generalized continued fraction with a=1
(define (cont-frac b k)
  (generalized-cont-frac (lambda (i) 1) b k))

(define (euler-cont-frac-term k)
  (if (= k 1)
    1
    (* 2 (- (* 2 k) 1))))

(define (euler-e k)
  (inc (* 2 (cont-frac euler-cont-frac-term k))))

;; (euler-e 7) => 2.7182818284590455

(define :e (euler-e 20))

(define (phi k)
  (inc (cont-frac (lambda (i) 1) k)))

;; (phi 36) =>  1.618033988749894
(define :golden-ratio (phi 36))

(define (tan-cf x k)
  (generalized-cont-frac
    (lambda (i) (if (= 1 i) x (- (square x))))
    (lambda (i) (- (* 2 i) 1))
    k))


(define (euler-arctan z k)
  (generalized-cont-frac
    (lambda (j)
      (if (= j 1)
        z
        (square (* z 
                   (- (* 2 j) 3)))))
    (lambda (j)
      (if (= j 1)
        1
        (- (- (* 2 j) 1)
           (* (- (* 2 j) 3) 
              (square z)))))
    k))

(define (gauss-arctan z k)
  (generalized-cont-frac
    (lambda (j)
      (if (= j 1)
        z
        (square (* z (- j 1)))))
    (lambda (j)
      (- (* 2 j) 1))
    k))

;; see, e.g., http://en.wikipedia.org/wiki/Computing_%CF%80#Other_classical_formulae
(define :pi (+ (* 20 (euler-arctan (/ 1 7) 45))
               (* 8 (euler-arctan (/ 3 79) 45))))
(define :2pi (* 2 :pi))
(define :pi/4 (+ (* 5 (euler-arctan (/ 1 7) 45))
                 (* 2 (euler-arctan (/ 3 79) 45))))
(define :pi/2 (* :pi/4 2))

(define (real-arctan x)
  (cond
    ((infinite? x) (* (if (positive? x) 1 -1) :pi/2))
    ((> (abs x) 10) (- :pi/2 (real-arctan (/ 1 x))))
    ((> (abs x) 0.9) (* 2 
                       (euler-arctan
                         (/ x (inc (sqrt (inc (square x)))))
                         25)))
    (else (euler-arctan x 25))))

(define (arctan x)
  (real-arctan x))

(define (arccot x) 
  (arctan (/ 1 x)))

(define (arcsin x)
  (* 2 
     (arctan (/ x 
               (inc (sqrt (- 1 (square x))))))))

(define (arccsc x) 
  (arcsin (/ 1 x)))

(define (arccos x)
  (- :pi/2
     (arcsin x)))

(define (arcsec x) 
  (arccos (/ 1 x)))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trigonometric functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need 8 terms to get machine precision after range-reduction
(define (sine-taylor-series x)
  (define (iter k result)
    (if (= 0 k)
      (* x result)
      (iter (- k 1) (+ 1 (* (/ (- (square x)) 
                               (* (* 2 k) (inc (* 2 k)))) 
                            result)))))
  (iter 20 1))

(define (sine-range-reduce x)
  (truncate 
    (+ (/ x :pi) 
       (/ 1 2))))
  
(define (real-sin x)
  ((lambda (n)
     (* (if (even? n) 1 -1)
        (sine-taylor-series (- x (* :pi n)))))
    (sine-range-reduce x)))

(define (real-cos x)
  (real-sin (- :pi/2 x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fast-expt b n)
  (cond
    ((infinite? n) (if (negative? n) 0 :+inf.0))
    ((= n 0) 1)
    ((< n 0) (fast-expt (/ 1 b) (- n)))
    (else (* (if (even? n) 1 b) 
             (fast-expt (* b b) (quotient n 2))))))

(define (euler-exp-a z k)
  (if (= 1 k)
    (* 2 z)
    (square z)))

(define (euler-exp-b z k)
  (+ 2
    (if (= 1 k)
      (- z)
      (* (- k 1) 4))))

(define (exp-cf z k)
  (inc
    (generalized-cont-frac
      (lambda (j) (euler-exp-a z j))
      (lambda (j) (euler-exp-b z j))
      k)))

(define (real-exp z)
  (* (fast-expt :e (truncate z))
     (exp-cf (- z (truncate z)) 25)))

(define (exp z)
  (if (complex-number? z)
    (* (real-exp (real-part z))
       (+ (real-cos (imag-part z))
          (* +i (real-sin (imag-part z)))))
    (real-exp z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hyperbolic trigonometric functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sinh x)
  (/ (- (exp x)
        (exp (- x)))
     2))

(define (csch x)
  (/ 1 (sinh x)))

(define (cosh x)
  (/ (+ (exp x)
        (exp (- x)))
     2))

(define (sech x)
  (/ (cosh x)))

(define (tanh x)
  (/ (sinh x)
     (cosh x)))

(define (coth x)
  (/ (cosh x)
     (sinh x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trigonometric Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sin z)
  (if (complex-number? z)
    (+ (* (real-sin (real-part z))
          (cosh (imag-part z)))
       (* +i
          (real-cos (real-part z))
          (sinh (imag-part z))))
    (real-sin z)))

(define (cos z)
  (if (complex-number? z)
    (- (* (real-cos (real-part z))
          (cosh (imag-part z)))
       (* +i
          (real-sin (real-part z))
          (sinh (imag-part z))))
    (real-cos z)))

(define (tan z)
  (/ (sin z)
     (cos z)))

(define (csc x)
  (/ 1 (sin x)))

(define (sec x)
  (/ 1 (cos x)))

(define (cot x)
  (/ 1 (tan x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logarithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (newtons-method f deriv guess n)
  ((lambda (iterate)
     (if (or (> n 53) (float= (* 1.0 (- guess iterate)) (* 1.0 guess)))
       (- guess iterate)
       (newtons-method f deriv (- guess iterate) (inc n))))
   (/ (f guess) (deriv guess))))
   
(define (ln-iterate c)
  (newtons-method 
    (lambda (x) (- (exp x) c)) 
    exp
    c
    0))

(define :ln-2 (ln-iterate 2.0))
(define :ln-10 (ln-iterate 10.0))

(define (real-ln c)
  (cond 
    ((< c 0) (+ (* +i :pi) (real-ln (- c))))
    ((infinite? c) :+inf.0)
    ((= c 0) :-inf.0)
    ((= c 1) 0)
    ((> c 10) (+ (ln (/ c 10)) :ln-10))
    (else (ln-iterate c))))

(define (exact-ln z)
  (if (complex-number? z)
    (+ (real-ln (magnitude z))
       (* +i (angle z)))
    (real-ln z)))
    
(define (ln z)
  (* 1.0 (exact-ln (* 1.0 z))))

;; base-2 logarithm
(define (lg x)
  (/ (ln x) :ln-2))

;; base-10 logarithm
(define (log x)
  (/ (ln x) :ln-10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Power function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pow b x)
  (* (fast-expt b (truncate x))
     (exp (* (- x (truncate x)) (ln b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Square root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (real-sqrt x)
  (cond
   ((negative? x) (* +i (real-sqrt (abs x))))
   ((> (abs x) 100) (* 10 (real-sqrt (/ x 100))))
   (else (newtons-method
           (lambda (t) (- (square t) x))
           (lambda (t) (* 2 t))
           (/ (inc x) 2)
           0))))

(define (sqrt x)
  (if (complex-number? x)
    (* (real-sqrt (magnitude x))
      (exp (* +i (angle x) (/ 1 2))))
    (real-sqrt x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inverse Hyperbolic Trig Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (arctan z)
  (if (complex-number? z)
    (* (/ +i 2)
       (ln (/ (- 1 (* +i z))
              (+ 1 (* +i z)))))
    (real-arctan z)))             

(define (arccosh x)
  (ln
    (+ x
       (sqrt (inc (square x))))))

(define (arcsinh x)
  (ln 
    (+ x
       (sqrt (- (square x) 1)))))

(define (arctanh x)
  (/ 
    (ln (/ (+ 1 x)
           (- 1 x)))
    2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; factorials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count) 
    product 
    (fact-iter (* counter product) (inc counter) max-count)))

(define (choose n k)
  (if (> k n)
    0
    (/ (factorial n) 
       (* (factorial k) 
          (factorial (- n k))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Stirling Numbers
;;;;;;;;;;;;;;;;;;;;;;;; 
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))
  
(define (stirling-s2 n k)
  (cond
    ((> k n) 0)
    ((zero? k) (if (zero? n) 1 0))
    ((= k 1) 1)
    ((= n k) 1)
    (else (sum (lambda (j)
                 (/ (* (if (even? (- k j)) 1 -1) 
                       (fast-expt j (- n 1)))
                    (* (factorial (- j 1))
                       (factorial (- k j)))))
                1
                inc
                k))))
          
