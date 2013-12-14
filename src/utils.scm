;; utils.scm
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
(define (complex-number? z)
  (and (number? z)
       (not (real? z))))

(define (real-number? z)
  (and (real? z)
       (not (complex? z))))

(define (zero? z)
  ((lambda (r)
     (= r
        (if (exact? r) 0 0.0)))
   (magnitude z)))

(define (negative? x)
  (and (real? x) (< x 0.0)))

(define (positive? x)
  (and (real? x) (> x 0.0)))

(define (even? n) 
  (zero? (remainder n 2)))

(define (inc x)
  (+ 1 x))

(define (square x)
  (* x x))

(define (sgn x)
  (if (positive? x)
      1
      (if (negative? x)
          -1
          0)))

(define (abs x)
  (cond
   ((infinite? x) :+inf.0)
   ((complex-number? x) (magnitude x))
   ((negative? x) (- x))
   (else x)))

(define *machine-epsilon*
  (let loop ((e 1.0))
    (if (= 1.0 (+ e 1.0))
        (* 2 e)
        (loop (/ e 2)))))

(define *sqrt-machine-epsilon* (sqrt *machine-epsilon*))

(define (float= a b)
  (<
     (/ (abs (- a b))
        (/ (+ 1.0 
              (min (abs a) 
                   (abs b))) 
           2))
   *machine-epsilon*))

;;; helpers for infinities
(define +inf.0 ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
                #b0111111111110000000000000000000000000000000000000000000000000000))
(define -inf.0 ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
                #b1111111111110000000000000000000000000000000000000000000000000000))

;; aliases to be consistent with how I'm defining constants...
(define :+inf.0 +inf.0)
(define :-inf.0 -inf.0)
(define :+inf.i (* +i :+inf.0))
(define :-inf.i (* -i :+inf.0))

(define (real-finite? x)
  (and (flo:flonum? x)
       (flo:finite? x)))

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

(define (newtons-method f deriv guess n)
  ((lambda (iterate)
     (if (or (> n 13) 
             (float= (- guess iterate) 
                     guess))
         (- guess iterate)
         (newtons-method f deriv (- guess iterate) (inc n))))
   (/ (f guess) 
      (deriv guess))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sigma term a b)
  (sum term a inc b))

(define (quotient a b)
  (truncate (/ a b)))

(define (remainder a b)
  (- a 
     (* b (quotient a b))))

(define void (if #f #f))

(define (identity x)
  x)

;; logging info
(define (log/info msg)
  (write-string msg)
  (newline)
  (flush-output))

(define-syntax assert
  (syntax-rules ()
    ((assert ?x)
     (if (not ?x) (error "Assertion failed" '?x)))))
