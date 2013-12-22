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
     (if (exact? r)
         (= r 0)
         (float= r 0)))
   (abs z)))

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
  (<= (abs (- a b))
      (* 1/2
         (min (abs a)
              (abs b))
         *machine-epsilon*)))

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
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ (term x) result))))
  (iter a 0))

(define (sigma term a b)
  (sum term a inc b))

;; number theoretic functions
(define (quotient a b)
  (truncate (/ a b)))

(define (remainder a b)
  (- a 
     (* b (quotient a b))))

(define (divisible? a b)
  (zero? (remainder a b)))

;; stream utils
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define naturals (integers-starting-from 1))

(define (range n) (stream-head (integers-starting-from 0) n))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (invert-stream s)
  (stream-map (lambda (x) (/ 1 x)) s))

(define (negate-stream s)
  (stream-map - s))

(define inverted-naturals (invert-stream naturals))

;; series utils
(define (integrate-series series)
  (mul-streams series inverted-naturals))

(define factorials
  (cons-stream 1
               (cons-stream 1
                            (mul-streams (stream-cdr factorials)
                                         (stream-cdr naturals)))))

(define add-series
  add-streams)

(define (scale-series factor stream)
  (stream-map (lambda (x) (* x factor)) stream))

(define negate-series negate-stream)

(define sine-series
  (cons-stream
   0
   (integrate-series
    (cons-stream 1 (integrate-series (negate-stream sine-series))))))

(define cosine-series
  (cons-stream 1 (integrate-series (negate-stream sine-series))))

;; (a0 + A)(b0 + B) = (+ (* a0 b0)
;;                       (* A (+ b0 B)))
(define (mul-series a b)
  (cons-stream (* (stream-car a) (stream-car b))
               (add-series
                 (scale-series (stream-car a) (stream-cdr b))
                 (mul-series (stream-cdr a) b))))

;; (define foo-stream (invert-unit-series bernouli-summand-stream))
;; (define (foo n) (stream-ref foo-stream n))
(define (invert-unit-series s)
  (cons-stream 1
               (negate-series
                (mul-series (stream-cdr s)
                            (invert-unit-series s)))))

;; sums as streams
(define (partial-sums s)
  (cons-stream (car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

(define harmonics
  (partial-sums inverted-naturals))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


;; miscellaneous
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
