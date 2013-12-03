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
(define (zero? x)
  (= 0 x))

(define (even? n) 
  (zero? (remainder n 2)))

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

(define (newtons-method f deriv guess n)
  ((lambda (iterate)
     (if (or (> n 53) (float= (* 1.0 (- guess iterate)) (* 1.0 guess)))
       (- guess iterate)
       (newtons-method f deriv (- guess iterate) (inc n))))
   (/ (f guess) (deriv guess))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))
