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

(load "src/mod-utils.scm")
(import "src/utils.scm")
(import "src/continued-fraction.scm")
(import "src/exponential.scm")
(import "src/logarithm.scm")
(import "src/trig.scm")
(import "src/hyperbolic-trig.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Square root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log/info "Loading the square root methods...")

(define (newtons-sqrt x guess k)
  (newtons-method
   (lambda (t) (- (square t) x))
   (lambda (t) (* 2 t))
   guess
   k))

(log/info "Defining :rt2 and friends...")
(define :rt2 (newtons-sqrt 2 19175002942688032928599/13558774610046711780701 0))
(define :rt3 (newtons-sqrt 3 1002978273411373057/579069776145402304 0))
(define :rt5 (newtons-sqrt 5 25377553679502347277411602/11349187026003431978487841 0))
(define :rt-pi (newtons-sqrt :pi 184238184425457900/103945264544593811 0))
(define :rt-e (newtons-sqrt :e 6803184337622361001/4126339884444745657 0))
(assert (float= (square :rt2) 2))

(define (real-sqrt x)
  (cond
   ((zero? x) 0)
   ((negative? x) (* +i (real-sqrt (abs x))))
   ((> x 100) (* 10 (real-sqrt (/ x 100))))
   (else (newtons-sqrt x (/ (inc x) 2) 0))))

(define (sqrt x)
  (if (infinite? x)
      (if (real? x)
          (if (negative? x)
              :+inf.i
              :+inf.0)
          (+ :+inf.0 :+inf.i))
      (if (complex-number? x)
          (* (real-sqrt (magnitude x))
             (exp (* +i (angle x) (/ 1 2))))
          (real-sqrt x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Power function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log/info "Power function...")
(define (pow b x)
  (* (fast-expt b (truncate x))
     (exp (* (- x 
                (truncate x)) 
             (ln b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; factorials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log/info "Factorials...")
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
