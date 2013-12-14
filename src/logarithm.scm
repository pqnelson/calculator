;; logarithm.scm
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

(import "src/utils.scm")
(import "src/continued-fraction.scm")
;; http://en.wikipedia.org/wiki/Logarithm#Power_series
(define (ln-series z)
  (* 2
     (/ (- z 1) 
        (+ z 1))
     ((lambda (u)
        (+ 1
         (* u
            (+ 1/3
               (* u
                  (+ 1/5
                     (* u
                        (+ 1/7
                           (* u 
                              (+ 1/9
                                 (/ u 11)))))))))))
      (square (/ (- z 1) (+ z 1))))))

(define (euler-ln-a z k)
  (if (> k 1) 
      (- (square (* (- k 1) z)))
      (* 2 z)))

(define (euler-ln-b j)
  (if (> j 0)
      (- (* 2 j) 1)
      0))

(define (raw-euler-ln-cf z n)
  (generalized-cont-frac
   (lambda (k)
     (euler-ln-a z k))
   euler-ln-b
   n))

(define (euler-ln-cf z n)
  (raw-euler-ln-cf 
   (/ (- z 1)
      (+ z 1))
   n))

(define (ln-iterate c)
  ((lambda (y)
     (+ y
        (ln-series (/ c (exp y)))))
   (ln-series c)))

(log/info "\nDefining :ln-2...")
(define :ln-2 (euler-ln-cf 2 40))
(define :ln-3 (+ :ln-2 (euler-ln-cf 3/2 40)))
(log/info "Defining :ln-10...")
(define :ln-10 (+ (* 3 :ln-2) (euler-ln-cf 5/4 40)))

(define (real-ln c)
  (cond 
   ((negative? c) (+ +i :pi (real-ln (- c))))
   ((infinite? c) :+inf.0)
   ((zero? c) :-inf.0)
   ((= c 1) 0)
   ((> c 1000) (+ (* 3 :ln-10)
                  (approx-real-ln (/ c 1000))))
   ((> c 10) (+ :ln-10
                (approx-real-ln (/ c 10))))
   ((> c :e) (+ 1
                (approx-real-ln (/ c :e))))
   (else (rationalize->exact (euler-ln-cf c 25) (expt 10 -50)))))

(define (ln z)
  (cond
   ((= 1 z) 0)
   ((= :e z) 1)
   ((complex-number? z) (+ (real-ln (magnitude z))
                           (* +i (angle z))))
   (else (real-ln z))))

;; base-2 logarithm
(define (lg x)
  (/ (ln x) :ln-2))

;; base-10 logarithm
(define (log x)
  (/ (ln x) :ln-10))
