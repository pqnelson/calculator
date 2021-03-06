;; hyperbolic-trig.scm
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
(import "src/logarithm.scm")
(import "src/exponential.scm")

(define (sinh-cf-a x k)
  (cond
   ((> k 2) (* -1 
               (* 2 
                  (- k 2))
               (inc (* 2 
                       (- k 2)))
               (square x)))
   ((= k 2) (- (square x)))
    ((= k 1) x)
    (else 0)))

(define (sinh-cf-b x k)
  (cond
   ((> k 1)
    (+ (* 2 
          (- k 1)
          (inc (* 2 
                  (- k 1))))
       (square x)))
   ((= k 1) 1)
   (else 0)))

(define (raw-sinh-cf x n)
  (generalized-cont-frac
   (lambda (j)
     (sinh-cf-a x j))
   (lambda (j)
     (sinh-cf-b x j))
   n))

(define (sinh-cf x n)
  (if (> (abs x) 4)
      ((lambda (s)
         (* 4 s 
            (cosh (/ x 4))
            (+ 1 
               (* 2 
                  (square s)))))
       (sinh-cf (/ x 4) (inc n)))
      (raw-sinh-cf x n)))

(define (sinh x)
  ((lambda (e)
     (/ (- 1 
           (square e))
        (* 2 
           e)))
   (* (sgn x)
      (exp (* (sgn x) x)))))

(define (csch x)
  (/ 1 (sinh x)))

;; from https://archive.org/details/ContinuedFractionExpansionForFunctionsCosxSecxChxSchx
(define (raw-cosh-cf x n)
  (define (F z k)
    (generalized-cont-frac
     (lambda (j)
       (/ (square z) 
          (* 4 
             (- (* 4 (square j)) 
                1))))
     (lambda (j)
       (if (positive? j) 1 0))
     k))
  (+ 1
     (/ (/ x 2)
        (- (square 
            (+ 1
               (F x n)))
           (square (/ x 2)))))) 

(define (cosh-cf x n)
  (if (> (abs x) 1)
      ((lambda (s c)
         (+ (square (square s))
            (* 6 
               (square (* s c)))
            (square (square c))))
       (sinh (/ x 4))
       (cosh-cf (/ x 4) (inc n)))
      (raw-cosh-cf x n)))

(define (naive-cosh x)
  (/ (+ (exp x)
        (exp (- x)))
     2))

(define (cosh x)
  ((lambda (e)
     (/ (+ 1 (square e))
        (* 2 e)))
   (exp (if (positive? x) (- x) x))))

(define (sech x)
  (/ 1 (cosh x)))

(define (lambert-tanh-cf x n)
  (generalized-cont-frac
   (lambda (j)
     (if (> j 1)
         (square x)
         x))
   (lambda (j)
     (if (> j 0)
         (- (* 2 j) 1)
         0))
   n))

(log/info "\nDefining :golden-ratio")
(define :golden-ratio (rationalize->exact
                       (/ (+ 1 (sqrt 5)) 2)
                       (expt 10 -100)))
(log/info "Defining :ln-phi")
(define :ln-phi (rationalize->exact
                 (+ :ln-3
                    (- :ln-2)
                    (euler-ln-cf (/ :golden-ratio 3/2) 40))
                 (expt 10 -100)))
(define (phi-cf k)
  (cont-frac (lambda (i) 1) k))

(define (lambert-tanh x)
  (cond
   ((> (abs x) 4)
    ((lambda (t)
       (/ (* 4 t (+ 1 (square t)))
          (+ 1
             (* (square t)
                (+ (square t) 6)))))
     (lambert-tanh (/ x 4))))
   ((zero? x) 0)
   ((= :ln-phi x) (/ (sqrt 5) 5))
   (else (lambert-tanh-cf x 30))))

(define (naive-tanh x)
  (/ (sinh x)
     (cosh x)))

(define (tanh x)
  (lambert-tanh x))

(define (coth x)
  (/ 1 
     (tanh x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inverse Hyperbolic Trig Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log/info "Defining inverse hyperbolic trig functions...")
(define (arccosh x)
  (ln
   (+ x
      (sqrt (inc (square x))))))

(define (arcsinh x)
  (ln 
   (+ x
      (sqrt (- (square x) 1)))))

(define (arctanh x)
  (cond
   ((infinite? x) (* -i :pi/2))
   ((= x 1) :+inf.0)
   (else (/ (- (ln (+ 1 x))
               (ln (- 1 x)))
            2))))

