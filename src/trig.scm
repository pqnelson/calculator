;; trig.scm
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

(import "src/exponential.scm")
(import "src/logarithm.scm")

(define (tan-cf x k)
  (generalized-cont-frac
   (lambda (i) (if (= 1 i) x (- (square x))))
   (lambda (i) (if (> i 0) (- (* 2 i) 1) 0))
   k))

(define (euler-arctan z k)
  (generalized-cont-frac
   (lambda (j)
     (if (> j 1) 
         (square (* z 
                    (- (* 2 j) 3)))
         z))
   (lambda (j)
     (cond
      ((> j 1)
       (- (- (* 2 j) 1)
          (* (- (* 2 j) 3) 
             (square z))))
      ((= j 1) 1)
      (else 0)))
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
(log/info "Defining :pi and friends...")
(define :pi (rationalize->exact
             (+ (* 20 (euler-arctan (/ 1 7) 45))
                (* 8 (euler-arctan (/ 3 79) 45)))
             (expt 10 -78))) ;; it's only good to 78 digits anyways...
(define :2pi (* 2 :pi))
(define :pi/4 (rationalize->exact
               (+ (* 5 (euler-arctan (/ 1 7) 45))
                  (* 2 (euler-arctan (/ 3 79) 45)))
               (expt 10 -78))) ;; again, good up to 78 digits
(define :pi/2 (* :pi/4 2))

(log/info "Defining :ln-pi...")
(define :ln-pi (rationalize->exact 
                  (+ (euler-ln-cf (/ :pi 3) 50) 
                     :ln-3)
                  (expt 10 -78)))
(define :ln-pi/2 (- :ln-pi :ln-2))
(define :ln-pi/4 (- :ln-pi/2 :ln-2))
(define :ln-2pi (+ :ln-2
                   :ln-pi))

;; inverse trig functions
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
  (cond 
   ((finite? x) (/ (-
                    (ln (inc (* +i x)))
                    (ln (inc (* -i x))))
                   (* 2 +i)))
   ((positive? x) :pi/2)
   (else (- :pi/2))))
   
(define (arccot x) 
  (if (zero? x) 
      :pi/2
      (arctan (/ 1 x))))

(define (arcsin z)
  (* -i
     (ln (+ (* +i z)
            (sqrt (- 1 (square z)))))))

(define (arccsc x) 
  (cond
   ((>= (abs x) 1) (error "INVALID DOMAIN arcsc --- " x))
   ((zero? x) :+inf.i)
   (else (arcsin (/ 1 x)))))

(define (arccos x)
  (- :pi/2
     (arcsin x)))

(define (arcsec x) 
  (cond
   ((zero? x) :+inf.i)
   ((finite? x) (arccos (/ 1 x)))
   ((complex-number? x) :pi/2)
   (else :pi/2)))

;; honest trig functions
(define (sine-taylor-series x)
  (define (iter k result)
    (if (= 0 k)
        (* x result)
        (iter (- k 1) 
              (+ 1 
                 (* (/ (- (square x)) 
                       (* (* 2 k) 
                          (inc (* 2 k)))) 
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
  (lambda (c)
    (if (zero? c)
        (* (if (real? z) 1 (sin z)) 
           :+inf.0)
        (/ (sin z)
           c))
    (cos z)))

(define (csc x)
  (let ((s (sin x)))
    (if (zero? s)
        :+inf.0
        (/ 1 s))))

(define (sec x)
  (let ((c (cos x)))
    (if (zero? c)
        :+inf.0
        (/ 1 c))))

(define (cot z)
  ((lambda (s)
     (if (zero? s)
         (* (if (real? z) 1 (cos z)) 
            :+inf.0)
         (/ (cos z) s)))
   (sin z)))
