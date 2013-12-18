;; quadrature.scm
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

(define (composite-rule method f a b n)
  (let ((h (/ (- b a) n)))
    (define (iter result start fin)
      (let ((term (+ result
                     (method f start fin))))
        (if (= fin b)
            term
            (iter term fin (+ fin h)))))
    (iter 0 a (+ a h))))
          
(define (trapezoid-rule f a b)
  (* (/ (- b a) 2)
     (+ (f a)
        (f b))))

(define (simpsons-rule f a b)
  (let ((h (/ (- b a) 2)))
    (* (/ h
          3)
       (+ (f a)
          (* 4 (f (+ a h)))
          (f b)))))

(define (booles-rule f a b)
  (let ((h (/ (- b a) 4)))
    (* h 
       2/45
       (+ (* 7 (f a))
          (* 32 (f (+ a h)))
          (* 12 (f (+ a (* 2 h))))
          (* 32 (f (+ a (* 3 h))))
          (* 7 (f b))))))

(define (composite-simpsons f a b n)
  (cond 
   ((<= n 0) 0)
   ((odd? n) (composite-simpsons f a b (inc n)))
   (else 
    (let ((h (/ (- b a) n)))
      (* (/ h 3)
         (sigma (lambda (k)
                  (cond 
                   ((= k 0) (f a))
                   ((= k n) (f b))
                   (else (* (if (even? k) 2 4) 
                            (f (+ a (* h k)))))))
                0 n))))))

(define (composite-simpsons-three-eights f a b n)
  (cond 
   ((<= n 0) 0)
   ((not (zero? (remainder n 3))) 
    (composite-simpsons-three-eights f a b (+ n (- 3 (remainder n 3)))))
   (else 
    (let ((h (/ (- b a) n)))
      (* 3/8  
         h
         (sigma (lambda (k)
                  (cond 
                   ((= k 0) (f a))
                   ((= k n) (f b))
                   (else (* (if (zero? (remainder k 3)) 2 3) 
                            (f (+ a (* h k)))))))
                0 n))))))

(define (adaptive-trapezoid f a b tol)
  (define (iter f a b tol)
    (let* ((midpoint (/ (+ a b) 2))
           (I-coarse (trapezoid-rule f a b))
           (I-fine (+ (trapezoid-rule f a midpoint)
                      (trapezoid-rule f midpoint b))))
      (if (or (< (abs (- I-coarse I-fine))
                 (* 3 (- b a) tol))
              (< tol
                 *machine-epsilon*))
          I-fine
          (+ (iter f a midpoint (/ tol 2))
             (iter f midpoint b (/ tol 2))))))
  (iter f a b tol))

(define (adaptive-simpsons f a b tol)
  (define (iter f a b tol)
    (let* ((midpoint (/ (+ a b) 2))
           (I-coarse (simpsons-rule f a b))
           (I-fine (+ (simpsons-rule f a midpoint)
                      (simpsons-rule f midpoint b))))
      (if (and (<= (abs (- I-coarse I-fine))
                   (* 15 tol))
               (> tol *machine-epsilon*))
          I-fine
          (+ (iter f a midpoint (/ tol 2))
             (iter f midpoint b (/ tol 2))))))
  (iter f a b tol))

(define (definite-integral f a b)
  (cond 
   ((float= a b) 0)
   ((< b a) (- (definite-integral f b a)))
   (else (adaptive-simpsons f a b *sqrt-machine-epsilon*))))


