;; exponential.scm
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
(import "src/continued-fraction.scm")

(define (euler-cont-frac-term k)
  (cond
   ((and (> k 1) (odd? k))
    (- (* 2 k) 1))
   ((and (> k 1) (even? k))
    (* 4
       (- (* 2 k)
          1)))
   ((= k 1) 1/2)
   ((= k 0) 1)
   (else 0)))

(define (euler-e k)
  (generalized-cont-frac (lambda (j) 1) euler-cont-frac-term k))

(log/info "\nDefining :e")
(define :e (euler-e 30)) ;; good to 100 digits

(log/info "Defining exponentiation...")
(define (fast-expt b n)
  (define (iter b n)
    (cond 
     ((= n 0) 1)
     ((= n 1) b)
     (else (* (if (even? n) 1 b) 
              (iter (* b b) (quotient n 2))))))
  (cond
   ((infinite? n) (if (negative? n) 0 :+inf.0))
   ((= b 1) 1)
   ((= b 0) 0)
   ((< b 0) (* (fast-expt (abs b) n)
               (exp (* +i :pi n))))
   ((< n 0) (iter (/ 1 b) (abs n)))
   (else (iter b n))))

(define (euler-exp-a z k)
  (if (= 1 k)
      (* 2 z)
      (square z)))

(define (euler-exp-b z k)
  (+ 2
     (if (= 1 k)
         (- z)
         (* (- k 1) 4))))

(define (faster-exp-num z z-squared k)
  (if (> k 1) 
      z-squared
      (* 2 z)))

(define (faster-exp-den z k)
  (cond
   ((> k 1) (- (* 4 k) 2))
   ((= k 1) (- 2 z))
   (else 1)))

(define (exp-cf z k)
  (generalized-cont-frac
   (lambda (j) (faster-exp-num z (square z) j))
   (lambda (j) (faster-exp-den z j))
   k))

(log/info "Defining :sqrt-e...")
(define :sqrt-e (exp-cf 1/2 30)) ;; good to 100 digits

;; good to ~30 digits
(define (real-exp z)
  (cond
   ((infinite? z) (if (negative? z) 0 z))
   ((zero? z) 1)
   ((= z 1) :e)
   ((= z -1) (/ 1 :e))
   ((>= (abs z) :ln-2) (* (fast-expt 2 (quotient z :ln-2))
                          (real-exp (remainder z :ln-2))))
   ((>= :ln-2 (abs z) 1/2)
    (* (real-exp (- z (* 1/2 (sgn z))))
       (if (negative? z)
           (/ 1 :sqrt-e)
           :sqrt-e)))
   ((> 1/2 (abs z))
    (* (exp-cf (- z (* 1/2 (sgn z))) 10)
       (if (negative? z)
           (/ 1 :sqrt-e)
           :sqrt-e)))
   (else (exp-cf z 10))))

(define (exp z)
  (if (complex-number? z)
      (* (real-exp (real-part z))
         (+ (real-cos (imag-part z))
            (* +i (real-sin (imag-part z)))))
      (real-exp z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Power function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log/info "Power function...")
(define (pow b x)
  (cond
   ((> (abs x) 1) 
    (* (fast-expt b (truncate x))
         (exp (* (- x 
                    (truncate x)) 
                 (ln b)))))
   ((float= (abs x) 1/2)
    (if (positive? x)
        (sqrt b)
        (/ 1 (sqrt b))))
   (else
    (exp (* x (ln b))))))

