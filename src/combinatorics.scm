;; combinatorics.scm
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; factorials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log/info "Factorials...")
;; (define (factorial n)
;;   (fact-iter 1 1 n))

;; (define (fact-iter product counter max-count)
;;   (if (> counter max-count) 
;;       product 
;;       (fact-iter (* counter product) (inc counter) max-count)))

(define (factorial n)
  (stream-ref factorials n))

;; see arXiv:1105.3689 for review of general properties
(define (choose n k)
  ;; use the identity {n\choose k} = (n/k) {n-1\choose k-1}
  (define (iter n k result)
    (if (zero? k)
        result
        (iter (- n 1) (- k 1) (* (/ n k) result))))
  (cond
   ((and (positive? n) (positive? k))
    (iter n k 1))
   ((and (negative? n) (positive? k))
    (* (if (even? k) 1 -1)
       (choose (+ (- n) k -1) k)))
   ((and (negative? n) (<= k n))
    (* (if (even? (- n k)) 1 -1)
       (choose (+ (- k) -1) (- n k))))
   ((and (positive? n) (zero? k)) 1)
   (else 0)))

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
