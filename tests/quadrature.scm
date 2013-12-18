;; this isn't really a test, more a note to myself about runge phenomena

(define (runge-test-fn x)
  (/ 1 (+ 1 
          (* 25 
             (square x)))))

(define (runge-test approx)
  (let ((true-runge-integral (* 2/5 (arctan 5))))
    (* 1.0 (- approx true-runge-integral))))

(define (simpsons-runge-test)
  (runge-test (simpsons-rule runge-test-fn -1 1)))

(define (booles-runge-test)
  (runge-test (booles-rule runge-test-fn -1 1)))
