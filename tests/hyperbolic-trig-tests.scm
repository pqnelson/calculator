(import "../src/hyperbolic-trig.scm")

;; various identities the golden ratio satisfies...
(assert (float= (/ 1 :golden-ratio)
                (- :golden-ratio 1)))
(assert (float= (+ (square :golden-ratio) 
                   (/ 1 :golden-ratio)) 
                (* 2 :golden-ratio)))
(assert (< :ln-phi 1/2))
(assert (float= (phi-cf 38) :golden-ratio))
(assert (not (float= (phi-cf 36) :golden-ratio)))


;; special arctanh values
(assert (float= (arctanh 0) 0))
(assert (float= (arctanh 1) :+inf))
(assert (float= (arctanh +i) (* +i :pi/4)))
