(import "../src/utils.scm")

(assert (float= 0 0))
(assert (float= 1 (+ 1 (* 1/2 
                          *machine-epsilon*))))
