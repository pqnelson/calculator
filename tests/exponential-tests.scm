(import "../src/exponential.scm")

(assert (float= (euler-e 8) :e))
;; (/ 2314069263277926900572908636794854738 (expt 10 35))

(log/info "\nAlmost-integer tests...")
(assert (float= (- (exp :pi) :pi) 
                191711222585275430905/9585992508901148567))

(assert (< (- 884736744 (exp (* :pi (sqrt 43))))
           2.255e-4))

(assert (< (- 147197952744 (exp (* :pi (sqrt 67))))
           1.3376e-6))

(assert (< (- 262537412640768744 (exp (* :pi (sqrt 163))))
           8e-13))

(assert (< (- (pow :golden-ratio 17) 3571)
           3e-4))
(assert (< (- (pow :golden-ratio 18) 5778)
           2e-4))
(assert (< (- (pow :golden-ratio 19) 9349)
           1.1e-4))
(log/info "All exponential tests passed.")
