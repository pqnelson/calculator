(import "./src/trig.scm")

(log/info "\nRunning trig tests...")
(log/info "Testing cosine identities...")
(assert (float= (cos 0) 1))
(assert (float= (cos (/ :pi 12))
                (/ (+ (sqrt 2) (sqrt 6)) 4)))
(assert (float= (cos (/ :pi 8))
                (/ (sqrt (+ 2 (sqrt 2))) 2)))
(assert (float= (cos (/ :pi 6))
                (/ (sqrt 3) 2)))

(log/info "Testing sine identities...")
(assert (= (real-sin 0) 0))
(assert (float= (sin (/ :pi 12))
                (/ (- (sqrt 6) (sqrt 2)) 4)))
(assert (float= (sin (/ :pi 8))
                (/ (sqrt (- 2 (sqrt 2))) 2)))
(assert (float= (sin (/ :pi 6)) 1/2))
(assert (float= (sin :pi/4) (/ 1 (sqrt 2))))
(assert (float= (sin (/ :pi 3))
                (/ (sqrt 3) 2)))
(assert (float= (sin (* 5/12 :pi))
                (/ (+ (sqrt 2) (sqrt 6)) 4)))

(log/info "All trig tests passed successfully!")
