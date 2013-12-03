# Introduction

These are some dirty tricks for some interactive Scheme calculations.

**Conventions:** We indicate a mathematical constant by prepending a colon 
to the identifier. So, for example, `:pi` refers to the mathematical constant 
approximately 3.141, `:e` refers to Euler's constant, `:golden-ratio` refers 
to...well, you get the idea.

## Infinities
We will declare infinity as a constant. Specifically, there are 4 of interest 
(plus-or-minus infinity, and plus-or-minus imaginary infinity):
```scheme
(define :+inf.0 (/ 1.0 0.0))
(define :-inf.0 (/ -1.0 0.0))
(define :+inf.i (/ +i 0.0))
(define :-inf.i (/ -i 0.0))

(define (complex-number? z)
  (and (number? z)
       (not (real? z))))

(define (real-infinite? x)
  (and (flo:flonum? x)
       (not (flo:finite? x))))

(define (infinite? z)
  (if (complex-number? z)
    (or (real-infinite? (real-part z))
        (real-infinite? (imag-part z)))
    (real-infinite? z)))

(define (finite? z)
  (not (infinite? z)))
```
It's mildly cavalier to treat infinity like a number, but meh the nervous 
programmer can rest assured we can always use Stereographic projection to embed 
the complex numbers into a compact space and work from there. (It's a 
triviality!)

One disadvantage is that we cannot say `(= x :+inf.0)` for example, Scheme will 
throw a fit. Instead, we can test indirectly for finiteness.

## Floating Point Equality
We can't always use `(= a b)` to test equality for floating point numbers `a` 
and `b`. Instead, following Donald Knuth (as in his *TAOCP*, vol 2, third ed., 
pp 233-235), we say `(float= a b)` if and only if 
`(<= (abs (- a b)) (* tol (abs a)))` and `(<= (abs (- a b)) (* tol (abs b)))`. 
Usually `tol` is machine epsilon. We see these two conditions hold if the 
absolute difference is less than `(* tol (min (abs a) (abs b)))`. This is our 
definition!
```scheme
(define (float= a b)
  (<= 
    (/ (abs (- a b))
       (/ (+ 1.0 (min (abs a) (abs b))) 2))
    1e-16))
```

## Continued Fractions

I took a more general approach than SICP, using Generalized continued fractions 
explicitly (SICP uses them implicitly and it is mildly confusing).

```scheme
(define (inc x)
  (+ 1 x))

;; wikipedia's notation for a generalized continued fraction
(define (generalized-cont-frac a b k)
  (define (recur i) 
    (if (> i k) 
      0 
      (/ (a i) 
         (+ (b i) (recur (inc i)))))) 
  (recur 1)) 

;; a continued fraction is a generalized continued fraction with a=1
(define (cont-frac b k)
  (generalized-cont-frac (lambda (i) 1) b k))
```

Great, so lets have some fun with this. There's a quick way to compute _e_ 
(Euler's constant):

```scheme
(define (euler-cont-frac-term k)
  (if (= k 1)
    1
    (* 2 (- (* 2 k) 1))))

(define (euler-e k)
  (+ 1.0 (* 2.0 (cont-frac euler-cont-frac-term k))))

;; (euler-e 7) => 2.7182818284590455

(define :e (euler-e 10))
```

### Golden Ratio
The famous continued fraction for the Golden ratio is quite 
simple [1; 1, 1, 1, ...]. 
```scheme
(define (phi k)
  (+ 1 (* 1.0 (cont-frac (lambda (i) 1) k))))

;; (phi 36) =>  1.618033988749894
(define :golden-ratio (phi 36))
```

### ArcTangent Function and other Inverse Trigonometric Functions
The inverse tangent function ("arc-tangent" function) can be computed using 
continued fractions as well. There are two different ways to do it. Euler 
gives us one approach:
```scheme
(define (euler-arctan z k)
  (generalized-cont-frac
    (lambda (j)
      (if (= j 1)
        z
        (square (* z 
                   (- (* 2 j) 3)))))
    (lambda (j)
      (if (= j 1)
        1
        (- (- (* 2 j) 1)
           (* (- (* 2 j) 3) 
              (square z)))))
    k))

;; see, e.g., http://en.wikipedia.org/wiki/Computing_%CF%80#Other_classical_formulae
(define :pi (+ (* 20 (euler-arctan (/ 1 7) 45))
               (* 8 (euler-arctan (/ 3 79) 45))))
(define :2pi (* 2 :pi))
(define :pi/4 (+ (* 5 (euler-arctan (/ 1 7) 45))
                 (* 2 (euler-arctan (/ 3 79) 45))))
(define :pi/2 (* :pi/4 2))
```
We use various identities with the arctangent function to "reduce the range".
```scheme
(define (real-arctan x)
  (cond
    ((infinite? x) (* (if (positive? x) 1 -1) :pi/2))
    ((> (abs x) 10) (- :pi/2 (real-arctan (/ 1 x))))
    ((> (abs x) 0.9) (* 2 
	                   (euler-arctan
                         (/ x (inc (sqrt (inc (square x)))))
                         25)))
    (else (euler-arctan x 25))))

;; holder for more general arctangent function
(define (arctan x) 
  (real-arctan x))
```
Observe we have to use `(eqv? x :+inf.0)` to check if the number is infinite 
or not.

Now, we can use the arctangent to compute the arc-sine and arc-cosine functions.
```scheme
(define (arccot x) 
  (arctan (/ 1 x)))

(define (arcsin x)
  (* 2 
     (arctan (/ x 
               (inc (sqrt (- 1 (square x))))))))

(define (arccsc x) 
  (arcsin (/ 1 x)))

(define (arccos x)
  (- :pi/2
     (arcsin x)))

(define (arcsec x) 
  (arccos (/ 1 x)))
```
### Tangent and other Trigonometric Functions
We can compute special functions as continued fractions, for example:

```scheme
(define (tan-cf x k)
  (generalized-cont-frac
    (lambda (i) (if (= 1 i) x (- (square x))))
    (lambda (i) (- (* 2 i) 1))
    k))
```
We can consider sine as a continued fraction, then use the identity 
`(= (cos x) (sin (- (/ :pi 2) x)))` to compute cosine. The problem with this 
approach is the sine continued fraction is...well, quite bizarre, and it's hard 
to handle range reduction. What we do instead is: consider the Taylor series. 
This is a common approach, decrease by multiples of pi, then plug it into the 
Taylor series. It works for "small-er" values, but for say "huge `x`"...we have 
unavoidable problems.

We implicitly put the truncated Taylor series in [Horner form](http://en.wikipedia.org/wiki/Horner%27s_method).
```scheme
;; need 8 terms to get machine precision after range-reduction
(define (sine-taylor-series x)
  (define (iter k result)
    (if (= 0 k)
      (* x result)
      (iter (- k 1) (+ 1 (* (/ (- (square x)) 
                               (* (* 2 k) (inc (* 2 k)))) 
                            result)))))
  (iter 20 1))

(define (sine-range-reduce x)
  (truncate 
    (+ (/ x :pi) 
       (/ 1 2))))
  
(define (sin x)
  ((lambda (n)
     (* (if (even? n) 1 -1)
        (sine-taylor-series (- x (* :pi n)))))
    (sine-range-reduce x)))

(define (cos x)
  (sin (- :pi/2 x)))

(define (csc x)
  (/ 1 (sin x)))

(define (sec x)
  (/ 1 (cos x)))

(define (cot x)
  (/ 1 (tan x)))
```
We do a crude form of range reduction when computing sine, it could 
(and should) be improved in the future.

# Exponentiation

Exponentiation occurs quite frequently in mathematics, so we should probably 
implement it. We note the identity `b^(-n)=(1/b)^n` is implemented, but the 
following implementation works if and only if `n` is an integer.
```scheme
(define (even? n) 
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond
    ((infinite? n) (if (negative? n) 0 :+inf.0))
    ((= n 0) 1)
    ((< n 0) (fast-expt (/ 1 b) (- n))))
    (else (* (if (even? n) 1 b) 
             (fast-expt (* b b) (quotient n 2))))))
```
What do we do for fractional `n`?

## Exponential Function

We use Euler's constant as the base, then use the natural logarithm to modify 
the exponent. We introduce the exponential function as a continued fraction 
(see, e.g., [wikipedia](https://en.wikipedia.org/wiki/Exponential_function#Continued_fractions_for_ex)):
```scheme
(define (euler-exp-a z k)
  (if (= 1 k)
    (* 2 z)
    (square z)))

(define (euler-exp-b z k)
  (+ 2
    (if (= 1 k)
      (- z)
      (* (- k 1) 4))))

(define (exp-cf z k)
  (+ 1.0
    (generalized-cont-frac
      (lambda (j) (euler-exp-a z j))
      (lambda (j) (euler-exp-b z j))
      k)))
```
We can combine the continued fraction and the "old-fashioned power" function 
to get a better exponential function:
```scheme
(define (exp z)
  (* (fast-expt :e (truncate z))
     (exp-cf (- z (truncate z)) 8)))

(assert (= (exp-cf 1 8) :e))
(assert (= (exp 1) :e))
```
Well, really, this is partially true. It's true for *real* `z`. To implement 
the exponential function for *complex* `z`, we would have
```scheme
(define (real-exp z)
  (* (fast-expt :e (truncate z))
     (exp-cf (- z (truncate z)) 8)))

(define (exp z)
  (if (and (number? z)
           (not (real? z)))
    (* (real-exp (real-part z))
       (+ (cos (imag-part z))
          (* +i (sin (imag-part z)))))
    (real-exp z)))
```
## Hyperbolic Trigonometric Functions
We can easily implement hyperbolic trigonometric functions now that we have 
the exponential:
```scheme
(define (sinh x)
  (/ (- (exp x)
        (exp (- x)))
     2))

(define (csch x)
  (/ 1 (sinh x)))

(define (cosh x)
  (/ (+ (exp x)
        (exp (- x)))
     2))

(define (sech x)
  (/ (cosh x)))

(define (tanh x)
  (/ (sinh x)
     (cosh x)))

(define (coth x)
  (/ (cosh x)
     (sinh x)))
```
## Logarithms

We use Newton's method for calculating the natural logarithm, but with 
some precautions: namely, we have a "counter limit" (we iterate at most 
53 times):
```scheme
(define (newtons-method f deriv guess n)
  ((lambda (iterate)
     (if (or (> n 53) (float= (- guess iterate) guess))
       (- guess iterate)
       (newtons-method f deriv (- guess iterate) (inc n))))
   (/ (f guess) (deriv guess))))
```
Then we just use this to figure out the natural logarithm, since 
`(exp (ln x))` is identical with `x`. So given `c`, we compute its natural 
logarithm by finding the root to the function `(lambda (x) (- (exp x) c))`.

We do some simplifications, namely, we precompute `ln 1` is zero, and we 
recursively factor out multiples of 10 until we get a "small enough number".

We "range reduce" to a number less than `:e`, then we guess some number based
on the relationship `(= (ln z) (* 2 (arctanh (/ (- z 1) (+ z 1)))))`.
```scheme
(define (ln-series z)
  ((lambda (u)
    (* 2
       u
       (+ 1
          (* (square u)
             (+ 1/3
                (* 1/5 (square u)))))))
   (/ (- z 1) (+ z 1)))) 

(define (ln-iterate c)
  (newtons-method 
    (lambda (x) (- (exp x) c)) 
    exp
    (ln-series c)
    0))

(define :ln-10 (ln-iterate 10))

(define (approx-real-ln c)
  (cond 
    ((< c 0) (+ +i :pi (real-ln (- c))))
    ((infinite? c) :+inf.0)
    ((= c 0) :-inf.0)
    ((= c 1) 0)
    ((> c 10) (+ (approx-real-ln (/ c 10)) :ln-10))
    ((> c :e) (+ (approx-real-ln (remainder c :e))
                 (quotient c :e)))
    (else (ln-iterate c))))

(define (real-ln z)
  ((lambda (y)
     (+ y
        (ln-iterate (/ c (exp y)))))
    (approx-real-ln z)))

(define (ln z)
  (if (complex-number? z)
    (+ (real-ln (magnitude z))
       (* +i (angle z)))
    (real-ln z)))
```
We can use the law of logarithms to implement logarithms in other bases, e.g.,
```scheme
(define :ln-2 (ln 2))

;; base-2 logarithm
(define (lg x)
  (/ (ln x) :ln-2))

;; base-10 logarithm
(define (log x)
  (/ (ln x) :ln-10))
```
### Inverse Trig Functions Redux
We had a temporary placeholder for `arctan`. Now that we have the logarithm 
function defined for, well, all values, then we can compute the arc-tangent 
function for complex values.
```scheme
(define (arctan z)
  (if (complex-number? z)
    (* (/ +i 2)
       (ln (/ (- 1 (* +i z))
              (+ 1 (* +i z)))))
    (real-arctan z)))
```
We can also consider the inverse hyperbolic trigonometric functions:
```scheme
(define (arccosh x)
  (ln
    (+ x
       (sqrt (inc (square x))))))

(define (arcsinh x)
  (ln 
    (+ x
       (sqrt (- (square x) 1)))))

(define (arctanh x)
  (/ 
    (ln (/ (+ 1 x)
           (- 1 x)))
    2))
```
# Square Roots
We can use Newton's method to quickly implement the squareroot, since it's 
essentially the root to the problem `(identical? (sqrt x) (- (square z) x))`. 
So we have:
```scheme
(define (real-sqrt x)
  (cond
   ((negative? x) (* +i (real-sqrt (abs x))))
   ((> x 100) (* 10 (real-sqrt (/ x 100))))
   (else (newtons-method
           (lambda (t) (- (square t) x))
           (lambda (t) (* 2 t))
           (/ (inc x) 2)
           0))))

(define (sqrt x)
  (if (complex-number? x)
    (* (real-sqrt (magnitude x))
      (exp (* +i (angle x) (/ 1 2))))
    (real-sqrt x)))
```
This is actually more precise than if we use floating point arithmetic. For 
example, `(sqrt 2)` gives us `886731088897/627013566048` -- we should note 
`(- (square (sqrt 2)) 2)` is about `-2.5e-24`. Similarly, 
`(- (square (sqrt 3)) 3)` gives us about `-6e-18`. This is beyond machine 
precision!
# Factorials and Friends

We have the run-of-the-mill factorial function:

```scheme
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count) 
    product 
    (fact-iter (* counter product) (inc counter) max-count)))
```
This is actually horribly inefficient for large `n`.

This allows us to write the binomial coefficient:
```scheme
(define (choose n k)
  (if (> k n)
    0
    (/ (factorial n) 
       (* (factorial k) 
          (factorial (- n k))))))
```

### Stirling Numbers
We can now compute Stirling numbers. We introduce the `sum` function:
```scheme
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))
```
Then we can construct Stirling numbers of the second kind:
```scheme
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
```
