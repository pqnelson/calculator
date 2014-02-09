# Overview

We define three constants (`:ln-2`, `:ln-3`, and `:ln-10`), and provide
a general logarithm function.

There are a lot of other methods in here, mostly for testing purposes.

## Constants

We compute the various constants using a continued fraction expression
for `ln(z)`. It works best when `z` is nearly 1. So, we do the
following:
```scheme
(log/info "\nDefining :ln-2...")
(define :ln-2 (rationalize->exact
               (inc (euler-ln-cf (/ 2 :e) 45))
               (expt 10 -100))) ;; good to 100 digits 
(log/info "Defining :ln-3...")
(define :ln-3 (rationalize->exact
               (+ 1 (euler-ln-cf (/ 3 :e) 47))
               (expt 10 -100))) ;; good to 100 digits
(log/info "Defining :ln-10...")
(define :ln-10 (rationalize->exact
                (+ (* 2 :ln-3) (euler-ln-cf 10/9 40))
                (expt 10 -100))) ;; good to 100 digits
```
We consider `1+ln(2/e)` as the definition for `ln(2)`. Likewise, we do
`1+ln(3/e)` for the definition for `ln(3)`. 

But for `ln(10)`, we use `2*ln(3) + ln(10/9)`, and use our previous
definition of `ln(3)`.

The continued fraction produces unwieldy results (huge fractions with
several hundred digits in the numerator and in the denominator). So, we
truncate it to have a precision of only 100 digits. This slows things
down on startup, but if we don't do this...it slows **everything** down
later on!

## The Real Logarithm

We compute the logarithm for real numbers, implicitly in the principal
branch. So `ln(-1) = i*pi`, and for any `x > 0` we have 
`ln(-x) = i*pi + ln(x)`.

The code for this is:
```scheme
(define (real-ln c)
  (cond 
   ((negative? c) (+ (* +i :pi) 
                     (real-ln (- c))))
   ((infinite? c) :+inf.0)
   ((zero? c) :-inf.0)
   ((float= c 1) 0)
   ((>= c 1000) (+ (* 3 :ln-10)
                   (real-ln (/ c 1000))))
   ((>= c 10) (+ :ln-10
                 (real-ln (/ c 10))))
   ((>= c :e) (+ 1
                 (real-ln (/ c :e))))
   ((> 1/2 c 0) (- (real-ln (/ 1 c))))
   (else (rationalize->exact (euler-ln-cf c 25) (expt 10 -30)))))
```
We treat `ln(+inf)=+inf`, although it's not well defined.

We also treat `ln(0)=-inf`. 

For any number "essentially equal" to 1, we give exactly 0.

Then we do some "range reduction". If, e.g., we consider `ln(1234)`, we
change this to `3*ln(10) + ln(1.234)`. We have precomputed `ln(10)` as a
constant, good to 100 digits, so we just have to compute `ln(1.234)`. 

Our "range reduction" gets our number to `0 < c < e`.

Then we do something cavalier, which scares me:
```
   ((> 1/2 c 0) (- (real-ln (/ 1 c))))
```
If the argument is between `0` and `1/2`, we use the Law of Logarithms
to invert it and negate the resulting logarithm. I have no proof that
this would "settle", and not descend into infinite recursion.

But if the number is between `1/2 < c < :e`, then we use the continued
fraction, keeping 30 digits of precision.

## Complex Logarithm

The complex logarithm is fairly straightforward:

```scheme
(define (ln z)
  (cond
   ((float= 1 z) 0)
   ((float= :e z) 1)
   ((complex-number? z) (+ (real-ln (magnitude z))
                           (* +i (angle z))))
   (else (real-ln z))))
```

The `ln(1)=0`, the `ln(e)=1`. Always.

For a complex number, we treat it as a polar quantity `r*e^it`, the
compute `ln(r) + it`. 

For real numbers, we just use the algorithm aforementioned.
