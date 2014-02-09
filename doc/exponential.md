# Overview

The basic game-plan in this module is to define `:e`, then define the
exponential function. Lets review how we do this piecewise.

## Defining Euler's Constant "e"

We use a continued fraction to do this. We use a rational approximation,
good to 100 digits. We use the same basic continued fraction for the
exponential function, so lets study it for `(exp z)`:

```scheme
(define (faster-exp-num z z-squared k)
  (if (> k 1) 
      z-squared
      (* 2 z)))
```

We only want to compute `(square z)` once, so we pass it in as a
parameter to the numerator. Basically, the numerator is always `(square
z)` except "at top", when it is `(* 2 z)`.

The denominator changes. It is a little strange, but it works:
```scheme
(define (faster-exp-den z k)
  (cond
   ((> k 1) (- (* 4 k) 2))
   ((= k 1) (- 2 z))
   (else 1)))
```
This is used to get 100 digits of precision for our definition of
Euler's constant `:e` and its square root `:sqrt-e`.

## Exponential Function Tricks

Now we define `fast-expt` for considering `x^n` where `n` is an integer
and `x` any real number:
```scheme
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
```
We hard code a number of conditions simplifying certain situations. For
example, `x^0 = 1` for any `x`. Or `x^1 = x` for any `x`.

We consider the "principal branch", taking `(-x)^k = e^(i*:pi*k)*x^k`.

The main workhorse is the internal `iter` function, which should run
logarithmically in the exponent.

### Real Exponential Function

Now we define the exponential function on the reals:
```scheme
(define (real-exp z)
  (cond
   ((infinite? z) (if (negative? z) 0 z))
   ((zero? z) 1)
   ((= z 1) :e)
   ((= z -1) (/ 1 :e))
   ((>= (abs z) :ln-2) (* (fast-expt 2 (quotient z :ln-2))
                          (real-exp (remainder z :ln-2))))
   ((>= :ln-2 (abs z) 1/2)
    (* (exp-cf (- z (* 1/2 (sgn z))) 10)
       (if (negative? z)
           (/ 1 :sqrt-e)
           :sqrt-e)))
   (else (exp-cf z 10))))
```
The first four cases are straightforward.

Lets consider this case:
```
   ((>= (abs z) :ln-2) (* (fast-expt 2 (quotient z :ln-2))
                          (real-exp (remainder z :ln-2))))
```
We do some "range reduction". We rewrite `z = q*ln(2) + r`. Then we say "Aha, 
`exp(z) = exp(q*ln(2) + r)` and this in turn is precisely `(2^q)*exp(r)`, 
and we know `0<=r<ln(2)`." We are in luck, because `ln(2)<0.6932` so we
have `exp(r)` be a quick computation.

But we can do even better! We already computed `exp(0.5)` and stored it
as `:sqrt-e`. So why not check if `0.5<=r`? We do this in the next
condition:
```
   ((>= :ln-2 (abs z) 1/2)
    (* (exp-cf (- z (* 1/2 (sgn z))) 10)
       (if (negative? z)
           (/ 1 :sqrt-e)
           :sqrt-e)))
```
So then we have rewritten `r = Q*0.5 + s` where `0<=s<0.1932`. Computing
`(exp 0.1932)` to, say, 30 digits is easy with the continued fraction:
just use 10 terms. That's precisely what we do.

If we're given some `-0.193 < z < 0.193`, then we just compute the
continued fraction.

### Complex Exponential Function

We have the completely general exponential function defined as:
```scheme
(define (exp z)
  (if (complex-number? z)
      (* (real-exp (real-part z))
         (+ (real-cos (imag-part z))
            (* +i (real-sin (imag-part z)))))
      (real-exp z)))
```
Basically, it checks if we are given a complex number `z = x + iy`. If
so, then compute `real-exp(x) * (cos(y) + i*sin(y))`. Otherwise, just
compute `real-exp(z)` since `z` is real.

