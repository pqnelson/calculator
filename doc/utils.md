# Overview

We will describe the functions living in `/src/utils.scm`. They are
mostly helper functions.

## Number Comparisons

I have redefined a few predicates for my convenience. They work as
expected:
```scheme
(define (complex-number? z)
  (and (number? z)
       (not (real? z))))

(define (real-number? z)
  (and (real? z)
       (not (complex? z))))

(define (zero? z)
  ((lambda (r)
     (if (exact? r)
         (= r 0)
         (float= r 0)))
   (abs z)))

(define (negative? x)
  (and (real? x) (< x 0.0)))

(define (positive? x)
  (and (real? x) (> x 0.0)))

(define (even? n) 
  (zero? (remainder n 2)))
```
There are a few helper functions for rudimentary arithmetic I have:
```scheme
(define (inc x)
  (+ 1 x))

(define (square x)
  (* x x))

(define (sgn x)
  (if (positive? x)
      1
      (if (negative? x)
          -1
          0)))

(define (abs x)
  (cond
   ((infinite? x) :+inf.0)
   ((complex-number? x) (magnitude x))
   ((negative? x) (- x))
   (else x)))
```
This last one, redefining `abs` is a bit of abuse of notation, but meh
nothing terrible.
  
## Floating Point Helpers

We define two constants (`*machine-epsilon*` and
`*sqrt-machine-epsilon*`) and one helper function for comparison
(`float=`). 

### Machine Epsilon

We specify the machine-epsilon value. This is a small number `epsilon`
such that
```scheme
(= (+ 1.0 (/ epsilon 2.0))
   1.0)
```
The implementation is quite straightforward:
```scheme
(define *machine-epsilon*
  (let loop ((e 1.0))
    (if (= 1.0 (+ e 1.0))
        (* 2 e)
        (loop (/ e 2)))))

(define *sqrt-machine-epsilon* (sqrt *machine-epsilon*))
```

### Floating Point Comparison

This is straight out of Donald Knuth's *The Art of Computer
Programming*, vol. 2, pages 233 *et seq.* of the Third edition. Knuth
defines four comparison operators. I will butcher them, and consider
only one. In pseudo-scheme, it is
```scheme
(define (float= a b epsilon)
  (<= (abs (- v u))
      (* epsilon
         (ldexp
          1
          (min (frexp u)
               (frexp v))))))
```
where `(frexp u)` is pseudo-scheme for the analogous
[C function](http://www.cplusplus.com/reference/cmath/frexp/)
and likewise for [ldexp](http://www.cplusplus.com/reference/cmath/ldexp/).

Knuth shows, in Eq (34), that this condition is then equal to
```scheme
(and 
 (<= (abs (- u v))
     (* epsilon
        (abs u)))
 (<= (abs (- u v))
     (* epsilon
        (abs v))))
```
But then, setting `epsilon` to `*machine-epsilon*`, this is logically
equivalent to:
```scheme
(define (float= a b)
  (<= (abs (- a b))
      (* 1/2
         (min (abs a)
              (abs b))
         *machine-epsilon*)))
```
Hence this gives us our definition!

# Infinities

This is, sadly, system dependent code. I've made my decision to stick
with MIT-Scheme, so it works with my local version of it.

## Infinity as Number

I feel a bit cavalier declaring infinity is a number, but that's how I
implemented it.
```scheme
(define +inf.0 ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
                #b0111111111110000000000000000000000000000000000000000000000000000))
(define -inf.0 ((make-primitive-procedure 'CAST-INTEGER-TO-IEEE754-DOUBLE 1)
                #b1111111111110000000000000000000000000000000000000000000000000000))
```
Technically, these are illegal identifiers, but I don't use them in
practice.
```scheme
;; aliases to be consistent with how I'm defining constants...
(define :+inf.0 +inf.0)
(define :-inf.0 -inf.0)
(define :+inf.i (* +i :+inf.0))
(define :-inf.i (* -i :+inf.0))
```
The convention I am sticking with is: any mathematical constant is
prefixed with a colon.

Now we have some predicates testing for finiteness:
```scheme
(define (real-finite? x)
  (and (flo:flonum? x)
       (flo:finite? x)))

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
These are self-explanatory, more or less.

# Incorrectly Placed Methods

I have a few functions lumped in here incorrectly which should be moved
in the future. Namely, Newton's method:
```scheme
(define (newtons-method f deriv guess n)
  ((lambda (iterate)
     (if (or (> n 13) 
             (float= (- guess iterate) 
                     guess))
         (- guess iterate)
         (newtons-method f deriv (- guess iterate) (inc n))))
   (/ (f guess) 
      (deriv guess))))
```
I also have a helper for computing sums:
```scheme
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sigma term a b)
  (sum term a inc b))
```

# Number Theoretic Helpers

I have redefined the `quotient` and `remainder` function to work with
floating point numbers.
```scheme
(define (quotient a b)
  (truncate (/ a b)))

(define (remainder a b)
  (- a 
     (* b (quotient a b))))
```
Again, these are the "follow your nose" definitions.

# Unit Testing

I have some pseudo-unit testing code. Basic assert macros, namely
```scheme
(define-syntax assert
  (syntax-rules ()
    ((assert ?x)
     (if (not ?x) (error "Assertion failed" '?x)))))
```
The logging function just prints out to the screen a message:
```scheme
(define (log/info msg)
  (write-string msg)
  (newline)
  (flush-output))
```
