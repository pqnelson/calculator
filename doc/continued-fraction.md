We could implement the naive continued fraction algorithm, as we compute
it in grade school. This turns out to be horribly inefficient, because
division operations kill us on performance.

Instead, we use continuants, the way we learn it at university. It is
tempting therefore to write:
```scheme
(define (inc x) (+ 1 x))

(define (generalized-cont-frac a b n)
  (define (next-term current-term prev-term k)
    (+ (* (b k) current-term)
       (* (a k) prev-term)))
  (define (iter A-current A-prev B-current B-prev k)
    (if (> k n)
      (/ A-current
         B-current)
      (iter (next-term A-current A-prev k) A-current
            (next-term B-current B-prev k) B-current
            (inc k))))
  (iter (+ (a 1) (* (b 1) (b 0))) (b 0)
        (b 1) 1 
        2))
```
Then each `next-term` involves 1 addition operation, and 2
multiplication operations. So each iteration has 3 addition
operations, and 4 multiplication operations. 

If we call our continued fraction operation with 20 iterations, that
would have the `else` clause called 19 times and hence we'd have 19
`inc` calls, 40 `next-term` calls. Hence we'd have 19+120=139 addition
operations, and 160 multiplication operations, with 1 division
operation. Typically a division operation is as complex as
multiplication. So `(generalized-cont-frac a b 20)` has at least 139
addition operations and 161 multiplicative operations. 

A general rule of thumb I pull from thin air (which can be completely
wrong) is that the computational complexity for 1 multiplication is the
square of that for 1 addition operation.
