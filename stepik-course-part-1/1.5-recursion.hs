-- irrefutable parameter is on the last line.

doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n | n < 0 = error "The argument must be bigger than 0."
             | n > 0 = n * doubleFact (n - 2)

-- can use `undefined` or `error` functions.
-- `undefined` can be used like `pass` in python.

-- `n | n > 0` is a guard clause.
-- can use `otherwise`


fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci (-2) = (-1)
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)



fibonacci' :: Integer -> Integer
fibonacci' = fib 0 1

fib :: Integer -> Integer -> Integer -> Integer
fib x y n | n == 0 = x
          | n > 0  = fib y (x + y) (n - 1)
          | n < 0  = fib (y - x) x (n + 1)
