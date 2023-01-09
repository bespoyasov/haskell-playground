foo a = a

bar = const foo

baz x = const True

quux = let x = x in x

corge = "Sorry, my value was changed"

grault x 0 = x
grault x y = x

garply = grault 'q'

waldo = foo


-- WHNF: https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form
-- seq:
-- - https://stackoverflow.com/questions/11046590/the-seq-function-and-strictness/11048004#11048004
-- - https://stackoverflow.com/questions/23570589/would-you-ever-write-seq-x-x
-- - https://wiki.haskell.org/Seq

-- As an example, suppose x :: Integer, then seq x b behaves essentially like if x == 0 then b else b – unconditionally equal to b, but forcing x along the way. In particular, the expression x `seq` x is completely redundant, and always has exactly the same effect as just writing x.

-- f $! x = x `seq` f x
-- const 42 $! undefined // Exception: Prelude.undefined


-- Lazy Computations: https://habr.com/ru/post/247213/
