import Data.Function

getSecondFrom :: a -> b -> c -> b
getSecondFrom a b c = b

variants :: a -> a -> b -> a -> a
variants a b c d = a -- or: b d

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y

sumSquares = (+) `on` (^ 2)

multSecond = g `on` h

-- multSecond ('A',2) ('E',7)

g a b = a * b

h tpl = snd (tpl)

-- Lambda expression.
-- \x -> 2 * x + 7

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x + y + z) `on3` (^ 2)

-- takes max from arg and 42,
-- cubes the it,
-- calcs log2 from the result.
doItYourself = f' . g' . h'

f' = logBase 2

g' = (^ 3)

h' = max 42

-- tuple as function:
-- (,) False 42

-- variants :: a -> (a,b) -> a -> (b,a,a)
-- variants x f z = (snd f, x, x / x, z / z, z / z, x / fst f, fst f ...)

-- swap     :: (a,b) -> (b,a)
-- curry    :: ((a, b) -> c) -> a -> b -> c
-- uncurry  :: (a -> b -> c) -> (a, b) -> c
-- flip     :: (a -> b -> c) -> b -> a -> c
-- (,)      :: a -> b -> (a, b)
-- const    :: a -> b -> a

-- swp = f (g h)

swp = f'' (g'' h'')

h'' = (,)

g'' = flip

f'' = uncurry
