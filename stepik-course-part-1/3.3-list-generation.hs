{-# LANGUAGE NoMonomorphismRestriction #-}

fibStream :: [Integer]
fibStream = zipWith (+) (fib 1 0) (fib 0 0) where
  fib x y = y : (fib y (x + y))


-- repeat = iterate repeatHelper
-- iterate :: (a -> a) -> a -> [a]
-- repeat :: a -> [a]

repeatHelper :: a -> a
repeatHelper = id

repeat' = iterate repeatHelper

-- [1..10] is syntactic sugar over `enumFromTo 1 10`
-- [1,3..10] with step, sugar over `enumFromThenTo 1 3 10`
-- [1..] infinite list (`enumFrom 1`)


data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd a) = Odd (a + 2)
  pred (Odd a) = Odd (a - 2)

  fromEnum (Odd a) = fromIntegral a
  toEnum a | a `mod` 2 == 0 = error "Argument can't be even."
           | otherwise = Odd $ toInteger a

  enumFrom (Odd a) = map Odd [a, a+2..]
  enumFromThen (Odd a) (Odd b) = map Odd [a, a + step..] where step = b - a
  enumFromTo (Odd a) (Odd limit) = map Odd [a, a + 2 .. limit]
  enumFromThenTo (Odd a) (Odd b) (Odd limit) = map Odd [a, a + step.. limit] where step = b - a


-- When using comprehension, read `x <- xs` as “for all x in xs” (∈):
-- [x^2 | x <- xs]

coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change a | a < minimum coins = []
         | otherwise = [xs | xs <- listOfLists, sum xs == a]
  where
    listOfLists = [n:y | n <- coins, y <- [] : change (a - n)]


-- “standard” solution:
-- change :: (Ord a, Num a) => a -> [[a]]
-- change n | n < 0     = []
--          | n == 0    = [[]]
--          | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]
