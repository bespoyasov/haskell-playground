import Data.List (unfoldr)

x = 7

a = foldr (-) x [2,1,5]
b = foldl (-) x [2,1,5]

-- b: f(f(f(ini 1) 2) 3)
-- a: f(1 f(2 f(3 ini)))


meanList :: [Double] -> Double
meanList = average . foldr sumAndSize (0, 0) where

  average :: (Double, Double) -> Double
  average (sum, size) = sum / size

  sumAndSize :: Double -> (Double, Double) -> (Double, Double)
  sumAndSize x (sum, size) = (sum + x, size + 1)


-- evenOnly :: [a] -> [a]
-- evenOnly lst = fst $ foldr f ([], even (length lst)) lst where
--   f x (lst, include) | include = (x:lst, False)
--                      | otherwise = (lst, True)

-- for infinite lists,
-- @see: https://wiki.haskell.org/Lazy_pattern_match

evenOnly :: [a] -> [a]
evenOnly = snd . foldr f ([], []) where
  f x ~(odd', even') = (x:even', odd')


lastElem :: [a] -> a
lastElem = foldl1 (\_ x -> x)

-- and also lastElem = foldl1 $ flip const


revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr f b where
  f x | x < a || x > b = Nothing
      | otherwise = Just (x, pred x)
