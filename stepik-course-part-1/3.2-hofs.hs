import Data.Char

readDigits :: String -> (String, String)
readDigits = span isDigit


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj a b = filter (\x -> a x || b x)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (pivot:items) = qsort less ++ [pivot] ++ qsort bigger where
  less = filter (< pivot) items
  bigger = filter (>= pivot) items


-- `concat` is like `flat`
-- `concatMap` is `flatMap`


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])


-- Always start with a trivial case like [1,2]
-- 1 : [2] -> 1 : 2 or 2 : 1 -> [[1,2], [2,1]]
-- 1 : [2,3] -> 1 : perms [2,3]


-- Well... https://www.lounge.se/HaskellPermutations
-- but again, `<-` wasn't in the course at that moment.
-- Had to take hints. (I'm kinda dumb when it comes to algos.)
-- The standard solution is:
-- perms :: [a] -> [[a]]
-- perms [] = [[]]
-- perms [x] = [[x]]
-- perms (x:xs) = concatMap (insertElem x) (perms xs) where
-- 			insertElem x [] = [[x]]
--       insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)


delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> max a $ max b c)
-- max3 = zipWith3 (\a b c -> a `max` b `max` c)
