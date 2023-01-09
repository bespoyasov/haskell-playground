addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b lst = a : b : lst


nTimes:: a -> Int -> [a]
nTimes a 0 = []
nTimes a x = a : (nTimes a (x - 1))

-- nTimes:: a -> Int -> [a]
-- nTimes = flip replicate

-- sndHead :: [(a, c)] -> c
-- sndHead ((,) ((:) _ _) x) = x
-- sndHead ((,) y z : x) = x
-- sndHead ((,) y x : z) = x -- okay
-- sndHead ((,) x y : z) = x
-- sndHead ((:) ((,) _ x) y) = x -- okay
-- sndHead ((_, x) : _) = x -- okay


oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
  | odd x     = x : oddsOnly xs
  | otherwise = oddsOnly xs


isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == reverse a


-- sum3 :: Num a => [a] -> [a] -> [a] -> [a]
-- sum3 a b c = sum2 a $ sum2 b c where
--   sum2 a b
--     | length a > length b = s' a b []
--     | otherwise = s' b a []
--     where
--       s' (a:as) (b:bs) result = a + b : s' as bs result
--       s' (a:as) [] result = a : s' as [] result
--       s' [] [] result = result

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = sum2 a $ sum2 b c where
  sum2 as [] = as
  sum2 [] bs = bs
  sum2 (a:as) (b:bs) = a + b : sum2 as bs


-- took help: https://stackoverflow.com/questions/53053864/beginner-haskell-group-list-of-ints
-- groupElems :: Eq a => [a] -> [[a]]
-- groupElems [] = []
-- groupElems (x:xs) = case span (== x) xs of
--   (grp, others) -> (x:grp) : groupElems others


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = equals : groupElems others where
  (equals, others) = span (== x) (x:xs)

--  Or using synonyms:
-- groupElems xs@(x:xs') = equals : groupElems others where
--   (equals, others) = span (== x) xs

-- [1,1,2,2,2,3,4]
-- ([1,1], [2,2,2,3,4]) => equals: [1,1]; others: [2,2,2,3,4]
-- [1,1] : [...]
--  [2,2,2,3,4]
--  ([2,2,2], [3,4]) => equals: [2,2,2]; others: [3,4]
--  [2,2,2] : [...]
--    [3,4]
--    ([3], [4]) => equals: [3]; others: [4]
--    [3] : [4]
--      [4]
--      ([4], []) => equals: [4]; others: []
--      [4] : [...]
--      [] patter matched => []
--      [4] : [] -> [4]
--    [3] : [4] -> [3,4]
--  [2,2,2] : [3,4] -> [2,2,2,3,4]
-- [1,1] : [2,2,2,3,4] -> [1,1,2,2,2,3,4]
