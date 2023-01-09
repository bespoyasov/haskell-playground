concatList :: [[a]] -> [a]
concatList = foldr (++) []


lengthList :: [a] -> Int
lengthList = foldr (const (+1)) 0
-- lengthList = foldr f 0 where f _ s = s + 1


sumOdd :: [Integer] -> Integer
sumOdd = foldr f 0 where
  f x acc | odd x = x + acc
          | otherwise = acc
