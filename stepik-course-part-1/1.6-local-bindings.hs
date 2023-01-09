-- seqA :: Integer -> Integer
-- seqA n | n < 0     = error "Argument must be non-negative."
--        | otherwise = let
--                        seq' n i k1 k2 k3 = let r = (k3 + k2) - 2 * k1
--                                            in if i == n then r else seq' n (i+1) k2 k3 r
--                      in seq' n 3 1 2 3


seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = let
        helper 2 _ _ k3   = k3
        helper n k1 k2 k3 = helper (n - 1) k2 k3 (k3 + k2 - 2 * k1)
      in helper n 1 2 3


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count n = (sum, count) where
  sum = sumDigits (abs n) 0
  sumDigits 0 acc = acc
  sumDigits x acc = sumDigits (x `div` 10) (acc + (x `mod` 10))

  count = countDigits (abs n) 0
  countDigits 0 acc = acc
  countDigits x acc = countDigits (x `div` 10) (acc + 1)



integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = answer where
  answer = step * ((fa + fb) / 2 + series)
  step = (b - a) / 1000
  fa = f a
  fb = f b
  series = sigma 0 1
  sigma value 1000 = value
  sigma value x = sigma (value + f (a + x * step)) (x + 1)

-- Pattern matching also forces expression evaluation.
