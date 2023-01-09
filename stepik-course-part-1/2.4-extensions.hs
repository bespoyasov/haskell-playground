class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a | doesEnrageGork a && doesEnrageMork a = stomp $ stab a
                | doesEnrageGork a = stab a
                | doesEnrageMork a = stomp a
                | otherwise = a

instance KnownToGork Integer where
  stomp a = a + 42
  doesEnrageGork _ = False

instance KnownToMork Integer where
  stab a = a * 69
  doesEnrageMork _ = False

instance KnownToGorkAndMork Integer where



ip = show a ++ show b ++ show c ++ show d

a = 12
b = 7.22
c = 4.12
d = 0.12


-- when a function is polymorphic on the return type
-- shrink down the return type using ::
-- toEnum :: Int -> a
-- toEnum 122 :: Int


class (Bounded a, Eq a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a | a == maxBound = minBound
          | otherwise = succ a

  spred :: a -> a
  spred a | a == minBound = maxBound
          | otherwise = pred a

instance SafeEnum Bool
instance SafeEnum Char




avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3.0

-- https://wiki.haskell.org/Reducible_expression
