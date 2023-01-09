-- sort :: ? => [d] -> [d]
-- Comparable d => ?
-- Ord d =>

-- Eq = equals, Ord = order.

-- When defining class, minimal complete definition.


class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString x | x == True = "true"
             | otherwise = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString pair = let (x, y) = pair
                  in "(" ++ toString x ++ "," ++ toString y ++ ")"
