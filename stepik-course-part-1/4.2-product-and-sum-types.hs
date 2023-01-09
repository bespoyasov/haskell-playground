-- `A | B` is a sum because A _OR_ B
-- `Constructor A B` is a product because A _AND_ B

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point ax ay) (Point bx by) = sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b


data Result = Fail | Success

doSomeWork :: Int -> (Result,Int)
-- doSomeWork x = (Fail, 500)
doSomeWork _ = (Success, 0)

data Result' = Fail' Int | Success'

instance Show Result' where
    show Success' = "Success"
    show (Fail' code) = "Fail: " ++ show code

doSomeWork' :: Int -> Result'
doSomeWork' x = case doSomeWork x of
  (Success, _) -> Success'
  (Fail, code) -> Fail' code



-- data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False



data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

instance Show Z where
  show (Z sign bits) = s ++ bs where
    s = case sign of
      Minus -> "-"
      Plus -> "+"

    h Zero = "0"
    h One = "1"

    bs = concat $ map h bits

toDigit :: Bit -> Int
toDigit Zero = 0
toDigit One = 1

toDecimal :: Z -> Int
toDecimal (Z sign bits) = s * n where
  s = case sign of
    Minus -> -1
    Plus -> 1

  convert :: [Int] -> Int -> Int
  convert [] _ = 0
  convert (smallest:rest) power = smallest * 2 ^ power + convert rest (power + 1)

  n = convert (map toDigit bits) 0

toBinary :: Int -> Z
toBinary x = (Z sign bits) where
  sign = case signum x of
    -1 -> Minus
    _ -> Plus

  convert :: Int -> [Bit]
  convert 0 = []
  convert n | n `mod` 2 == 1 = One : convert (n `div` 2)
            | n `mod` 2 == 0 = Zero : convert (n `div` 2)

  bits = convert . abs $ x

add :: Z -> Z -> Z
add a b = toBinary $ toDecimal a + toDecimal b

mul :: Z -> Z -> Z
mul a b = toBinary $ toDecimal a * toDecimal b


-- “Lazy” and irrefutable patterns.
foo :: Bool -> Int
foo ~True = 1
foo False = 0
