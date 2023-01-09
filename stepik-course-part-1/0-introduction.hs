import Data.Char

main = putStrLn "Hello, world!"

-- lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)
lenVec3 x y z = sqrt $ x ^ 2 + y ^ 2 + z ^ 2
-- $ is the operator with 0 priority and right associativity.

sign x = if x > 0 then 1 else if x < 0 then (-1) else 0

max5 = max 5
-- max5 x = max 5 x
-- x is reduced
-- due to partial application.

-- infixl / infixr / infix
-- define operators with specified associativity.

infixl 6 *+*
(*+*) a b = a ^ 2 + b ^ 2

-- calling a function is always more prior to using an operator:
-- result is 100
-- let cst = (*) 2 ((+) 1 4) ^ 2

infixl 9 |-|
x |-| y = abs (x - y)

-- (`mod` 14) ((+ 5) 10)
-- is 1 because `mod` is infix function
-- basically an operator and it's sectioned right.

-- logBase 4 (min 20 (9 + 7))
rewriteLogBase = logBase 4 $ min 20 $ 9 + 7

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

-- standardDiscount :: ???
standardDiscount = discount 1000 5


dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2


-- prepend item in list -> item : list
-- concatenate: ++

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt  y else 100
