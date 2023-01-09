import Data.Char (digitToInt)

-- data Bool = True | False

-- data T = A               ok
-- data Type = A | B | C    ok
-- data T = Con1 | Con2     ok
-- data T = Con1 | myCon2   nope
-- data Data = A | B        ok
-- data myType = A | B | C  nope

-- derived instances to log to console or compare
-- (there can be more type classes):
data B = T | F deriving (Show, Eq)

-- to avoid non-total functions,
-- use `:set -fwarn-incomplete-patterns`.
-- Use tabs when searching for a flag in the CLI.


-- Below, could use `data Color = Red | Green | Blue deriving Show`
-- but it seems like the task was to implement it “from scratch”.

-- data Color = Red | Green | Blue deriving Eq

-- instance Show Color where
--   show x | x == Red = "Red"
--          | x == Green = "Green"
--          | otherwise = "Blue"

data Color = Red | Green | Blue

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"


-- Should be partial (non-total):
charToInt :: Char -> Int
charToInt c
  | c >= '0' && c <= '9' = digitToInt c
  | otherwise = error "Argument must be a digit char."


-- Should also be partial:
stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


-- Must be: Error > Warning > Info
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Info Info = EQ
cmp Info _ = LT
cmp Warning Info = GT
cmp Warning Warning = EQ
cmp Warning _ = LT
cmp Error Error = EQ
cmp Error _ = GT

-- Or:
-- instance Enum LogLevel where
--   fromEnum Error    = 3
--   fromEnum Warning  = 2
--   fromEnum Info     = 1

-- cmp :: LogLevel -> LogLevel -> Ordering
-- cmp x y = compare (fromEnum x) (fromEnum y)

-- Or:
-- import Data.Function

-- cmp :: LogLevel -> LogLevel -> Ordering
-- cmp = compare `on` index
--   where index Info = 1
--         index Warning = 2
--         index Error = 3


-- `case ... of` for pattern matching inside an expression.

data Result = Fail | Success

doSomeWork :: Int -> (Result,Int)
doSomeWork x = (Fail, 500)
-- doSomeWork _ = (Success, 0)

processData :: Int -> String
processData x = case (doSomeWork x) of
  (Success, _) -> "Success"
  (Fail, code) -> "Fail: " ++ show code
