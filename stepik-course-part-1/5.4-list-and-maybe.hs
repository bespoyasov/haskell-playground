module ListAndMaybe where

import Data.Char (digitToInt, isDigit)
import Numeric (readInt)

-- Make Maybe a Monad instance manually:
-- import Prelude hiding (Just, Maybe, Nothing)

-- data Maybe a = Nothing | Just a
--   deriving (Eq, Ord)

-- instance Monad Maybe where
--   return = Just

--   (Just x) >>= k = k x
--   Nothing >>= _ = Nothing

--   (Just _) >> m = m
--   Nothing >> _ = Nothing

--   fail = Nothing

type Name = String

type DataBase = [(Name, Name)]

dads, moms :: DataBase
dads = [("Bill", "John"), ("Ann", "Mike")]
moms = [("Bill", "Jane"), ("Ann", "Alice")]

getM, getD :: Name -> Maybe Name
getM person = lookup person moms
getD person = lookup person dads

-- Bill's great grandmother:
-- getD "Bill" >>= getM >>= getM

granmas :: Name -> Maybe (Name, Name)
granmas person = do
  mom <- getM person
  grandmomM <- getM mom
  dad <- getD person
  grandmomD <- getM dad
  return (grandmomM, grandmomD)

-- Effects are hidden in the implementation details.

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken x
  | all isDigit x = Just (Number (read x :: Int))
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . (map asToken) . words

-- Empty list in list Monad is as Nothing in Maybe.

-- instance Monad [] where
--   return x = [x]
--   xs >>= k = concatMap k xs
--   fail _ = []

-- Given:
-- data Board = ...
-- And:
-- nextPositions :: Board -> [Board]

data Board = Board Int deriving (Show)

nextPositions :: Board -> [Board]
nextPositions (Board x) = [Board $ x + 1, Board $ x + 2]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = filter pred [b]
  | otherwise = do
      move <- nextPositions b
      nextPositionsN move (n - 1) pred

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise = do
      a <- [1 .. x]
      b <- [1 .. x]
      c <- [1 .. x]

      -- This is also pattern matching:
      True <- [a ^ 2 + b ^ 2 == c ^ 2 && a < b && c <= x]
      return (a, b, c)
