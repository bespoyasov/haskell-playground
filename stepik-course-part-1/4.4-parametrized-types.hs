-- https://xlinux.nist.gov/dads/HTML/manhattanDistance.html
-- https://wiki.haskell.org/Converting_numbers

import Data.Char
import Data.List

data Coord a = Coord a a deriving Show

originInt = Coord (0::Int) (0::Int)
originDouble = Coord (0::Double) (0::Double)

p1 = Coord (3::Int) (4::Int)
p2 = Coord (3.0::Double) (4.0::Double)

distance :: Coord Double -> Coord Double -> Double
distance (Coord ax ay) (Coord bx by) = sqrt ( (ax - bx) ^ 2 + (ay - by) ^ 2 )

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord ax ay) (Coord bx by) = abs (ax - bx) + abs (ay - by)


-- data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter scale (Coord a b) = Coord x y where
  x = fromIntegral a * scale + scale / 2
  y = fromIntegral b * scale + scale / 2

getCell :: Double -> Coord Double -> Coord Int
getCell scale (Coord x y) = Coord a b where
  a = floor (x / scale)
  b = floor (y / scale)


findDigit :: [Char] -> Maybe Char
findDigit s = case filter isDigit s of
  [] -> Nothing
  xs -> Just $ head xs

findDigitOrX :: [Char] -> Char
findDigitOrX x = case findDigit x of
  Nothing -> 'X'
  (Just a) -> a


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs



-- Some “lib functions” that I couldn't import for some Yak-Shaving reasons.

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

trim :: String -> String
trim = trimEnd . trimStart

splitOn ch str = (a, b) where
  a = trim $ takeWhile (/= ch) str
  b = trim $ tail $ dropWhile (/= ch) str


-- Something similar to a “real life” task.
-- "firstName = John\nlastName = Connor\nage = 30" -> Person

sample1 = "firstName = John\nlastName = Connor\nage = 30"
sample2 = "firstName = John\nlastName Connor\nage = 30"

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
  deriving (Show, Eq)

data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving (Show, Eq)

correctFields = [
  (0, "firstName"),
  (1, "lastName"),
  (2, "age")]

parsePerson :: String -> Either Error Person
parsePerson str
  | hasMissingFields = Left IncompleteDataError
  | not hasCorrectKeys = Left IncompleteDataError
  | not hasCorrectFormat = Left ParsingError
  | not hasCorrectAge = Left (IncorrectDataError $ val age)
  | otherwise = Right (Person {firstName=fn, lastName=ln, age=ag})
  where
    fields = lines str
    hasMissingFields = length fields < 3
    hasCorrectFormat = all (isInfixOf "=") fields

    pairs = map (splitOn '=') fields
    key = fst
    val = snd

    hasCorrectKeys = all (existsIn pairs) correctFields
    existsIn candidates (i, fieldName) = key (candidates !! i) == fieldName

    firstName = pairs !! 0
    lastName = pairs !! 1
    age = pairs !! 2

    hasCorrectAge = all isDigit $ val age

    fn = val firstName
    ln = val lastName
    ag = read (val age) :: Int



-- Strict flag `!`:

data Coords a = Coords a !a

getX :: Coords a -> a
getX (Coords x _) = x

getY :: Coords a -> a
getY (Coords _ y) = y
