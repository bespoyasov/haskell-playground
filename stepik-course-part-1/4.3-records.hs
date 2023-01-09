-- f $ g $ h $ x
-- x & h & g & f
-- `&` is left associative.

import Data.Time.Clock
import Data.Time.Format

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {
  timestamp :: UTCTime,
  logLevel :: LogLevel,
  message :: String
}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString entry = timeToString (entry & timestamp)
  ++ ": "
  ++ logLevelToString (entry & logLevel)
  ++ ": "
  ++ (entry & message)


test1 =
  let time = read "2019-02-24 18:28:52.607875 UTC"::UTCTime
      entry = LogEntry time Info "Info Message"
  in logEntryToString entry


-- LogEntry time Info "Info Message"
-- Or:
-- LogEntry {
--   timestamp = time,
--   logLevel = Info,
--   message = "Info Message"
-- }


-- entry = LogEntry time Info "Info Message" where
--   time = read "2019-02-24 18:28:52.607875 UTC"::UTCTime

updateMessage :: String -> LogEntry -> LogEntry
updateMessage newMessage entry = entry {message = newMessage}


data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving Show

jeff = Person {firstName = "Jeff", lastName = "Bezos", age = 58}
bill = Person {firstName = "Bill", lastName = "Murray", age = 72}

updateLastName :: Person -> Person -> Person
updateLastName from to = to {lastName = (from & lastName)}

billBezos = updateLastName jeff bill


fullName :: Person -> String
fullName p = firstName p ++ " " ++ lastName p

-- Or:
-- fullName (Person fn ls _) = fn ++ " " ++ ln
-- fullName (Person {firstName = fn, lastName = ln}) = fn ++ " " ++ ln


data Shape = Circle Double | Rectangle Double Double

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False


-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName person = person {firstName = shorten . firstName $ person} where
  shorten :: String -> String
  shorten name | length name > 1 = (head name) : "."
               | otherwise = name
