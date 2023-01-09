module MonadIO where

import Data.List (isInfixOf)
import System.Directory (getDirectoryContents, removeFile)

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  case name of
    "" -> main'
    _ -> putStrLn $ "Hi, " ++ name ++ "!"

-- Sort of:
-- newtype IO a = IO (RealWorld -> (RealWorld, a))
-- return :: a -> IO a
-- (>>=) :: IO a -> (a -> IO b) -> IO b

-- Isn't it the State monad tho?

-- Use Control.Monad when working with monads.
--
-- sequence_ :: Monad m => [m a] -> m ()
-- sequence_ = foldr (>>) (return ())
--
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
-- mapM f = sequence_ . map f
--
-- Everything with `_` postfix makes effects and returns unit.
-- Use it when need effects, use without `_` when need values.
--
-- sequence :: Monad m => [m a] -> m [a]
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

removeAndTell name = do
  putStrLn $ "Removing file: " ++ name
  removeFile name

main'' :: IO ()
main'' = do
  putStr "Substring: "
  mask <- getLine
  names <- getDirectoryContents "."

  case mask of
    "" -> putStrLn "Canceled"
    _ -> do
      let toDelete = filter (isInfixOf mask) names
      mapM_ removeAndTell toDelete
