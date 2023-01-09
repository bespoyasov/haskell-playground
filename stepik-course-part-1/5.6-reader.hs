module ReaderMonad where

-- instance Monad ((->) e) where
--   return :: a -> e -> a
--   return x = \_ -> x
--   (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
--   m >>= k = \e -> k (m e) e

safeHead = do
  b <- null
  if b
    then return Nothing
    else do
      h <- head
      return $ Just h

safeHead' = do
  e <- id
  if null e
    then return Nothing
    else return $ Just (head e)

newtype Reader r a = Reader {runReader :: r -> a}

instance Monad (Reader r) where
  return x = Reader $ \e -> x
  m >>= k = Reader $ \e ->
    let v = runReader m e
     in runReader (k v) e

ask :: Reader r r
ask = Reader id

type User = String

type Password = String

type Users = [(User, Password)]

pwds :: Users
pwds = [("Bill", "123"), ("Alice", "ABC")]

firstUser :: Reader Users User
firstUser = do
  e <- ask
  return $ fst (head e)

asks :: (r -> a) -> Reader r a
asks = Reader

firstUserPwd :: Reader Users Password
firstUserPwd = asks (snd . head)

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ runReader m . f

someUsers :: Users
someUsers = [("user", "123456"), ("x", "hi"), ("root", "123456")]

usersWithBadPasswords :: Reader Users [User]
usersWithBadPasswords = asks (map fst . filter isBadPwd)
  where
    isBadPwd (_, pwd) = pwd == "123456"
