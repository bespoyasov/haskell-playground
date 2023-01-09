module MonadIdentity where

import Control.Applicative
import Control.Monad (ap)
import Data.Functor

newtype Identity a = Identity {runIdentity :: a}
  deriving (Show, Eq)

-- Again,
-- `return` is the trivial Kleisli arrow
-- (take a value, return a wrapped one);
-- `mbind` take a wrapped value, extracts it,
-- runs calculation, returns a new wrapped value.

instance Monad Identity where
  return = Identity
  Identity x >>= k = k x

data SomeType a = SomeType a deriving (Show)

instance Applicative SomeType where
  pure = return
  (<*>) = ap

instance Monad SomeType where
  return = SomeType
  (>>=) (SomeType a1) f = f a1

-- My original solution:
-- instance Functor SomeType where
--   fmap f x = x >>= (return . f)

-- After using linters:
instance Functor SomeType where
  fmap f x = x <&> f

-- First monad law:
-- return x >>= k   ≣   k x

-- Second monad law:
-- m >>= return   ≣   m

-- Third monad law:
-- (m >>= k) >>= k'   ≣    m >>= (\x -> k x >>= k')

-- do {expr1; expr2}   ≣   e1 >> e2
-- do {p <- expr1; expr2}   ≣   expr1 >>= \p -> expr2
-- do {let v = e1; e2}   ≣ let v = e1 in do e2
