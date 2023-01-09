{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (ap, liftM)

-- A specific container is not important;
-- we can abstract the “laws” and usage patterns
-- into a monadic expression.
-- f :: a -> m b

data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (m' ++ m'') x''
  where
    Log m' x' = f x
    Log m'' x'' = g x'

add1Log = toLogger (+ 1) "added one"

mult2Log = toLogger (* 2) "multiplied by 2"

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m  -- bind
-- `bind` should “take out” `a` from `m` container
-- apply the given function and return `m b`.

toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f = \x -> return (f x)

-- toKleisli cos 0 :: [Double]
-- toKleisli cos 0 :: Maybe Double

returnLog :: a -> Log a
returnLog = Log []

-- It's basically, the `>>=` operator for the `Log` context.

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log ms a) f = Log (ms ++ ms') a'
  where
    (Log ms' a') = f a

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x = foldl (>>=) (return x)

-- Hehe:
-- execLoggersList x = foldr (=<<) (return x) . reverse

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) f g x = g x >>= f
