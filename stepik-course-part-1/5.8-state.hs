module StateMonad where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- `get` for reading;
-- `put` for writing;
-- `modify` for applying `f` to the state.

readerToState :: Reader r a -> State r a
readerToState m = state $ \e -> (runReader m e, e)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m =
  let (a, w) = runWriter m
   in state $ \e -> (a, e `mappend` w)

-- Jzs, I'm stupid...

fibStep :: State (Integer, Integer) ()
fibStep = do
  (a, b) <- get
  put (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (traverse tree) 1
  where
    traverse (Leaf _) = do
      n <- get
      modify succ
      return $ Leaf n
    traverse (Fork l _ r) = do
      l' <- traverse l
      n <- get
      modify succ
      r' <- traverse r
      return $ Fork l' n r'
