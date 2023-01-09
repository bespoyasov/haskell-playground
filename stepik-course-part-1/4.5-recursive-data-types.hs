data List a = Nil | Cons a (List a)
  deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


data Nat = Zero | Suc Nat
  deriving (Show, Eq)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Suc b) = add (Suc a) b

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = (Suc Zero)
fac (Suc Zero) = (Suc Zero)
fac (Suc x) = mul (Suc x) (fac x)



data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = max (height a + 1) (height b + 1)
-- height (Node a b) = 1 + on max height a b

size :: Tree a -> Int
size (Leaf a) = 1
size (Node a b) = size a + size b + 1
-- size (Node a b) = 1 + on (+) size a b


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node a b) = (x, y) where
      (a', a'') = go a
      (b', b'') = go b
      x = a' + b'
      y = a'' + b''



infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand = until (\x -> expand' x == x) expand'
expand' ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand' (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand' (e1 :+: e2) = expand e1 :+: expand e2
expand' (e1 :*: e2) = expand e1 :*: expand e2
expand' e = e
