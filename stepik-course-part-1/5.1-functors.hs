import Data.Char

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- Since `f` must have a `* -> *` kind.

-- Takes a function and a container,
-- applies the function to every element of the given container,
-- and returns the same container with transformed elements.
-- (“It moves the calculation / transformation into the container.”)

-- Since `:k [] :: * -> *`, then:
-- instance Function [] where
--   fmap = map

-- instance Function Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just a) = Just $ f a


data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
  deriving Show

instance Functor GeomPrimitive where
  fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)
  fmap f (Point p) = Point (fmap f p)


-- `fmap` operator is `<$>`
-- for keeping structure but overriding values, use `<$`


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (fmap f a)
  fmap f (Branch l a r) = Branch (fmap f l) (fmap f a) (fmap f r)

-- instance Functor Tree where
--   fmap f (Leaf a) = Leaf (f <$> a)
--   fmap f (Branch l a r) = Branch (f <$> l) (f <$> a) (f <$> r)


-- For types with kind `* -> * -> *`, use partial application:
-- instance Functor (Either e) where...
-- (a -> b) -> (Either e a) -> (Either e b)

-- instance Functor (Either e) where
--   fmap _ (Left x) = Left x
--   fmap f (Right y) = Right (f y)

-- :k (->) :: * -> * -> *, then:
-- :k e :: * -> *, so:
-- (a -> b) -> (e -> a) -> (e -> b)
-- (Here `e` in “environment”.)

-- instance Functor ((->) e) where
--   fmap = (.)

-- :t (fmap length tail) :: [a] -> Int


data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
  fmap f (Map es) = Map (map (f <$>) es)


-- Functor laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g
