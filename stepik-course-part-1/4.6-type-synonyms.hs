import Data.Semigroup
import Prelude hiding (lookup)
import qualified Data.List as L

-- `newtype` is lazier than `data`.

-- newtype A = A a
-- newtype A a b = A a /ok
-- newtype A a b = A a b
-- newtype A = A A A
-- newtype A a = A
-- newtype A = A
-- newtype A = A A /ok
-- newtype A a = A a a
-- newtype A a b = A b /ok
-- newtype A a = A a /ok

-- Monoid has:
-- - neutral element (mempty)
-- - binary operation (mappend :: a -> a -> a)
-- - fold function (mconcat :: [a] -> a)


newtype Xor = Xor { getXor :: Bool }
    deriving (Eq, Show)

instance Semigroup Xor where
  a <> b = undefined

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor a) (Xor b) | a && not b = Xor True
                            | not a && b = Xor True
                            | otherwise = Xor False



newtype Box a = Box { getBox :: Maybe a }
    deriving (Eq, Show)

-- instance Monoid a => Monoid (Box a) where
--     mempty = Box (Just mempty)

--     mappend _ (Box (Nothing)) = Box Nothing
--     mappend (Box (Nothing)) _ = Box Nothing
--     mappend (Box x) (Box y) = Box (mappend x y)

-- Box Nothing `mappend` mempty ≡
-- ≡ mempty `mappend` Box Nothing ≡
-- ≡ Box Nothing

-- Assume Box Product:
-- Box Product => Maybe Product
-- The neutral element is Just Product 1 because product mempty is 1.
-- Just Product 1 `mappend` Nothing => Nothing
-- Nothing `mappend` Just Product 1 => Nothing
-- Just Product 1 `mappend` Just Product 2 => Just (Product 1 `mappend` Product 2) => Just (Product 1 * 2)


class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v

  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
  deriving (Eq, Show)


instance MapLike ListMap where
  empty = ListMap []

  lookup _ (ListMap []) = Nothing
  lookup k (ListMap ((key, val):xs)) | k == key = Just val
                                     | otherwise = lookup k (ListMap xs)

  insert k v listMap = ListMap (sort $ (insert' k v listMap)) where
    sort (ListMap xs) = L.sortBy (\(k1,_) (k2,_) -> k2 `compare` k1) xs

    insert' k v (ListMap []) = ListMap ((k,v):[])
    insert' k v (ListMap (pair@(key, val):xs))
      | k == key = ListMap ((k,v):xs)
      | otherwise = ListMap (pair : getListMap (insert k v (ListMap xs)))

  delete k (ListMap []) = ListMap []
  delete k (ListMap (pair@(key, val):xs))
    | k == key = ListMap xs
    | otherwise = ListMap (pair : getListMap (delete k (ListMap xs)))


-- `a -> a -> a` is an example of endomorphism.
-- `Endo` can be a `Monoid` where `id` is `mempty`
-- and function composition is `mappend`.

-- mconcat $  map Endo [(+2), (*5), (^2)] :: Num a -> Endo a
-- let k = appEndo . mconcat . map Endo
