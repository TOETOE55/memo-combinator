module Memo (
    Memoizable(..),
    memoize2,
    memoize3,
    memoizeSecond
) where

import GHC.Arr
import Data.Int
import Data.Word
import Data.IntTrie


class Memoizable k where
    memoize :: (k -> v) -> (k -> v)

instance Memoizable () where
    memoize f = const (f ())

instance (Memoizable a, Memoizable b) => Memoizable (a, b) where
    memoize = uncurry . memoize2 . curry

instance (Memoizable a, Memoizable b, Memoizable c) => Memoizable (a, b, c) where
    memoize = uncurry3 . memoize3 . curry3 where
        curry3 f a b c = f (a, b, c)
        uncurry3 f (a, b, c) = f a b c

instance (Memoizable a, Memoizable b) => Memoizable (Either a b) where
    memoize f = either (memoize (f . Left)) (memoize (f . Right))

instance Memoizable a => Memoizable (Maybe a) where
    memoize f = table (f Nothing) (memoize (f . Just)) where
        table n j Nothing = n
        table n j (Just x) = j x

instance Memoizable a => Memoizable [a] where
    memoize f = table (f []) (memoize (\x -> memoize (f . (x:)))) where
        table nil cons []     = nil
        table nil cons (x:xs) = cons x xs

instance Memoizable Bool where
    memoize f = cond (f True) (f False) where
        cond t f True  = t
        cond t f False = f

instance Memoizable Int8 where
    memoize f = (array rg (zip rs (map f rs)) !) where
        rg = (-128, 127)
        rs = range rg

instance Memoizable Int16 where
    memoize f = apply (fmap f identity)

instance Memoizable Int32 where
    memoize f = apply (fmap f identity)

instance Memoizable Int64 where
    memoize f = apply (fmap f identity)

instance Memoizable Int where
    memoize f = apply (fmap f identity)

instance Memoizable Integer where
    memoize f = apply (fmap f identity)

instance Memoizable Word8 where
    memoize f = (array rg (zip rs (map f rs)) !) where
        rg = (0, 255)
        rs = range rg

instance Memoizable Word16 where
    memoize f = apply (fmap f identity)

instance Memoizable Word32 where
    memoize f = apply (fmap f identity)

instance Memoizable Word64 where
    memoize f = apply (fmap f identity)


memoize2 :: (Memoizable a, Memoizable b) => (a -> b -> v) -> (a -> b -> v)
memoize2 f = memoize (memoize . f)

memoizeSecond :: Memoizable b => (a -> b -> v) -> (a -> b -> v)
memoizeSecond = (memoize .)

memoize3 :: (Memoizable a, Memoizable b, Memoizable c) => (a -> b -> c -> v) -> (a -> b -> c -> v)
memoize3 f = memoize (memoize2 . f)

memoizeThird :: Memoizable c => (a -> b -> c -> v) -> (a -> b -> c -> v)
memoizeThird = (memoizeSecond .)