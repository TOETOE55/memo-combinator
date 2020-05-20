{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE UndecidableInstances#-}

module Memo (
    Memoizable(..),
    memoize2,
    -- memoize3
) where

import Data.Bits
import Data.IntTrie


class Memoizable k v where
    memoize :: (k -> v) -> (k -> v)

instance Memoizable () v where
    memoize f = const (f ())

instance (Memoizable a (b -> v), Memoizable b v) => Memoizable (a, b) v where
    memoize = uncurry . memoize2 . curry

instance 
    ( Memoizable a (b -> c -> v)
    , Memoizable b (c -> v)
    , Memoizable c v)
    => Memoizable (a, b, c) v where
    memoize = uncurry3 . memoize3 . curry3 where
        curry3 f a b c = f (a, b, c)
        uncurry3 f (a, b, c) = f a b c

instance (Memoizable a v, Memoizable b v) => Memoizable (Either a b) v where
    memoize f = either (memoize (f . Left)) (memoize (f . Right))

instance Memoizable a v => Memoizable (Maybe a) v where
    memoize f = table (f Nothing) (memoize (f . Just)) where
        table n j Nothing = n
        table n j (Just x) = j x

instance {-# Overlapping #-} Memoizable a ([a] -> v) => Memoizable [a] v where
    memoize f = table (f []) (memoize (\x -> memoize (f . (x:)))) where
        table nil cons []     = nil
        table nil cons (x:xs) = cons x xs

instance Memoizable Bool v where
    memoize f = cond (f True) (f False) where
        cond t f True  = t
        cond t f False = f

-- instance Memoizable Int8 v where
--     memoize f = (array rg (zip rs (map f rs)) !) where
--         rg = (-128, 127)
--         rs = range rg


-- instance Memoizable Word8 v where
--     memoize f = (array rg (zip rs (map f rs)) !) where
--         rg = (0, 255)
--         rs = range rg

instance (Ord a, Num a, Bits a) => Memoizable a v where
    memoize f = apply (fmap f identity)


memoize2 :: (Memoizable a (b -> v), Memoizable b v) => (a -> b -> v) -> (a -> b -> v)
memoize2 f = memoize (memoize . f)


memoize3 :: 
    ( Memoizable a (b -> c -> v)
    , Memoizable b (c -> v)
    , Memoizable c v) 
    => (a -> b -> c -> v) -> (a -> b -> c -> v)
memoize3 f = memoize (memoize2 . f)
