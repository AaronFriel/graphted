{- |
Module      :  Data.Functor.Graph
Description :  Graph indexed functors
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Graph where

class GFunctor (f :: p -> * -> *) where
    type family Fmap f (i :: p) :: p
    type instance Fmap f i = i

    type family Fconst f (i :: p) :: p
    type instance Fconst f i = Fmap f i

    gmap :: (a -> b) -> f i a -> f (Fmap f i) b

    {-# INLINABLE gconst #-}
    gconst :: a -> f i b -> f (Fconst f i) a
    default gconst :: (Fconst f i ~ Fmap f i) => a -> f i b -> f (Fconst f i) a
    gconst = gmap . const
