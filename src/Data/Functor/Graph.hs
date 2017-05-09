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

-- | Graph indexed functor.
class GFunctor (f :: p -> * -> *) where

    -- | The fmap operation ('fmap') on the graph index.
    --
    -- Default instance: @Fmap f i = i@
    type family Fmap f (i :: p) :: p
    type instance Fmap f i = i

    -- | The Replace operation ('<$') on the graph index.
    --
    -- Default instance: @Replace f i = 'Fmap' f i@ 
    type family Replace f (i :: p) :: p
    type instance Replace f i = Fmap f i

    -- | Map a function over over the functor ('fmap').
    gmap :: (a -> b) -> f i a -> f (Fmap f i) b

    -- | Replace all values with a constant ('<$').
    --
    -- Default implementation requires the default instance of 'Replace'.
    {-# INLINABLE greplace #-}
    greplace :: a -> f i b -> f (Replace f i) a
    default greplace :: (Replace f i ~ Fmap f i) => a -> f i b -> f (Replace f i) a
    greplace = gmap . const
