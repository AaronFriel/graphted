{-# LANGUAGE CPP                  #-}
{- |
Module      :  Control.Applicative.Graph
Description :  Graph indexed applicative functors
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
#endif

{-# LANGUAGE UndecidableInstances #-}

module Data.Pointed.Graph where

import Control.Graphted.Class


-- | Graph indexed pointed functor.
class GPointed (f :: p -> * -> *) where

    -- | The pure element of the graph index.
    type family Pure f :: p
    type instance Pure f = Unit f

    -- | Return a pointed functor indexed by the 'Pure' type instance ('pure').
    --
    -- >>> :t gpure @_ @(GWrapped Maybe) "Hello, World"
    -- :: GWrapped Maybe () [Char]
    gpure :: forall a. a -> f (Pure f) a

-- This implementation will only work with type applications.
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    default gpure :: PureCxt f (Pure f) => a -> f (Pure f) a
    gpure = gpureAt @(Pure f)

    -- | A constraint on generating a pure index from a free variable. Default is empty.
    type family PureCxt f (i :: p) :: Constraint
    type instance PureCxt f i = ()


    -- | Return a pointed functor indexed by a type 't' in the domain of 'p'.
    --
    -- Accessible with type applications, e.g.:
    --
    -- >>> :t gpure' @_ @(GWrapped Maybe) @Int
    -- gpure' @_ @(GWrapped Maybe) @Int :: a -> GWrapped Maybe Int a
    --
    gpure' :: forall t a. PureCxt f t => a -> f t a

    {-# MINIMAL gpure' #-}

{-
    -- | A point element of the graph index.
    type family Point f t = (i :: p) | i -> t
    type instance Point f t = Unit f

    gpoint :: forall a. a -> f (forall t. Point f t) a
-}
#endif

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
-- | Return a pointed functor with a given type in the index.
--
-- Accessible with type applications, e.g.:
-- 
-- >>> :t gpureAt @Int 
-- gpureAt @Int :: (GPointed f, PureCxt f Int) => a -> f Int a
gpureAt :: forall t a f. (GPointed f, PureCxt f t) => a -> f t a
gpureAt = gpure' @_ @_ @t
#endif
