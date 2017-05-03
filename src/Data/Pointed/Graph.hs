{- |
Module      :  Control.Applicative.Graph
Description :  Graph indexed applicative functors
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE CPP #-}

{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}

#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE TypeApplications     #-}
#endif

{-# LANGUAGE TypeFamilies         #-}
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
    -- >>> :t gpoint @_ @(GWrapped Maybe) "Hello, World"
    -- :: GWrapped Maybe () [Char]
    gpoint :: forall a. a -> f (Pure f) a

-- This implementation will only work with type applications.
#if __GLASGOW_HASKELL__ >= 801
    gpoint = gpoint' @p @f @(Pure f)

    -- | Return a pointed functor indexed by a type 't' in the domain of 'p'.
    --
    -- Accessible with type applications, e.g.:
    -- 
    -- >>> :t gpoint' @_ @(GWrapped Maybe) @Int
    -- gpoint' @_ @(GWrapped Maybe) @Int :: a -> GWrapped Maybe Int a
    -- 
    gpoint' :: forall t a. a -> f t a

    {-# MINIMAL gpoint' #-}
#endif
