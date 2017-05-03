{- |
Module      :  Control.Applicative.Graph
Description :  Graph indexed applicative functors
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Pointed.Graph where

import Control.Graphted.Class

-- | Pointed functor.
class GPointed (f :: p -> * -> *) where
    type family Pure f :: p
    type instance Pure f = Unit f

    -- | Accessible only with type applications.
    gpoint' :: forall t a. a -> f t a

    gpoint :: forall a. a -> f (Pure f) a
    gpoint = gpoint' @p @f @(Pure f)

    {-# MINIMAL gpoint' #-}
