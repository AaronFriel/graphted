{- |
Module      :  Control.Applicative.Graph
Description :  Graph indexed applicative functors
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Polypinds             #-}
{-# LANGUAGE TypeFamilies          #-}

-- For the default Apply, Then, and But instances.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Applicative.Graph where

import Control.Graphted.Class

import Data.Functor.Graph
import Data.Pointed.Graph

class (GFunctor f, GPointed f) => GApplicative (f :: p -> * -> *) where
    type family Apply f (i :: p) (j :: p) :: p
    type instance Apply f i j = Combine f i j

    type family Then f (i :: p) (j :: p) :: p
    type instance Then f i j = Apply f (Fconst f i) j

    type family But f (i :: p) (j :: p) :: p
    type instance But f i j = Apply f (Apply f (Pure f) i) j

    -- <*>
    gap :: Inv f i j => f i (a -> b) -> f j a -> f (Apply f i j) b

    -- *>
    {-# INLINE gthen #-}
    gthen :: Inv f i j => f i a -> f j b -> f (Then f i j) b
    default gthen :: (Apply f (Fconst f i) j ~ Then f i j, Inv f (Fconst f i) j)
                  => f i a -> f j b -> f (Then f i j) b
    gthen a b = (id `gconst` a) `gap` b

    -- <*
    {-# INLINE gbut #-}
    gbut :: Inv f i j => f i a -> f j b -> f (But f i j) a
    default gbut :: (Apply f (Apply f (Pure f) i) j ~ But f i j, Inv f (Pure f) i, Inv f (Apply f (Pure f) i) j)
                 => f i a -> f j b -> f (But f i j) a
    gbut a b = gpoint const `gap` a `gap` b
