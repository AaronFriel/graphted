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
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

-- For the default Apply, Then, and But instances.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Applicative.Graph where

import Control.Graphted.Class

import Data.Functor.Graph
import Data.Pointed.Graph

-- | Graph indexed applicative functor.
class (GFunctor f, GPointed f) => GApplicative (f :: p -> * -> *) where

    -- | The apply operation ('<*>') on the graph index.
    --
    -- Default instance: @Apply f i j = 'Combine' f i j@ 
    type family Apply f (i :: p) (j :: p) :: p
    type instance Apply f i j = Combine f i j

    -- | The then operation ('*>') on the graph index.
    --
    -- Default instance: @'Then' f i j = 'Apply' f ('Fconst' f i) j@ 
    type family Then f (i :: p) (j :: p) :: p
    type instance Then f i j = Apply f (Fconst f i) j

    -- | The but operation ('<*') on the graph index.
    --
    -- Default instance: @But f i j = 'Apply' f ('Apply' f ('Pure' f) i) j@ 
    type family But f (i :: p) (j :: p) :: p
    type instance But f i j = Apply f (Apply f (Pure f) i) j

    -- | Sequential application ('<*>').
    gap :: Inv f i j => f i (a -> b) -> f j a -> f (Apply f i j) b

    -- | Sequence actions, discarding the value of the first argument ('*>').
    --
    -- Default implementation requires the default instance of 'Then'.
    {-# INLINE gthen #-}
    gthen :: Inv f i j => f i a -> f j b -> f (Then f i j) b
    default gthen :: (Apply f (Fconst f i) j ~ Then f i j, Inv f (Fconst f i) j)
                  => f i a -> f j b -> f (Then f i j) b
    gthen a b = (id `gconst` a) `gap` b

    -- | Sequence actions, discarding values of the second argument ('<*').
    --
    -- Default implementation requires the default instance of 'But'.
    {-# INLINE gbut #-}
    gbut :: Inv f i j => f i a -> f j b -> f (But f i j) a
    default gbut :: (Apply f (Apply f (Pure f) i) j ~ But f i j, Inv f (Pure f) i, Inv f (Apply f (Pure f) i) j)
                 => f i a -> f j b -> f (But f i j) a
    gbut a b = gpoint const `gap` a `gap` b

    {-# MINIMAL gap #-}
