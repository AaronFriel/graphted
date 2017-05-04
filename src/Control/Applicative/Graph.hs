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
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}

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

    -- | An invariant on the indexes of 'Apply'.
    --
    -- Default instance: @ApplyInv m i j = 'Inv' m i j@
    type family ApplyInv f (i :: p) (j :: p) :: Constraint
    type instance ApplyInv f i j = Inv f i j

    -- | The then operation ('*>') on the graph index.
    --
    -- Default instance: @'Then' f i j = 'Apply' f ('Fconst' f i) j@ 
    type family Then f (i :: p) (j :: p) :: p
    type instance Then f i j = Apply f (Fconst f i) j

    -- | An invariant on the indexes of 'Then'.
    --
    -- Default instance: @ThenInv m i j = 'ApplyInv' m i j@
    type family ThenInv f (i :: p) (j :: p) :: Constraint
    type instance ThenInv f i j = ApplyInv f i j

    -- | The but operation ('<*') on the graph index.
    --
    -- Default instance: @But f i j = 'Apply' f ('Apply' f ('Pure' f) i) j@ 
    type family But f (i :: p) (j :: p) :: p
    type instance But f i j = Apply f (Apply f (Pure f) i) j

    -- | An invariant on the indexes of 'But'.
    --
    -- Default instance: @ButInv m i j = 'ApplyInv' m i j@
    type family ButInv f (i :: p) (j :: p) :: Constraint
    type instance ButInv f i j = ApplyInv f i j

    -- | Sequential application ('<*>').
    gap :: ApplyInv f i j => f i (a -> b) -> f j a -> f (Apply f i j) b

    -- | Sequence actions, discarding the value of the first argument ('*>').
    --
    -- Default implementation requires the default instance of 'Then'.
    {-# INLINE gthen #-}
    gthen :: ThenInv f i j => f i a -> f j b -> f (Then f i j) b
    default gthen :: (Apply f (Fconst f i) j ~ Then f i j, ApplyInv f (Fconst f i) j, ThenInv f (Fconst f i) j)
                  => f i a -> f j b -> f (Then f i j) b
    gthen a b = (id `gconst` a) `gap` b

    -- | Sequence actions, discarding values of the second argument ('<*').
    --
    -- Default implementation requires the default instance of 'But'.
    {-# INLINE gbut #-}
    gbut :: ButInv f i j => f i a -> f j b -> f (But f i j) a
    default gbut :: (Apply f (Apply f (Pure f) i) j ~ But f i j, 
                    ApplyInv f (Pure f) i, 
                    ApplyInv f (Apply f (Pure f) i) j,
                    ButInv f (Apply f (Pure f) i) j)
                 => f i a -> f j b -> f (But f i j) a
    gbut a b = gpure const `gap` a `gap` b

    {-# MINIMAL gap #-}
