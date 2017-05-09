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

    -- | The 'liftA2' operation on the graph index.
    --
    -- Default instance: @Lift f i j = 'Apply' f ('Apply' f ('Pure' f) i) j@ 
    type family LiftA2 f (i :: p) (j :: p) :: p
    type instance LiftA2 f i j = Apply f (Fmap f i) j

    -- | An invariant on the indexes of 'But'.
    --
    -- Default instance: @ButInv m i j = 'ApplyInv' m i j@
    type family LiftA2Inv f (i :: p) (j :: p) :: Constraint
    type instance LiftA2Inv f i j = ApplyInv f i j

    -- | The then operation ('*>') on the graph index.
    --
    -- Default instance: @'Then' f i j = 'Apply' f ('Replace' f i) j@ 
    type family Then f (i :: p) (j :: p) :: p
    type instance Then f i j = Apply f (Replace f i) j

    -- | An invariant on the indexes of 'Then'.
    --
    -- Default instance: @ThenInv m i j = 'ApplyInv' m i j@
    type family ThenInv f (i :: p) (j :: p) :: Constraint
    type instance ThenInv f i j = ApplyInv f i j

    -- | The but operation ('<*') on the graph index.
    --
    -- Default instance: @But f i j = 'LiftA2' f i j@ 
    type family But f (i :: p) (j :: p) :: p
    type instance But f i j = LiftA2 f i j

    -- | An invariant on the indexes of 'But'.
    --
    -- Default instance: @ButInv m i j = 'ApplyInv' m i j@
    type family ButInv f (i :: p) (j :: p) :: Constraint
    type instance ButInv f i j = ApplyInv f i j

    -- | Sequential application ('<*>').
    gap :: ApplyInv f i j => f i (a -> b) -> f j a -> f (Apply f i j) b

    -- | Lift a binary function to actions.
    --
    gliftA2 :: LiftA2Inv f i j => (a -> b -> c) -> f i a -> f j b -> f (LiftA2 f i j) c 
    default gliftA2 :: (Apply f (Fmap f i) j ~ LiftA2 f i j, ApplyInv f (Fmap f i) j)
                    => (a -> b -> c) -> f i a -> f j b -> f (LiftA2 f i j) c
    gliftA2 f x = gap (gmap f x)

    -- | Sequence actions, discarding the value of the first argument ('*>').
    --
    -- Default implementation requires the default instance of 'Then'.
    {-# INLINE gthen #-}
    gthen :: ThenInv f i j => f i a -> f j b -> f (Then f i j) b
    default gthen :: (Apply f (Replace f i) j ~ Then f i j, ApplyInv f (Replace f i) j, ThenInv f (Replace f i) j)
                  => f i a -> f j b -> f (Then f i j) b
    gthen a b = (id `greplace` a) `gap` b

    -- | Sequence actions, discarding values of the second argument ('<*').
    --
    -- Default implementation requires the default instance of 'But'.
    {-# INLINE gbut #-}
    gbut :: ButInv f i j => f i a -> f j b -> f (But f i j) a
    default gbut :: (LiftA2 f i j ~ But f i j, LiftA2Inv f i j)
                 => f i a -> f j b -> f (But f i j) a
    gbut = gliftA2 const

    {-# MINIMAL gap #-}
