{-# LANGUAGE CPP                   #-}
{- |
Module      :  Control.Applicative.Graph
Description :  Graph indexed applicative functors
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeApplications      #-}
#endif

-- For the default type instances.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Applicative.Graph where

import Control.Graphted.Class

import Data.Functor.Graph
import Data.Pointed.Graph

#if !MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
import Data.Proxy
#endif

type family DefaultThen (useReplace :: Bool) (f :: p -> * -> *) (i :: p) (j :: p) where
    DefaultThen 'True  f i j = Apply f (Replace f i) j
    DefaultThen 'False f i j = LiftA2 f i j

type family DefaultThenCxt (useReplace :: Bool) (f :: p -> * -> *) (i :: p) (j :: p) where
    DefaultThenCxt 'True  f i j = (Apply f (Replace f i) j ~ Then f i j, ApplyInv f (Replace f i) j)
    DefaultThenCxt 'False f i j = (LiftA2 f i j ~ Then f i j,            LiftA2Inv f i j)

class GApplicativeThen useReplace (f :: p -> * -> *) where
    gdefaultThenProxy :: DefaultThenCxt useReplace f i j => proxy useReplace -> f i a -> f j b -> f (Then f i j) b
    gdefaultThen :: DefaultThenCxt useReplace f i j => f i a -> f j b -> f (Then f i j) b

instance GApplicative f => GApplicativeThen 'True f where
    {-# INLINE gdefaultThenProxy #-}
    gdefaultThenProxy _ a b = (id `greplace` a) `gap` b
    {-# INLINE gdefaultThen #-}
    gdefaultThen a b = (id `greplace` a) `gap` b

instance GApplicative f => GApplicativeThen 'False f where
    {-# INLINE gdefaultThenProxy #-}
    gdefaultThenProxy _ a b = gliftA2 (flip const) a b
    {-# INLINE gdefaultThen #-}
    gdefaultThen a b = gliftA2 (flip const) a b

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
    -- Default instance: @LiftA2 f i j = 'Apply' f ('Fmap' f i) j@
    type family LiftA2 f (i :: p) (j :: p) :: p
    type instance LiftA2 f i j = Apply f (Fmap f i) j

    -- | An invariant on the indexes of 'But'.
    --
    -- Default instance: @LiftA2Inv m i j = 'ApplyInv' m i j@
    type family LiftA2Inv f (i :: p) (j :: p) :: Constraint
    type instance LiftA2Inv f i j = ApplyInv f i j

    -- | Whether to use 'gliftA2', or 'gap' and 'greplace' in the definition
    -- of 'gthen'.
    --
    -- If an efficient 'Replace' exists, we should probably use that to reduce
    -- allocations. But liftA2 might also be appropriate.
    type family ThenUseReplace f :: Bool
    type instance ThenUseReplace f = EfficientReplace f

    -- | The then operation ('*>') on the graph index.
    --
    -- Default instance depends on @'ThenUseReplace' f@:
    --
    -- * 'True': @'Then' f i j = 'Apply' f ('Replace' f i) j@
    -- * 'False': @'Then' f i j = 'LiftA2' f i j@
    type family Then f (i :: p) (j :: p) :: p
    type instance Then f i j = DefaultThen (ThenUseReplace f) f i j

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
    -- Default implementation is defined in terms of 'Apply' and 'Fmap'.
    gliftA2 :: LiftA2Inv f i j => (a -> b -> c) -> f i a -> f j b -> f (LiftA2 f i j) c
    default gliftA2 :: (Apply f (Fmap f i) j ~ LiftA2 f i j, ApplyInv f (Fmap f i) j)
                    => (a -> b -> c) -> f i a -> f j b -> f (LiftA2 f i j) c
    gliftA2 f x = gap (gmap f x)

    -- | Sequence actions, discarding the value of the first argument ('*>').
    --
    -- Default implementation requires the default instance of 'Then'.
    {-# INLINE gthen #-}
    gthen :: ThenInv f i j => f i a -> f j b -> f (Then f i j) b
    default gthen :: (GApplicativeThen (ThenUseReplace f) f, DefaultThenCxt (ThenUseReplace f) f i j)
                   => f i a -> f j b -> f (Then f i j) b
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    gthen a b = gdefaultThen @(ThenUseReplace f) a b
#else
    gthen a b = gdefaultThenProxy (Proxy :: Proxy (ThenUseReplace f)) a b
#endif

    -- | Sequence actions, discarding values of the second argument ('<*').
    --
    -- Default implementation requires the default instance of 'But'.
    {-# INLINE gbut #-}
    gbut :: ButInv f i j => f i a -> f j b -> f (But f i j) a
    default gbut :: (LiftA2 f i j ~ But f i j, LiftA2Inv f i j)
                 => f i a -> f j b -> f (But f i j) a
    gbut = gliftA2 const

    {-# MINIMAL gap #-}
