{- |
Module      :  Control.Graph.Base
Description :  Base type class for graph indexed types.
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE CPP #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies    #-}

module Control.Graphted.Class where

#if __GLASGOW_HASKELL__ >= 801
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

-- | Base class that all Graph-indexed types may implement.
--
class Graphted (f :: p -> * -> *) where
    -- | The unit of our kind p.
    type Unit f :: p

    -- | An invariant on combining indexes.
    type family Inv f (i :: p) (j :: p) :: Constraint
    type instance Inv f i j = ()

    -- | An elementary composition of indexes.
    --
    -- N.B.: This may be nonsensical if and only if type classes override the
    -- default definitions of their own type families.
    --
    -- This exists for convenience.
    type family Combine f (i :: p) (j :: p) :: p
