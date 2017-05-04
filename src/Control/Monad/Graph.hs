{- |
Module      :  Control.Monad.Graph
Description :  Graph indexed monads
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

module Control.Monad.Graph where

import Control.Graphted.Class

import Control.Applicative.Graph

-- | Graph indexed monad.
class GApplicative m => GMonad (m :: p -> * -> *) where

    -- | The apply operation ('>>=') on the graph index.
    --
    -- Default instance: @Bind m i j = 'Combine' m i j@ 
    type family Bind m (i :: p) (j :: p) :: p
    type instance Bind m i j = Combine m i j

    -- | An invariant on the indexes of 'Bind'.
    --
    -- Default instance: @BindInv m i j = 'Inv' m i j@
    type family BindInv m (i :: p) (j :: p) :: Constraint
    type instance BindInv m i j = Inv m i j

    -- | The join operation ('Control.Monad.join') on the graph index.
    --
    -- Default instance: @Join m i j = 'Bind' m i j@ 
    type family Join m (i :: p) (j :: p) :: p
    type instance Join m i j = Bind m i j

    -- | An invariant on the indexes of 'Join'.
    --
    -- Default instance: @JoinInv m i j = 'BindInv' m i j@
    type family JoinInv m (i :: p) (j :: p) :: Constraint
    type instance JoinInv m i j = BindInv m i j

    -- | Sequentially compose two actions, with the second dependent on the first.
    gbind :: BindInv m i j => m i a -> (a -> m j b) -> m (Bind m i j) b

    -- | Remove one level of nested structure. 
    --
    -- Default implementation requires the default instance of 'Join'.
    {-# INLINE gjoin #-}
    gjoin :: (JoinInv m i j) => m i (m j b) -> m (Join m i j) b
    default gjoin :: (Bind m i j ~ Join m i j, BindInv m i j) => m i (m j b) -> m (Join m i j) b
    gjoin x = x `gbind` id
