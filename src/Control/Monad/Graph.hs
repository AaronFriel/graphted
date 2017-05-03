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

class GApplicative m => GMonad (m :: p -> * -> *) where
    type family Bind m (i :: p) (j :: p) :: p
    type instance Bind m i j = Combine m i j

    type family Join m (i :: p) (j :: p) :: p
    type instance Join m i j = Bind m i j

    gbind :: Inv m i j => m i a -> (a -> m j b) -> m (Bind m i j) b

    {-# INLINE gjoin #-}
    gjoin :: (Inv m i j) => m i (m j b) -> m (Join m i j) b
    default gjoin :: (Bind m i j ~ Join m i j, Inv m i j) => m i (m j b) -> m (Join m i j) b
    gjoin x = x `gbind` id
