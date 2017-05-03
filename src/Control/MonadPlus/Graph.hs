{- |
Module      :  Control.MonadPlus.Graph
Description :  Graph indexed monads with choice and failure
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

module Control.MonadPlus.Graph where

import Control.Graphted.Class

import Control.MonadZero.Graph

class GMonadZero m => GMonadPlus (m :: p -> * -> *) where
    type family Plus m (i :: p) (j :: p) :: p
    type instance Plus m i j = Combine m i j

    gplus :: m i a -> m j a -> m (Plus m i j) a
