{- |
Module      :  Control.MonadOr.Graph
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

module Control.MonadOr.Graph where

import Control.Graphted.Class

import Control.MonadZero.Graph

class GMonadZero m => GMonadOr (m :: p -> * -> *) where
    type family Or m (i :: p) (j :: p) :: p
    type instance Or m i j = Combine m i j

    gorelse :: m i a -> m j a -> m (Or m i j) a
