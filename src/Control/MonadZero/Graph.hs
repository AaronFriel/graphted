{- |
Module      :  Control.MonadZero.Graph
Description :  Graph indexed monads with failure
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

module Control.MonadZero.Graph where

import Control.Graphted.Class

import Control.Monad.Graph

class GMonad m => GMonadZero (m :: p -> * -> *) where
    type family Zero m :: p
    type instance Zero m = Unit m

    gzero :: m (Zero m) a
