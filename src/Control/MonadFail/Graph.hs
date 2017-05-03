{- |
Module      :  Control.MonadFail.Graph
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

module Control.MonadFail.Graph where

import Control.Graphted.Class

import Control.Monad.Graph
import Control.MonadZero.Graph

class GMonad m => GMonadFail (m :: p -> * -> *) where
    type family Fail m :: p
    type instance Fail m = Unit m

    gfail :: String -> m (Fail m) a

    default gfail :: (GMonadZero m, Zero m ~ Fail m) => String -> m (Fail m) a
    gfail _ = gzero
