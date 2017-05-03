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

-- | Graph indexed monad with failure.
class GMonad m => GMonadFail (m :: p -> * -> *) where
    -- | The unit failure element of the index.
    --
    -- Default instance: @Fail m = 'Unit' m@
    type family Fail m :: p
    type instance Fail m = Unit m

    -- | Fail with a message.
    --
    -- Default implementation requires the default instance of 'Fail'.
    {-# INLINE gfail #-}
    gfail :: String -> m (Fail m) a
    default gfail :: (GMonadZero m, Zero m ~ Fail m) => String -> m (Fail m) a
    gfail _ = gzero
