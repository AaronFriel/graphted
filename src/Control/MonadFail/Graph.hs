{- |
Module      :  Control.MonadFail.Graph
Description :  Graph indexed monads with failure.
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

This is only used in Do Notation with refutable patterns. e.g.:

@
    do  Just a <- m
        k a
@

Is desugared as:

@
    let f (Just a) = k a
        f _        = fail "Pattern match failure in do expression..."    
    in m >>= k
@

With @-XApplicativeDo@, there are two outstanding issues:

__First__, This will not compile (https://ghc.haskell.org/trac/ghc/ticket/13648) as
the body statements @m1@ and @m2@ are desugared incorrectly:

@
    f m1 m2 k = do
        m1
        m2
        k
@

To resolve, replace @m1@ with @_ <- m1@.

__Second__, @'fail'@ must be in scope (https://ghc.haskell.org/trac/ghc/ticket/13649)
when wildcard patterns are used. The module "Prelude.Graphted" takes care of
this, and custom preludes must as well. A @'GMonadFail'@ constraint will not
be added unless a refutable pattern is used.

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
