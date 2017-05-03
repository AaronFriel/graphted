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

-- | Graph indexed monad with a monoidal zero.
--
-- See the typeclassopedia <https://wiki.haskell.org/Typeclassopedia>.
class GMonad m => GMonadZero (m :: p -> * -> *) where

    -- | The zero element ('mzero', 'mempty') of the graph index.
    type family Zero m :: p
    type instance Zero m = Unit m

    -- | Identity element.
    gzero :: m (Zero m) a
