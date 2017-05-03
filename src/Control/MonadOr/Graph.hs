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

-- | Graph indexed monad with a monoidal operation satisfying the left catch law.
--
-- See the typeclassopedia <https://wiki.haskell.org/Typeclassopedia>.
class GMonadZero m => GMonadOr (m :: p -> * -> *) where
    
    -- | The or operation ('<|>') on the graph index.
    --
    -- Default instance: @Or m i j = 'Combine' m i j@ 
    type family Or m (i :: p) (j :: p) :: p
    type instance Or m i j = Combine m i j

    -- | An associative binary operation ('<|>').
    gorelse :: m i a -> m j a -> m (Or m i j) a
