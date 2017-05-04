{-# LANGUAGE CPP           #-}
{- |
Module      :  Data.GWrapped
Description :  Wrapped type constructors (typically Monad), graphted.
Copyright   :  (c) Aaron Friel
License     :  BSD-3
Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.GWrapped where

import Control.Graphted

import Control.Applicative (Alternative (..))
import Control.Monad       (MonadPlus (..))
-- import Data.Type.Equality (type (~~))

-- | Wrap a non-indexed type constructor:
newtype GWrapped (m :: * -> *) (p :: *) a = GWrapped { unG :: m a }

newtype Singleton i = Singleton i

-- | Lift an object to 'GWrapped'.
liftG :: m a -> GWrapped m p a
liftG = GWrapped

instance Graphted (GWrapped m) where
    type Unit (GWrapped m) = ()
    type Inv (GWrapped m) i j = i ~ j
    type Combine (GWrapped m) i j = i

instance Applicative f => GPointed (GWrapped f) where
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    type PureCxt (GWrapped f) i = ()
    gpure' = GWrapped . pure

    -- type Point (GWrapped f) i = Singleton i 
#else
    gpure = GWrapped . pure
#endif

instance Functor f => GFunctor (GWrapped f) where
    gmap f = GWrapped . fmap f . unG
    gconst f = GWrapped . ((<$) f) . unG

instance Applicative f => GApplicative (GWrapped f) where
    type But (GWrapped m) i j = i
    gap   (GWrapped m) (GWrapped k) = GWrapped $ m <*> k
    gthen (GWrapped m) (GWrapped k) = GWrapped $ m  *> k
    gbut  (GWrapped m) (GWrapped k) = GWrapped $ m <* k

instance Monad m => GMonad (GWrapped m) where
    gbind (GWrapped m) k = GWrapped $ m >>= unG . k
    gjoin (GWrapped m) = GWrapped $ m >>= unG

instance Monad m => GMonadFail (GWrapped m) where
    gfail = GWrapped . fail

instance MonadPlus m => GMonadZero (GWrapped m) where
    gzero = GWrapped $ mzero

instance MonadPlus m => GMonadPlus (GWrapped m) where
    gplus (GWrapped m) (GWrapped k) = GWrapped $ m `mplus` k

instance (Alternative m, MonadPlus m) => GMonadOr (GWrapped m) where
    gorelse (GWrapped m) (GWrapped k) = GWrapped $ m <|> k
