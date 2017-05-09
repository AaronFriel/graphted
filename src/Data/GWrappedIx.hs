{-# LANGUAGE CPP                  #-}

{- |
Module      :  Data.WrappedIxIx
Description :  Wrapped type constructors (typically Monad), graphted.
Copyright   :  (c) Aaron Friel
License     :  BSD-3
Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GWrappedIx where

import Control.Graphted
import Control.Monad.Indexed
import Data.Functor.Indexed

import GHC.Exts (Any)

-- | Wrap a two-parameter-indexed type constructor:
newtype WrappedIx (m :: * -> * -> * -> *) (p :: (*, *)) a = WrappedIx { unIx :: m (FstIx p) (SndIx p) a }

type family FstIx p :: * where
    FstIx '(i, j) = i

type family SndIx p :: * where
    SndIx '(i, j) = j

instance Graphted (WrappedIx m) where
    -- Hackish?
    -- Ideally we would separate out 'Unit' and 'Pure', such that:
    -- 
    -- point :: GPointed f => forall t a. a -> f (Pure f t) a
    --
    -- And with impredicative types:
    --
    -- But f i j = Apply f (Apply f (forall t. Pure f t) i) j
    type Unit (WrappedIx m) = '(Any, Any)

    type Inv (WrappedIx m) i j = SndIx i ~ FstIx j

    type Combine (WrappedIx m) i j = '( FstIx i, SndIx j )

-- | Lift an object to 'WrappedIx'.
liftIx :: m i j a -> WrappedIx m '(i, j) a
liftIx = WrappedIx

instance IxPointed f => GPointed (WrappedIx f) where
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    type PureCxt (WrappedIx f) i = FstIx i ~ SndIx i
    gpure' = WrappedIx . ireturn

    -- type Point (WrappedIx f) i = '(i, i)
#else
    gpure = WrappedIx . ireturn
#endif

instance IxFunctor f => GFunctor (WrappedIx f) where
    gmap f = WrappedIx . imap f . unIx
    greplace a = WrappedIx . imap (const a) . unIx

instance IxApplicative f => GApplicative (WrappedIx f) where
    type But (WrappedIx f) l r = '( FstIx l, SndIx r )
    gap   (WrappedIx m) (WrappedIx k) = WrappedIx $ m <<*>> k
    gthen (WrappedIx m) (WrappedIx k) = WrappedIx $ m *>> k
    gbut (WrappedIx m) (WrappedIx k) = WrappedIx $ m <<* k

instance IxMonad m => GMonad (WrappedIx m) where
    gbind (WrappedIx m) k = WrappedIx $ m >>>= unIx . k
    gjoin (WrappedIx m) = WrappedIx $ m >>>= unIx

instance IxMonadZero m => GMonadFail (WrappedIx m) where
    gfail _ = WrappedIx imzero

instance IxMonadZero m => GMonadZero (WrappedIx m) where
    gzero = WrappedIx imzero

instance IxMonadPlus m => GMonadPlus (WrappedIx m) where
    type PlusInv (WrappedIx m) l r = ( FstIx l ~ FstIx r, SndIx l ~ SndIx r )
    gplus (WrappedIx m) (WrappedIx k) = WrappedIx $ m `implus` k
