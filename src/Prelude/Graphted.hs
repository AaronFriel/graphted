{-# LANGUAGE CPP                   #-}
{- |
Module      :  Prelude.Graphted
Description :  Prelude with operators overridden by graph-indexed implementations
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  experimental
Portability :  portable

-}

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
{-# LANGUAGE ApplicativeDo         #-}
#endif

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}

module Prelude.Graphted (
    -- Functor
    fmap, (<$), (<$>),
    -- Applicative
    pure, (<*>), (*>), (<*),
    -- Monad
    return, (>>=), (=<<), (>>),
    -- MonadFail
    fail,
    -- MonadPlus, MonadOr
    zero, (<+>), (<|>),

    -- Extra operators:
    (<**>), liftA, liftA2, liftA3,
    join, liftM, liftM2, liftM3, liftM4, liftM5, ap,

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    mapM_, sequence_,
#endif

    module X
    ) where


import Prelude as X hiding (fail, fmap, mapM_, pure, return, sequence_, (*>),
                     (<$), (<$>), (<*), (<*>), (=<<), (>>), (>>=))

import Control.Graphted

infixl 4 <$
infixl 1 >>, >>=
infixr 1 =<<
infixl 4 <*>, <*, *>, <**>

fmap :: GFunctor f => (a -> b) -> f i a -> f (Fmap f i) b
fmap = gmap

(<$) :: GFunctor f => b -> f i a -> f (Replace f i) b
(<$) = greplace

(<$>) :: GFunctor f => (a -> b) -> f i a -> f (Fmap f i) b
(<$>) = fmap

pure :: GPointed f => a -> f (Pure f) a
pure = gpure

(<*>) :: (GApplicative f, ApplyInv f i j) => f i (a -> b) -> f j a -> f (Apply f i j) b
(<*>) = gap

(*>) :: (GApplicative f, ThenInv f i j) => f i a -> f j b -> f (Then f i j) b
(*>)= gthen

(<*) :: (GApplicative f, ButInv f i j) => f i a -> f j b -> f (But f i j) a
(<*) = gbut

return :: GPointed m => a -> m (Pure m) a
return = gpure

(>>=) :: (GMonad m, BindInv m i j) => m i a -> (a -> m j b) -> m (Bind m i j) b
(>>=) = gbind

(=<<) :: (GMonad m, BindInv m i j) => (a -> m j b) -> m i a -> m (Bind m i j) b
(=<<) = flip (>>=)

zero :: GMonadZero m => m (Zero m) a
zero = gzero

fail :: GMonadFail m => String -> m (Fail m) a
fail = gfail

(<+>) :: (GMonadPlus f, PlusInv f i j) => f i a -> f j a -> f (Plus f i j) a
(<+>) = gplus

(<|>) :: (GMonadOr f, OrInv f i j) => f i a -> f j a -> f (Or f i j) a
(<|>) = gorelse

-- Simplified binding, what GHC.Base would like to do but cannot for backwards compatbility.
(>>) :: (GApplicative m, ThenInv m i j) => m i a -> m j b -> m (Then m i j) b
(>>) = gthen

join :: (GMonad m, JoinInv m i j) => m i (m j b) -> m (Join m i j) b
join = gjoin

(<**>) :: (GApplicative f, _) => f i1 a -> f i2 (a -> b)
       -> f (Apply f (Apply f (Pure f) i1) i2) b
a <**> b = pure (flip ($)) <*> a <*> b

liftA :: (GApplicative f, _) => (a -> b) -> f i1 a
      -> f (Apply f (Pure f) i1) b
liftA f a = pure f <*> a

liftA2 :: (GApplicative f, _) => (a1 -> a2 -> b) -> f i1 a1 -> f i2 a2
       -> f (Apply f (Apply f (Pure f) i1) i2) b
liftA2 f a b = pure f <*> a <*> b

liftA3 :: (GApplicative f, _) => (a1 -> a2 -> a3 -> b) -> f i1 a1 -> f i2 a2 -> f i3 a3
       -> f (Apply f (Apply f (Apply f (Pure f) i1) i2) i3) b
liftA3 f a b c = pure f <*> a <*> b <*> c

liftM :: (GApplicative m, _) => (t -> b) -> m j t -> m (Fmap m j) b
liftM f m1              = do { x1 <- m1; return (f x1) }

liftM2 :: (GApplicative m, _)
       => (t1 -> t -> b)
       -> m i1 t1
       -> m i t
       -> m (Apply m (Fmap m i1) i) b
liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

liftM3 :: (GApplicative m, _)
       => (t2 -> t1 -> t -> b)
       -> m i2 t2
       -> m i1 t1
       -> m i t
       -> m (Apply m (Apply m (Fmap m i2) i1) i) b
liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

liftM4 :: (GApplicative m, _)
       => (t3 -> t2 -> t1 -> t -> b)
       -> m i3 t3
       -> m i2 t2
       -> m i1 t1
       -> m i t
       -> m (Apply m (Apply m (Apply m (Fmap m i3) i2) i1) i) b
liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

liftM5 :: (GApplicative m, _)
       => (t4 -> t3 -> t2 -> t1 -> t -> b)
       -> m i4 t4
       -> m i3 t3
       -> m i2 t2
       -> m i1 t1
       -> m i t
       -> m (Apply m (Apply m (Apply m (Apply m (Fmap m i4) i3) i2) i1) i) b
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
ap :: (GApplicative m, _) => m i (t -> b) -> m j t -> m (Apply m (Fmap m i) j) b
ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
#else
-- ap :: (GApplicative m, Inv m (Fmap m i) j) => m i (t -> b) -> m j t -> m (Apply m (Fmap m i) j) b
ap m1 m2 = liftM m1 m2
#endif

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
-- Recursive bindings may be impossible. This type is inferred, but not always satisfiable.
-- We will need to implement our own folds and control flow.
mapM_ :: (GApplicative m, Foldable t, Apply m (Fmap m i) (Pure m) ~ Pure m, _)
      => (a1 -> m i a) -> t a1 -> m (Pure m) ()
mapM_ f = foldr ((>>) . f) (return ())

-- As above.
sequence_ :: (GApplicative m, Foldable t, Apply m (Fmap m i) (Pure m) ~ Pure m, _)
          => t (m i a) -> m (Pure m) ()
sequence_ = foldr (>>) (return ())
#endif
