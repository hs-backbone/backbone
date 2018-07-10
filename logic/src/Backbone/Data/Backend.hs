{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Backbone.Data.Backend
    ( BMonad(..)
    , BInt(..)
    , BBool(..)
    )
where

import qualified Prelude as P
import Data.Functor.Identity (Identity)
import GHC.Types hiding (Module, type (*))

data Prelude = Prelude

-- | the Monad used to get computations back into the haskell-universe
type family BMonad rep :: Type -> Type

type instance BMonad Prelude = Identity

-- | the representation of Natural-Numbers in this universe
type family BInt rep :: Type
type instance BInt Prelude = P.Int

-- | the representation of Booleans in this universe
type family BBool rep :: Type
type instance BBool Prelude = P.Bool