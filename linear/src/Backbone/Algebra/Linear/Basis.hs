{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operations
module Backbone.Algebra.Linear.Basis
    ( ExtractBasis(..)
    , Basis(..)
    , FiniteBasis(..)
    , BasisCompat(..)
    , getDim'
    )
where

import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           NumHask.Algebra.Natural
import           GHC.Types

-- extract the basis from the type
type family ExtractBasis (x :: m) :: Type

class SingKind (KindOf a) => Basis a b where
    --the type after the koordinates got projected through the basis
    type Projection a b :: Type
    type Projection a b = b

    -- projects the coordinates through the basis
    project :: Demote (KindOf a) -> b -> Projection a b

-- a finite basis
class Basis a b => FiniteBasis a b where
    type Dim a :: Nat

    getDim :: Natural n => Demote (KindOf a) -> n

getDim'
    :: forall (a :: k) b n . (SingI a, FiniteBasis (a :: k) b, Natural n) => n
getDim' = getDim @a @b (demote @a)

--class to interact with explicit basises
class SingKind (ElementTypeLevelRep a) => BasisCompat a where
    type ElementTypeLevelRep a :: Type
    toBasis :: a -> Demote (ElementTypeLevelRep a)
    fromBasis :: Demote (ElementTypeLevelRep a) -> a
