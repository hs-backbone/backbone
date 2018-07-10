{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Backbone.Analysis.Metric
    ( ExtractMetric(..)
    , PreMetric(..)
    , Dual(..)
    , Metric(..)
    , Norm(..)
    , Abs(..)
    )
where

import           NumHask.Analysis.Metric       as NH
                                         hiding ( Metric )
import           NumHask.Algebra.Abstract.Additive
import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Multiplicative
import           GHC.Types

-- has this type a Metric?
type family ExtractMetric (x :: m) :: Type

-- | the PreMetric
-- laws: https://en.wikipedia.org/wiki/Metric_(mathematics)
-- very important for statisticans (https://en.wikipedia.org/wiki/Divergence_(statistics))
-- theoretically b >= 0, but I don't know how to capture this...
class PreMetric n a b | n a -> b where
    preMetric :: a -> a -> b

newtype Dual a = Dual a

instance PreMetric n a b => PreMetric (Dual n) a b where
    {-# INLINE preMetric #-}
    preMetric a b = preMetric @n b a

-- | the Metric 
-- laws: https://en.wikipedia.org/wiki/Metric_(mathematics)
class PreMetric n a b => Metric n a b | n a -> b

{-# INLINE metric #-}
metric :: forall n a b . Metric n a b => a -> a -> b
metric = preMetric @n

-- | the Metric 
-- laws: https://en.wikipedia.org/wiki/Metric_(mathematics)
class (Metric n a b, Group (Sum a), Multiplication b) => Norm n a b | n a -> b where
    {-# INLINE norm #-}
    norm :: a -> b
    norm a = metric @n a zero

    {-# INLINE normSquared #-}
    normSquared :: a -> b
    normSquared x = n * n
        where
            n = norm @n x


data Abs

instance (Invertible (Sum a), Addition a, Signed a) => PreMetric Abs a a where
    {-# INLINE preMetric #-}
    preMetric a b = abs (a - b)

instance (Invertible (Sum a), Addition a, Signed a) => Metric Abs a a

instance (CommutativeRing a, Signed a) => Norm Abs a a where
    norm = abs
