{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Backbone.Analysis.Hilbert
    ( Banach(..)
    , Hilbert(..)
    )
where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           Backbone.Algebra.Linear.Basis
import           Backbone.Algebra.Abstract.Module
import           Backbone.Algebra.Abstract.VectorSpace
import           Backbone.Algebra.Abstract.Tensor
import           Backbone.Analysis.Metric
import           Prelude                 hiding ( recip
                                                , Semigroup
                                                , (/)
                                                )
import           GHC.Types               hiding ( Module(..) )
import           Data.Coerce


---------------------------------------

-- | A Banach space is a Vector Space equipped with a compatible Norm and Metric.
--
-- See <http://en.wikipedia.org/wiki/Banach_space wikipedia> for more details.
class (VectorSpace a, Norm (GetNorm a) a (Scalar a)) => Banach a where
    type GetNorm a :: Type

    {-# INLINE normalize #-}
    normalize :: a -> a
    normalize v = v ./ (norm @(GetNorm a) v)

---------------------------------------

-- | Hilbert spaces are a natural generalization of Euclidean space that allows for infinite dimension.
--
-- See <http://en.wikipedia.org/wiki/Hilbert_space wikipedia> for more details.
--
-- FIXME:
-- The result of a dot product must always be an ordered field.
-- This is true even when the Hilbert space is over a non-ordered field like the complex numbers.
-- But the "OrdField" constraint currently prevents us from doing scalar multiplication on Complex Hilbert spaces.
-- See <http://math.stackexchange.com/questions/49348/inner-product-spaces-over-finite-fields> and <http://math.stackexchange.com/questions/47916/banach-spaces-over-fields-other-than-mathbbc> for some technical details.
class ( Banach v , TensorAlgebra v , Real (Scalar v) --,OrdField (Scalar v) ) 
                                                        ) => Hilbert v where
    infix 8 <.>
    (<.>) :: v -> v -> Scalar v

-- instance Hilbert Float    where (<>) = (*)
-- instance Hilbert Double   where (<>) = (*)

{-# INLINE squaredInnerProductNorm #-}
squaredInnerProductNorm :: Hilbert v => v -> Scalar v
squaredInnerProductNorm v = v <.> v

--FIXME correct functions
{-# INLINE innerProductNorm #-}
innerProductNorm :: Hilbert v => v -> Scalar v
innerProductNorm = undefined -- sqrt . squaredInnerProductNorm

{-# INLINE innerProductDistance #-}
innerProductDistance :: Hilbert v => v -> v -> Scalar v
innerProductDistance _ _ = undefined --innerProductNorm $ v1-v2
-- instance VectorSpace b => VectorSpace (a -> b) where g ./. f = \a -> g a ./. f a
