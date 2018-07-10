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
module Backbone.Algebra.Abstract.Tensor
    ( TensorAlgebra(..)
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
import           Backbone.Analysis.Metric
import           Backbone.Data.Representation
import           Prelude                 hiding ( recip
                                                , Semigroup
                                                , (/)
                                                )
import           GHC.Types               hiding ( Module(..) )
import           Data.Coerce
---------------------------------------

-- | Tensor algebras generalize the outer product of vectors to construct a matrix.
--
-- See <https://en.wikipedia.org/wiki/Tensor_algebra wikipedia> for details.
--
-- FIXME:
-- This needs to be replaced by the Tensor product in the Monoidal category Vect
class
    ( cont ~ (ElCont rep a)
    , VectorSpace (f (Rep cont m))
    , VectorSpace (f' (Rep cont m'))
    , Scalar (f (Rep cont m)) ~ Scalar (f' (Rep cont m'))
    , VectorSpace ((f (Rep cont m))><(f' (Rep cont m')))
    , Scalar ((f rep)><(f' rep')) ~ Scalar (f (Rep cont m))
    , Field ((f rep)><(f' rep')) -- FIXME: should not be needed!
    ) => TensorAlgebra rep a
        where

    -- | Take the tensor product of two vectors
    (><) :: f (Rep cont m) -> f' (Rep cont m') -> ((f rep)><(f' rep'))

-- instance TensorAlgebra Float    where  (><) = (*); vXm = (*);  mXv = (*)
-- instance TensorAlgebra Double   where  (><) = (*); vXm = (*);  mXv = (*)
-- instance TensorAlgebra Rational where  (><) = (*); vXm = (*);  mXv = (*)

---------------------------------------

{-
-- | Bregman divergences generalize the squared Euclidean distance and the KL-divergence.
-- They are closely related to exponential family distributions.
--
-- Mark Reid has a <http://mark.reid.name/blog/meet-the-bregman-divergences.html good tutorial>.
--
-- FIXME:
-- The definition of divergence requires taking the derivative.
-- How should this relate to categories?
class
    ( Hilbert v
    ) => Bregman v
        where
    divergence :: v -> v -> Scalar v
    divergence v1 v2 = f v1 - f v2 - (derivative f v2 <> v1 - v2)
        where
            f = bregmanFunction
    bregmanFunction :: v -> Scalar v
law_Bregman_nonnegativity :: v -> v -> Logic v
law_Bregman_nonnegativity v1 v2 = divergence v1 v2 > 0
law_Bregman_triangle ::
-}
