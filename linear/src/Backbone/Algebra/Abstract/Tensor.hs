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
module Backbone.Algebra.Abstract.Tensor
    ( --VecSpace(..)
     TensorAlgebra(..)
    , ContravatiantTensor(..)
    , dot
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
-- newtype VecSpace a = VecSpace a

class (Field (Scalar f), VectorSpace f, VectorSpace f', VectorSpace (f >< f')
        , Scalar f ~ Scalar f'
        , Scalar f ~ Scalar (f >< f')
        ) => TensorAlgebra f f'
        where
    -- | Take the tensor product of two vectors
    (><) :: f -> f' -> (f >< f')

-- instance TensorAlgebra Float    where  (><) = (*); 
-- instance TensorAlgebra Double   where  (><) = (*);
-- instance TensorAlgebra Rational where  (><) = (*);

type family CDot (m :: [d]) (m' :: [d]) :: [d] where
    CDot a '[] = a
    CDot '[] a = '[]
    CDot (a : as) (a : as') = CDot as as'
    CDot (a : as) (a': as') = a : CDot as (a' : as')

--FIXME: Does this even work?
-- type family CDotInjectiveLeft (res :: [d]) (r :: [d]) :: [d] where
--     CDotInjectiveLeft r r = '[]
--     CDotInjectiveLeft (l : rest') r = l : CDotInjectiveLeft rest' r

-- type family CDotInjectiveRight (res :: [d]) (l :: [d]) :: [d] where
--     CDotInjectiveLeft r '[] = r
--     CDotInjectiveLeft (l : rest') (l : ls) = CDotInjectiveRight rest' ls

class (TensorAlgebra (f (Rep cont m)) (f' (Rep cont m')),
        ((f (Rep cont m)) >< (f' (Rep cont m'))) ~ f (Rep cont (CDot m m')))
        => ContravatiantTensor f cont m f' m'
instance (TensorAlgebra (f (Rep cont m)) (f' (Rep cont m'))
        , ((f (Rep cont m)) >< (f' (Rep cont m'))) ~ f (Rep cont (CDot m m'))) 
        => ContravatiantTensor f cont m f' m'

dot :: forall f f' cont m m'. (ContravatiantTensor f cont m f' m')
    => f (Rep cont m) -> f' (Rep cont m') -> f (Rep cont (CDot m m'))
dot = (><) @(f (Rep cont m)) @(f' (Rep cont m'))
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
