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
module Backbone.Algebra.Abstract.Multilinear
    ( 
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

--TODO Multilinear
-- -- | Tensor algebras generalize the outer product of vectors to construct a matrix.
-- --
-- -- See <https://en.wikipedia.org/wiki/Tensor_algebra wikipedia> for details.
-- --
-- -- FIXME:
-- -- This needs to be replaced by the Tensor product in the Monoidal category Vect
-- class
--     ( VectorSpace v
--     , VectorSpace (v><v)
--     , Scalar (v><v) ~ Scalar v
--     , Field (v><v)
--     ) => Multilinear v
--         where