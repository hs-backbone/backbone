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
module Backbone.Algebra.Abstract.VectorSpace
    ( FreeModule(..)
    , FiniteModule(..)
    , VectorSpace(..)
    )
where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           Backbone.Algebra.Linear.Basis
import           Backbone.Algebra.Abstract.Module
import           Backbone.Analysis.Metric
import           Prelude                 hiding ( recip
                                                , Semigroup
                                                , (/)
                                                )
import           GHC.Types               hiding ( Module(..) )
import           Data.Coerce

-- | a free Module has a Basis
class (Module a, Basis (GetBasis a) a) => FreeModule a where
    type GetBasis a :: Type

-- | a finite Module has a finite Basis
class (FreeModule a, FiniteBasis (GetBasis a) a) => FiniteModule a where
    unsafeToModule :: [Scalar a] -> a

class (FreeModule a, Field (Scalar a)) => VectorSpace a where
    {-# INLINE (./) #-}
    infixl 7 ./
    (./) :: a -> Scalar a -> a
    v ./ r = v .* recip r

    infixl 7 ./.
    (./.) :: a -> a -> a

--FIXME: move and associate with basis!    
instance {-# OVERLAPPABLE #-} (Field a, Scalar a ~ a,
                                (a >< a) ~ a, FreeModule a)
                                => VectorSpace a where
    (./) = (/)
    (./.) = (/)
