{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
module Backbone.Algebra.Abstract.Module
    (
        Scalar
    ,   type (><)
    ,   Cone (..)
    ,   Module (..)
    )
where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           Prelude                 hiding ( (*) )
import           GHC.Types               hiding ( Module(..) )


type family Scalar m :: Type

type instance Scalar Int      = Int
type instance Scalar Integer  = Integer
type instance Scalar Float    = Float
type instance Scalar Double   = Double
type instance Scalar Rational = Rational

type instance Scalar (a,b) = Scalar a
type instance Scalar (a,b,c) = Scalar a
type instance Scalar (a,b,c,d) = Scalar a

type instance Scalar (a -> b) = Scalar b

-- FIXME: straight from subhask!
-- | A Cone is an \"almost linear\" subspace of a module.
-- Examples include the cone of positive real numbers and the cone of positive semidefinite matrices.
--
-- See <http://en.wikipedia.org/wiki/Cone_%28linear_algebra%29 wikipedia> for more details.
--
-- FIXME:
-- There are many possible laws for cones (as seen in the wikipedia article).
-- I need to explicitly formulate them here.
-- Intuitively, the laws should apply the module operations and then project back into the "closest point" in the cone.
--
-- FIXME:
-- We're using the definition of a cone from linear algebra.
-- This definition is closely related to the definition from topology.
-- What is needed to ensure our definition generalizes to topological cones?
-- See <http://en.wikipedia.org/wiki/Cone_(topology) wikipedia>
-- and <http://ncatlab.org/nlab/show/cone ncatlab> for more details.
class (Semiring (Scalar m)) => Cone m where
    infixl 7 *..
    (*..) :: Scalar m -> m -> m

    infixl 7 ..*..
    (..*..) :: m -> m -> m

instance (Scalar m ~ m, Semiring m) => Cone m where
    (*..) = (*)
    (..*..) = (*)

-- the  tensor product
infixr 8 ><
type family (><) (a::k1) (b::k2) :: *
type instance Int       >< Int        = Int
type instance Integer   >< Integer    = Integer
type instance Float     >< Float      = Float
type instance Double    >< Double     = Double
type instance Rational  >< Rational   = Rational

-- type instance (a,b)     >< Scalar b   = (a,b)
-- type instance (a,b,c)   >< Scalar b   = (a,b,c)

type instance (a -> b)  >< c          = a -> (b><c)

-- | more info: https://en.wikipedia.org/wiki/Module_(mathematics)
class
    ( AbelianGroup (Sum v)
    , v ~ (v><Scalar v)
    , v ~ (Scalar v><v)
    ) => Module v
        where
    -- | Scalar multiplication.
    infixl 7 .*
    (.*) :: v -> Scalar v -> v

infixl 7 *.
{-# INLINE (*.) #-}
(*.) :: Module v => Scalar v -> v -> v
(*.) a b = b .* a

instance {-# OVERLAPPABLE #-} (Ring a, Scalar a ~ a, (a >< a) ~ a) => Module a where
    (.*) = (*)
-- instance (Module b) => Module (a -> b)
--         where
--     f .*  b = \a -> f a .*  b
