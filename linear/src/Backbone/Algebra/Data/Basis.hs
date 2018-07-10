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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Backbone.Algebra.Data.Basis
    (
        InifiniteBasis(..)
    ,   ExplBasis(..)
    ,   SExplBasis
    ,   FinBasis(..)
    ,   FinBasisT(..)
    ,   OneShape(..)
    ,   Shaped(..)
    )
where

import           Backbone.Algebra.Linear.Basis
import           Backbone.Algebra.Linear.Matrix
import           GHC.Types               hiding ( Module(..), type (*))
import           Data.Singletons.TH
import           NumHask.Algebra.Integral
import qualified Numeric.Natural               as Na
import           Prelude                        ( Show , Eq)
import qualified Prelude                       as P
import           GHC.TypeNats
import           Unsafe.Coerce

--------------------------------------------
---- Bases

-- Inifinitly-dimensional standard-basis
-- should serve more as a template, but may be useful in the future
data InifiniteBasis = InfStandard
    deriving (Show, Eq)
$(promoteShowInstances [''InifiniteBasis])
$(promoteEqInstances [''InifiniteBasis])

data instance Sing :: (InifiniteBasis) -> Type where
    SInfStandard :: Sing (InfStandard)

instance SingI (InfStandard) where
    sing = SInfStandard

instance SingKind (InifiniteBasis) where
    -- this is the magic sauce...no Demoting of a!
    type Demote (InifiniteBasis) = (InifiniteBasis)
    fromSing SInfStandard = InfStandard

    toSing InfStandard = SomeSing SInfStandard

instance Basis (x :: InifiniteBasis) b where
    --type BasisType (InfStandard :: InifiniteBasis a) = a

    project _ a = a

--- the Explicit, finite basis  
-- We need to write them per hand!
-- due to type-system limitations (see: https://github.com/goldfirere/singletons/issues/347)
-- this is way more complicated than it should be

$(singletons [d|
    -- the hidden type-level representation
    -- an explicit Basis
    -- written by hand to avoid (SingKind a)
    data ExplBasis (b :: Type) = ExplBasis [b]
        deriving (Show, Eq)
    |])

--FIXME: currently unable to write this class  
-- instance (BasisCompat a) => Basis (x :: (ExplBasis (ElementTypeLevelRep b))) b where
--     --the type after the koordinates got projected through the basis
--     -- type Projection a b :: Type
--     -- type Projection a b = b

--     -- projects the coordinates through the basis
--     project :: Demote (KindOf a) -> b -> Projection a b

-- a finiti-dimensional, not explicitly shaped standard-basis                
--type-level
newtype FinBasis = FinBasis Nat
$(promoteShowInstances [''FinBasis])
$(promoteEqInstances [''FinBasis])
--term level
newtype FinBasisT = FinBasisT Na.Natural
    deriving (Show, Eq)

data instance Sing :: FinBasis -> Type where
    SFinBasis :: forall (k :: Nat). 
                    Sing (k) 
                    -> Sing ('FinBasis k)

instance SingI (n) => SingI ('FinBasis n) where
    sing = SFinBasis sing

instance SingKind FinBasis where
    type Demote FinBasis = FinBasisT
    fromSing (SFinBasis n) = FinBasisT (fromSing n)

    toSing (FinBasisT n) = withSomeSing n (\n' -> SomeSing P.$ SFinBasis n')

instance Basis (x :: FinBasis) b where

    project _ a = a

instance FiniteBasis ('FinBasis n) b where
    type Dim ('FinBasis n) = n

    getDim (FinBasisT n) = fromInteger (P.toInteger n)

-- -- a finiti-dimensional, explicitly shaped standard-basis    
-- --type-level
-- newtype ShapedBasis (a :: Type) = ShapedBasis [Nat]
-- $(promoteShowInstances [''ShapedBasis])
-- $(promoteEqInstances [''ShapedBasis])
-- --term level
-- newtype ShapedBasisT a n = ShapedBasisT [n]
--     deriving (Show, Eq)

-- data instance Sing :: ShapedBasis a -> Type where
--     SShapedBasis :: forall (k :: [Nat]). 
--                     Sing (k) 
--                     -> Sing ('ShapedBasis k)

-- instance SingI (n) => SingI ('ShapedBasis n) where
--     sing = SShapedBasis sing

-- instance SingKind (ShapedBasis a) where
--     type Demote (ShapedBasis a) = ShapedBasisT a Na.Natural
--     fromSing (SShapedBasis n) = ShapedBasisT (fromSing n)

--     toSing (ShapedBasisT n) = withSomeSing n (\n' -> SomeSing P.$ SShapedBasis n')

-- instance Basis (x :: ShapedBasis a) where
--     type BasisType (x :: ShapedBasis a) = a

--     project _ a = a

-- instance FiniteBasis (('ShapedBasis n) :: ShapedBasis a) where
--     type Dim ('ShapedBasis n) = CalcDim n

--     getDim (ShapedBasisT n) = fromInteger (P.toInteger (calcDim n))

-- type family CalcDim (e :: [Nat]) :: Nat where
--     CalcDim (x:xs) = x * (CalcDim xs)
--     CalcDim '[] = 1

-- calcDim :: [Na.Natural] -> Na.Natural
-- calcDim [] = 1
-- calcDim (x:xs) = x P.* (calcDim xs)

-- promotable finite-dim, single-dim List
data OneShape :: Type -> Nat -> Type where
    OneNil :: OneShape a 0
    OneCons :: a -> OneShape a x -> OneShape a (x + 1)

-- promotable finite-dim, multi-dim List
data Shaped :: Type -> [Nat] -> Type where
    SNil :: Shaped a '[]
    SCons :: OneShape a n -> Shaped a s -> Shaped a (n : s)

data instance Sing :: forall a x. OneShape a x -> Type where
    SOneNil  :: Sing OneNil
    SOneCons :: Sing y -> Sing ys -> Sing (OneCons y ys)

instance SingKind a => SingKind (OneShape a x) where
    type Demote (OneShape a x) = OneShape (Demote a) x
    fromSing SOneNil           = OneNil
    fromSing (SOneCons sy sys) = OneCons (fromSing sy) (fromSing sys)
    toSing OneNil         = SomeSing SOneNil
    -- The lines below in particular requires GHC 8.6 in order to typecheck.
    -- FIXME: delete unsafeCoerce when 8.6 is ready
    toSing (OneCons y ys) = withSomeSing y  P.$ \sy  ->
                            withSomeSing ys P.$ \sys ->
                            SomeSing P.$ unsafeCoerce P.$ SOneCons sy sys


-- TODO write helper