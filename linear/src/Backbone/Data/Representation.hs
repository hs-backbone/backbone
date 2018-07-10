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
module Backbone.Data.Representation
    ( RepShape(..)
    , RepInf(..)
    , Represent(..)
    , ElemContainer(..)
    , BasisI(..)
    , MetricI(..)
    , BasisMetricI(..)
    , Default(..)
    )
where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           Backbone.Algebra.Linear.Hadamard
import           Backbone.Algebra.Linear.Basis
import           Backbone.Algebra.Abstract.Module
import           Backbone.Algebra.Abstract.VectorSpace
import           Backbone.Analysis.Metric
import           Backbone.TH.TH
import           Language.Haskell.TH     hiding (Type)
import           Prelude                 ((>>=), Show, Eq)
import           GHC.Types               hiding ( Module(..) )
import           Data.Coerce

-----------------------------------------------------------
--- the physical representation

-- all the info needed to derive the physical representation
data RepShape k = Rep {rep :: k, shape :: [Nat]}
data RepInf k =  RepInf {repInf :: k}

-- the usual split into Element and Container-types 
data ElemContainer = ElCont {container :: Type, element :: Type}

-- derive the physical representation
type family Represent (a :: r) :: Type

-----------------------------------------------------------
--- the associated meta-info (everything that has no impact on the physical representation)

-- only associate Info is the Basis
data BasisI = Basis {basis :: Type}

-- only associated Info is a Metric
data MetricI = Metric {metric :: Type}

-- both a Basis and a Metric 
data BasisMetricI m = BasisMetric {basis' :: Type, metric' :: Type}

-- the Default ("common") representaiton
--type role Default phantom nominal
newtype Default (meta :: m) (rep :: r) = Default {unwrapDefault :: Represent rep}

deriving instance Show (Represent rep) => Show (Default meta rep)
deriving instance Eq (Represent rep) => Eq (Default meta rep)

type instance ExtractBasis (Basis b) = b
type instance ExtractBasis (BasisMetric b _) = b

type instance ExtractMetric (Metric m) = m
type instance ExtractMetric (BasisMetric _ m) = m

$(runQ [t|forall meta rep. Represent rep|] >>= (genNum' ''Default ["meta"] []))

