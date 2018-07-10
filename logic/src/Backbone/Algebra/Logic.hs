{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
-- |
--
-- See "Plausibility Measures: A User's Guide" from UAI 1995
module Backbone.Algebra.Logic where

import           Control.Monad
import           Test.QuickCheck.Gen            ( suchThat
                                                , oneof
                                                )

import           Backbone.Algebra.Logic.Logic
import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Module
import           NumHask.Algebra.Abstract.Additive
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Data.Rational
import           Test.QuickCheck                ( frequency
                                                , Arbitrary(..)
                                                )
import           Control.DeepSeq
import           Backbone.TH.Deriving
import           Prelude                        ( Read(..)
                                                , Show(..)
                                                , Bool(..)
                                                , ($)
                                                )
import qualified Prelude                       as P
import           Data.Coerce

class (Ord r, Ring r) => OrdRing_ r
instance (Ord r, Ring r) => OrdRing_ r

--------------------------------------------------------------------------------

-- | The Goedel fuzzy logic is one of the simpler fuzzy logics.
-- In particular, it is an example of a Heyting algebra that is not also a Boolean algebra.
--
-- See the <plato.stanford.edu/entries/logic-fuzzy standford encyclopedia of logic>
type Goedel = Goedel_ Rational

newtype Goedel_ r = Goedel_ r

deriveHierarchyFiltered ''Goedel_ [ ''Eq ] [ ''Arbitrary,(mkName "Scalar"),(mkName "Actor")]

instance (OrdRing_ r, Arbitrary r, Logic r ~ Bool) => Arbitrary (Goedel_ r) where
    arbitrary = fmap Goedel_ $ arbitrary `suchThat` ((>=zero) && (<=one))

instance OrdRing_ r => POrd (Goedel_ r) where
    inf (Goedel_ r1) (Goedel_ r2) = Goedel_ $ min r1 r2

instance OrdRing_ r => Lattice (Goedel_ r) where
    sup (Goedel_ r1) (Goedel_ r2) = Goedel_ $ max r1 r2

deriving instance (OrdRing_ r) => Ord (Goedel_ r)

instance OrdRing_ r => MinBound  (Goedel_ r) where
    minBound = Goedel_ zero

instance OrdRing_ r => Bounded  (Goedel_ r) where
    maxBound = Goedel_ one

instance (OrdRing_ r, Decide (Logic r)) => Heyting (Goedel_ r) where
--     (Goedel_ r1)==>(Goedel_ r2) = if r1 <= r2 then Goedel_ 1 else Goedel_ (1 - r1 + r2)
  (Goedel_ r1)==>(Goedel_ r2) = if r1 <= r2 then Goedel_ one else Goedel_ r2

---------------------------------------

-- | H3 is the smallest Heyting algebra that is not also a boolean algebra.
-- In addition to true and false, there is a value to represent whether something's truth is unknown.
-- AFAIK it has no real applications.
--
-- See <https://en.wikipedia.org/wiki/Heyting_algebra#Examples wikipedia>
data H3
    = HTrue
    | HFalse
    | HUnknown
    deriving (Read,Show)

instance NFData H3 where
    rnf HTrue = ()
    rnf HFalse = ()
    rnf HUnknown = ()

instance Arbitrary H3 where
    arbitrary = oneof $ P.map return [HTrue, HFalse, HUnknown]

type instance Logic H3 = Bool

instance Eq H3 where
    HTrue    == HTrue    = True
    HFalse   == HFalse   = True
    HUnknown == HUnknown = True
    _        == _        = False

instance POrd H3 where
    inf HTrue    HTrue    = HTrue
    inf HTrue    HUnknown = HUnknown
    inf HUnknown HTrue    = HUnknown
    inf HUnknown HUnknown = HUnknown
    inf _        _        = HFalse

instance Lattice H3 where
    sup HFalse    HFalse   = HFalse
    sup HFalse    HUnknown = HUnknown
    sup HUnknown  HFalse   = HUnknown
    sup HUnknown  HUnknown = HUnknown
    sup _         _        = HTrue

instance Ord H3

instance MinBound H3 where
    minBound = HFalse

instance Bounded H3 where
    maxBound = HTrue

instance Heyting H3 where
    _        ==> HTrue    = HTrue
    HFalse   ==> _        = HTrue
    HTrue    ==> HFalse   = HFalse
    HUnknown ==> HUnknown = HTrue
    HUnknown ==> HFalse   = HFalse
    _        ==> _        = HUnknown

---------------------------------------

-- | K3 stands for Kleene's 3-valued logic.
-- In addition to true and false, there is a value to represent whether something's truth is unknown.
-- K3 is an example of a logic that is neither Boolean nor Heyting.
--
-- See <http://en.wikipedia.org/wiki/Three-valued_logic wikipedia>.
--
-- FIXME: We need a way to represent implication and negation for logics outside of the Lattice hierarchy.
data K3
    = KTrue
    | KFalse
    | KUnknown
    deriving (Read,Show)

instance NFData K3 where
    rnf KTrue = ()
    rnf KFalse = ()
    rnf KUnknown = ()

instance Arbitrary K3 where
    arbitrary = oneof $ P.map return [KTrue, KFalse, KUnknown]

type instance Logic K3 = Bool

instance Eq K3 where
    KTrue    == KTrue    = True
    KFalse   == KFalse   = True
    KUnknown == KUnknown = True
    _        == _        = False

instance POrd K3 where
    inf KTrue    KTrue    = KTrue
    inf KTrue    KUnknown = KUnknown
    inf KUnknown KTrue    = KUnknown
    inf KUnknown KUnknown = KUnknown
    inf _        _        = KFalse

instance Lattice K3 where
    sup KFalse    KFalse   = KFalse
    sup KFalse    KUnknown = KUnknown
    sup KUnknown  KFalse   = KUnknown
    sup KUnknown  KUnknown = KUnknown
    sup _         _        = KTrue

instance Ord K3

instance MinBound K3 where
    minBound = KFalse

instance Bounded K3 where
    maxBound = KTrue

--------------------------------------------------------------------------------
-- | A Boolean algebra is a special type of Ring.
-- Their applications (set-like operations) tend to be very different than Rings, so it makes sense for the class hierarchies to be completely unrelated.
-- The "Boolean2Ring" type, however, provides the correct transformation.

newtype Boolean2Ring b = Boolean2Ring b

deriveHierarchy ''Boolean2Ring [ ''Boolean ]

instance (Boolean b, Eq b) => Magma (Sum (Boolean2Ring b)) where
    (Sum (Boolean2Ring b1)) `magma`(Sum (Boolean2Ring b2)) = Sum $ Boolean2Ring $ (b1 || b2) && not (b1 && b2)

instance (Boolean b, Eq b) => Semigroup (Sum (Boolean2Ring b))

instance (Boolean b, Eq b) => Commutative (Sum (Boolean2Ring b))

instance (Boolean b, Eq b) => Unital (Sum (Boolean2Ring b)) where
    unit = Sum $ Boolean2Ring false

instance (Boolean b, Eq b) => Invertible (Sum (Boolean2Ring b)) where
    inv = P.id

-- instance (Boolean b, Eq b) => Group (Boolean2Ring b) where
--     negate = id
instance (Boolean b, Eq b) => Magma (Product (Boolean2Ring b)) where
    (Product (Boolean2Ring b1)) `magma`(Product (Boolean2Ring b2)) = Product $ Boolean2Ring $ b1 && b2

instance (Boolean b, Eq b) => Commutative (Product (Boolean2Ring b))

instance (Boolean b, Eq b) => Unital (Product (Boolean2Ring b)) where
    unit = Product $ Boolean2Ring true

instance (Boolean b, Eq b) => Absorbing (Product (Boolean2Ring b)) where
    absorb = Product $ Boolean2Ring false

instance (Boolean b, Eq b) => (Semigroup (Product (Boolean2Ring b)))

instance (Boolean b, Eq b) => Distributive (Boolean2Ring b)

instance (Boolean b, Eq b) => Ring (Boolean2Ring b)
