{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines the basic Logic used in subhask
-- The class hierarchies are significantly more general than those in the standard Prelude.
module Backbone.Algebra.Logic.Logic
    (
    -- * Comparisons
    Logic
--     , TLogic
    , IdempLogic
    , Classical
    , ClassicalLogic
    , Elem
--     , TElem
    , Container (..)
    , law_Container_preservation
--  , ifThenElse
    , IfThenElse(..)
    , Eq (..)
    , law_Eq_reflexive
    , law_Eq_symmetric
    , law_Eq_transitive
    , defn_Eq_noteq
    , POrd (..)
    , law_POrd_commutative
    , law_POrd_associative
    , theorem_POrd_idempotent
    , Lattice (..)
    -- , isChain
    -- , isAntichain
    , POrdering (..)
    , law_Lattice_commutative
    , law_Lattice_associative
    , theorem_Lattice_idempotent
    , law_Lattice_infabsorption
    , law_Lattice_supabsorption
    , law_Lattice_reflexivity
    , law_Lattice_antisymmetry
    , law_Lattice_transitivity
    , defn_Lattice_greaterthan
    , MinBound (..)
    , law_MinBound_inf
    , Bounded (..)
    , law_Bounded_sup
    , Complemented (..)
    , law_Complemented_not
    , Heyting (..)
    , modusPonens
    , law_Heyting_maxbound
    , law_Heyting_infleft
    , law_Heyting_infright
    , law_Heyting_distributive
    , Boolean
    , law_Boolean_infcomplement
    , law_Boolean_supcomplement
    , law_Boolean_infdistributivity
    , law_Boolean_supdistributivity
    , Ord (..)
    , law_Ord_totality
    , law_Ord_min
    , law_Ord_max
    , Ordering (..)
    , min
    , max
    , Graded (..)
    , law_Graded_fromEnum
    , law_Graded_pred
    , defn_Graded_predN
    , (>.)
    , (<.)
    , Enum (..)
    , law_Enum_toEnum
    , law_Enum_succ
    , defn_Enum_succN

    -- ** Boolean helpers
    , (||)
    , (&&)
    , true
    , false
    , otherwise
    ) where

import qualified Prelude as P
import Prelude (($), (++))

import Data.Ratio
import Test.QuickCheck (frequency, Arbitrary(..))

import GHC.Types hiding (Module, type (*), True, False)

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Natural
import Backbone.Data.Backend

-- the next step after NumHask. Also added some missing instances
type Natural' a = (Natural a, ClassicalLogic (Logic a), Lattice a, POrd a, Invertible (Sum a), Unital (Product a))

-------------------------------------------------------------------------------
-- comparison hierarchy

-- generalization of a "set"
type family Elem s

-- | This is a generalization of a "set".
-- We do not require a container to be a boolean algebra, just a semigroup.
class Eq a => Container a where
    {-# MINIMAL elem | notElem #-}
    elem :: Elem a -> a -> Logic a
    elem = not notElem

    notElem :: Elem a -> a -> Logic a
    notElem = not elem

law_Container_preservation :: Container s => s -> s -> Elem s -> Logic s
law_Container_preservation a1 a2 e = (a1==a2) ==> ((e `elem` a1) ==> (e `elem` a2))

type instance Elem Bool = ()
instance Container Bool where
    elem _ P.True  = P.True
    elem _ P.False = P.False

type instance Elem () = ()
instance Container () where
    elem () = \_ -> ()

instance Eq b => Container (a -> b)

type instance Elem [a] = a

instance Eq a => Container [a] where
    elem _ []       = false
    elem x (y:ys)   = x==y || elem x ys

    notElem = not elem

--------------------

class
    ( Monoid (Elem a)
    , Container a
    , IfThenElse (Logic a)
    ) => IfThenElse a
        where
    ifThenElse :: a -> b -> b -> b
    ifThenElse a = ifThenElse (unit `elem` a)
instance {-# OVERLAPPABLE #-} 
    ( Monoid (Elem a)
    , Container a
    , IfThenElse (Logic a)
    ) => IfThenElse a

instance Magma Bool where
    {-# INLINE magma #-}
    magma = (||)
instance Semigroup Bool
instance Unital Bool where unit = P.False
instance IfThenElse Bool where
    ifThenElse P.True  b _ = b
    ifThenElse P.False _ b = b
instance Magma () where
    {-# INLINE magma #-}
    magma () () = ()
instance Unital () where unit = ()
instance Semigroup ()
instance IfThenElse () where
    ifThenElse () b _ = b

----------------------------------------

-- | Every type has an associated logic.
-- Most types use classical logic, which corresponds to the Bool type.
-- But types can use any logical system they want.
-- Functions, for example, use an infinite logic.
-- You probably want your logic to be an instance of "Boolean", but this is not required.
--
-- See wikipedia's articles on <https://en.wikipedia.org/wiki/Algebraic_logic algebraic logic>,
-- and <https://en.wikipedia.org/wiki/Infinitary_logic infinitary logic> for more details.
type family Logic a :: Type

type IdempLogic a = Logic (Logic (Logic a))~Logic (Logic a)

type Classical (alg :: Type -> Constraint) (a :: Type) = (ClassicalLogic a, alg a)

class (Boolean a, IfThenElse a, Bounded a)
     => ClassicalLogic a where
    fromBool :: Bool -> a
    toBool :: P.Monad (BMonad a) => a -> (BMonad a) Bool

pattern True :: forall a. ClassicalLogic a => a
pattern True <- true  where
    True = true

pattern False :: forall a. ClassicalLogic a => a
pattern False <- false where
     False = false

-- FIXME: figure out how to get this working
-- {-# COMPLETE True, False #-}

-- | Defines equivalence classes over the type.
-- The values need not have identical representations in the machine to be equal.
--
-- See <https://en.wikipedia.org/wiki/Equivalence_class wikipedia>
-- and <http://ncatlab.org/nlab/show/equivalence+class ncatlab> for more details.
class (IdempLogic a, Container (Logic a), Boolean (Logic a)) => Eq a where
    {-# MINIMAL (==) | (/=) #-}

    infix 4 ==
    (==) :: a -> a -> Logic a
    (==) = not (/=)

    infix 4 /=
    (/=) :: a -> a -> Logic a
    (/=) = not (==)

law_Eq_reflexive :: Eq a => a -> Logic a
law_Eq_reflexive a = a==a

law_Eq_symmetric :: Eq a => a -> a -> Logic (Logic a)
law_Eq_symmetric a1 a2 = (a1==a2)==(a2==a1)

law_Eq_transitive :: Eq a => a -> a -> a -> Logic a
law_Eq_transitive a1 a2 a3 = (a1==a2&&a2==a3) ==> (a1==a3)

defn_Eq_noteq :: (IdempLogic a, Eq a) => a -> a -> Logic (Logic a)
defn_Eq_noteq a1 a2 = (a1/=a2) == (not $ a1==a2)

#define mkEq(x) \
type instance Logic x = Bool; \
instance Eq x where (==) = (P.==); (/=) = (P./=);{-# INLINE (==) #-};{-# INLINE (/=) #-};

mkEq(Bool)
mkEq(Char)
mkEq(Int)
mkEq(P.Integer)
mkEq(Float)
mkEq(Double)
mkEq(Rational)

type instance Logic () = ()
instance Eq () where
    () == () = ()
    () /= () = ()

-- type instance Logic (a -> b) = (Neighbor (a -> b) -> Bool)
type instance Logic (a -> b) = a -> Logic b
instance Eq b => Eq (a -> b) where
--     (==) f g (xs,nb) = go xs
--         where
--             go (x:xs) = (f x==g x) nb && go xs
--             go []     = P.True

type instance Logic [a] = Logic a

type instance Logic (P.Maybe a) = Logic a

instance Eq a => Eq (P.Maybe a) where
    P.Nothing   == P.Nothing   = true
    P.Nothing   == _         = false
    _         == P.Nothing   = false
    (P.Just a1) == (P.Just a2) = a1==a2

instance Eq a => Eq [a] where
    (x:xs)==(y:ys) = x==y && xs==ys
    (_:_)==[]     = false
    []    ==(_:_) = false
    []    ==[]     = true
--------------------

-- | This is more commonly known as a "meet" semilattice
class Eq b => POrd b where
    inf :: b -> b -> b

    {-# INLINE (<=) #-}
    infix 4 <=
    (<=) :: b -> b -> Logic b
    b1 <= b2 = inf b1 b2 == b1

    {-# INLINE (<) #-}
    (<) :: b -> b -> Logic b
    b1 < b2 = inf b1 b2 == b1 && b1 /= b2

law_POrd_commutative :: POrd b => b -> b -> Logic b
law_POrd_commutative b1 b2 = inf b1 b2 == inf b2 b1

law_POrd_associative :: POrd b => b -> b -> b -> Logic b
law_POrd_associative b1 b2 b3 = inf (inf b1 b2) b3 == inf b1 (inf b2 b3)

theorem_POrd_idempotent :: POrd b => b -> Logic b
theorem_POrd_idempotent b = inf b b == b

#define mkPOrd(x) \
instance POrd x where \
    inf = (P.min) ;\
    (<=) = (P.<=) ;\
    (<) = (P.<) ;\
    {-# INLINE inf #-} ;\
    {-# INLINE (<=) #-} ;\
    {-# INLINE (<) #-}

   
mkPOrd(Bool)
mkPOrd(Char)
mkPOrd(Int)
mkPOrd(P.Integer)
mkPOrd(Float)
mkPOrd(Double)
mkPOrd(Rational)

instance POrd () where
    {-# INLINE inf #-}
    inf () () = ()

instance POrd b => POrd (a -> b) where
    {-# INLINE inf #-}
    inf f g = \x -> inf (f x) (g x)

    {-# INLINE (<=) #-}
    (f<=g) a = f a <= g a

instance (Eq a, ClassicalLogic a, Logic a ~ Bool) => POrd [a] where
    inf [] _  = []
    inf _  [] = []
    inf (x:xs) (y:ys) = if x==y
        then x:inf xs ys
        else []
-------------------

-- | Most Lattice literature only considers 'Bounded' lattices, but here we have both upper and lower bounded lattices.
--
-- prop> minBound <= b || not (minBound > b)
--
class POrd b => MinBound b where
    minBound :: b

law_MinBound_inf :: MinBound b => b -> Logic b
law_MinBound_inf b = inf b minBound == minBound

-- | "false" is an upper bound because `a && false = false` for all a.
{-# INLINE false #-}
false :: MinBound b => b
false = minBound

instance MinBound ()       where minBound = ()         ; {-# INLINE minBound #-}
instance MinBound Bool     where minBound = P.False      ; {-# INLINE minBound #-}
instance MinBound Char     where minBound = P.minBound ; {-# INLINE minBound #-}
instance MinBound Int     where minBound = P.minBound ; {-# INLINE minBound #-}
instance {-# OVERLAPPABLE #-}(LowerBoundedField a, POrd a) => MinBound a where minBound = negInfinity ; {-# INLINE minBound #-}
-- FIXME: add
-- FIXME: should be a primop for this
-- FIXME: standard Prelude

instance (Eq a, ClassicalLogic a, Logic a ~ Bool) => MinBound [a] where
    minBound = []

instance MinBound b => MinBound (a -> b) where minBound = \_ -> minBound ; {-# INLINE minBound #-}

-------------------

-- | Represents all the possible ordering relations in a classical logic
data POrdering
    = PLT
    | PGT
    | PEQ
    | PNA
    deriving (P.Read,P.Show)

type instance Logic POrdering = Bool

instance Arbitrary POrdering where
    arbitrary = frequency
        [ (one, P.return PLT)
        , (one, P.return PGT)
        , (one, P.return PEQ)
        , (one, P.return PNA)
        ]

instance Eq POrdering where
    {-# INLINE (==) #-}
    PLT == PLT = P.True
    PGT == PGT = P.True
    PEQ == PEQ = P.True
    PNA == PNA = P.True
    _ == _ = P.False

-- | FIXME: there are many semigroups over POrdering;
-- how should we represent the others? newtypes?
instance Magma POrdering where
    {-# INLINE magma #-}
    PEQ `magma` x = x
    PLT `magma` _ = PLT
    PGT `magma` _ = PGT
    PNA `magma` _ = PNA

instance Semigroup POrdering

type instance Logic Ordering = Bool

instance Eq Ordering where
    {-# INLINE (==) #-}
    EQ == EQ = P.True
    LT == LT = P.True
    GT == GT = P.True
    _  == _  = P.False

instance Magma Ordering where
    {-# INLINE magma #-}
    EQ `magma` x = x
    LT `magma` _ = LT
    GT `magma` _ = GT

instance Semigroup Ordering

instance Unital POrdering where
    {-# INLINE unit #-}
    unit = PEQ

instance Unital Ordering where
    {-# INLINE unit #-}
    unit = EQ

-- |
--
--
-- See <https://en.wikipedia.org/wiki/Lattice%28order%29 wikipedia> for more details.
class POrd b => Lattice b where
    sup :: b -> b -> b

    {-# INLINE (>=) #-}
    infix 4 >=
    (>=) :: b -> b -> Logic b
    b1 >= b2 = sup b1 b2 == b1

    {-# INLINE (>) #-}
    (>) :: b -> b -> Logic b
    b1 > b2 = sup b1 b2 == b1 && b1 /= b2

    -- | This function does not make sense on non-classical logics
    --
    -- FIXME: there are probably related functions for all these other logics;
    -- is there a nice way to represent them all?
    {-# INLINABLE pcompare #-}
    pcompare :: Logic b ~ Bool => b -> b -> POrdering
    pcompare a b = if a==b
        then PEQ
        else if a < b
            then PLT
            else if a > b
                then PGT
                else PNA

law_Lattice_commutative :: Lattice b => b -> b -> Logic b
law_Lattice_commutative b1 b2 = sup b1 b2 == sup b2 b1

law_Lattice_associative :: Lattice b => b -> b -> b -> Logic b
law_Lattice_associative b1 b2 b3 = sup (sup b1 b2) b3 == sup b1 (sup b2 b3)

theorem_Lattice_idempotent :: Lattice b => b -> Logic b
theorem_Lattice_idempotent b = sup b b == b

law_Lattice_infabsorption :: Lattice b => b -> b -> Logic b
law_Lattice_infabsorption b1 b2 = inf b1 (sup b1 b2) == b1

law_Lattice_supabsorption :: Lattice b => b -> b -> Logic b
law_Lattice_supabsorption b1 b2 = sup b1 (inf b1 b2) == b1

law_Lattice_reflexivity :: Lattice a => a -> Logic a
law_Lattice_reflexivity a = a<=a

law_Lattice_antisymmetry :: (ClassicalLogic a, Lattice a, Logic a ~ Bool) => a -> a -> Logic a
law_Lattice_antisymmetry a1 a2
    | a1 <= a2 && a2 <= a1 = a1 == a2
    | otherwise = true

law_Lattice_transitivity :: (ClassicalLogic a, Lattice a, Logic a ~ Bool) => a -> a -> a -> Logic a
law_Lattice_transitivity  a1 a2 a3
    | a1 <= a2 && a2 <= a3 = a1 <= a3
    | a1 <= a3 && a3 <= a2 = a1 <= a2
    | a2 <= a1 && a1 <= a3 = a2 <= a3
    | a2 <= a3 && a3 <= a1 = a2 <= a1
    | a3 <= a2 && a2 <= a1 = a3 <= a1
    | a3 <= a1 && a1 <= a2 = a3 <= a2
    | otherwise = true

defn_Lattice_greaterthan :: (ClassicalLogic a, Lattice a, Logic a ~ Bool) => a -> a -> Logic a
defn_Lattice_greaterthan a1 a2
    | a1 < a2 = a2 >= a1
    | a1 > a2 = a2 <= a1
    | otherwise = true

#define mkLattice(x)\
instance Lattice x where \
    sup = (P.max) ;\
    (>=) = (P.>=) ;\
    (>) = (P.>) ;\
    {-# INLINE sup #-} ;\
    {-# INLINE (>=) #-} ;\
    {-# INLINE (>) #-}

mkLattice(Bool)
mkLattice(Char)
mkLattice(Int)
mkLattice(P.Integer)
mkLattice(Float)
mkLattice(Double)
mkLattice(Rational)

instance Lattice () where
    {-# INLINE sup #-}
    sup () () = ()

instance Lattice b => Lattice (a -> b) where
    {-# INLINE sup #-}
    sup f g = \x -> sup (f x) (g x)

    {-# INLINE (>=) #-}
    (f>=g) a = f a >= g a

{-# INLINE (&&) #-}
infixr 3 &&
(&&) :: Lattice b => b -> b -> b
(&&) = inf

{-# INLINE (||) #-}
infixr 2 ||
(||) :: Lattice b => b -> b -> b
(||) = sup

--TODO polymorphic chain
--FIXME: uncomment
-- -- | A chain is a collection of elements all of which can be compared
-- {-# INLINABLE isChain #-}
-- isChain :: (Lattice a, ClassicalLogic a) => [a] -> Logic a
-- isChain [] = true
-- isChain (x:xs) = all (/=PNA) (P.map (pcompare x) xs) && isChain xs

-- -- | An antichain is a collection of elements none of which can be compared
-- --
-- -- See <http://en.wikipedia.org/wiki/Antichain wikipedia> for more details.
-- --
-- -- See also the article on <http://en.wikipedia.org/wiki/Dilworth%27s_theorem Dilward's Theorem>.
-- {-# INLINABLE isAntichain #-}
-- isAntichain :: (Lattice a, ClassicalLogic a) => [a] -> Logic a
-- isAntichain [] = true
-- isAntichain (x:xs) = all (==PNA) (P.map (pcompare x) xs) && isAntichain xs

-------------------

-- | An element of a graded lattice has a unique predecessor.
--
-- See <https://en.wikipedia.org/wiki/Graded_poset wikipedia> for more details.
class Lattice b => Graded b where
    type Nat b :: Type
    -- | Algebrists typically call this function the "rank" of the element in the poset;
    -- however we use the name from the standard prelude instead
    fromEnum :: Natural (BInt b) => b -> (BInt b)

    -- | The predecessor in the ordering
    pred :: b -> b

    -- | Repeatedly apply the "pred" function
    predN :: (Natural' (BInt b), P.Show (BInt b))  => (BInt b) -> b -> b
    predN i b =
        if i  < zero 
            then P.error $ "predN called on negative number " ++ (P.show i)
            else if i == zero
                then b
                else predN (i-one) $ pred b

law_Graded_fromEnum :: (Lattice b, Graded b, Natural' (BInt b), ClassicalLogic (Logic b)) => b -> b -> Logic (BInt b)
law_Graded_fromEnum b1 b2 = 
    if (b1 <  b2)
        then (fromEnum b1 <  fromEnum b2)
        else if (b1 >  b2)
            then (fromEnum b1 >  fromEnum b2)
            else if (b1 == b2)
                then (fromEnum b1 == fromEnum b2)
                else True

law_Graded_pred :: (Graded b, Natural' (BInt b)) => b -> b -> Logic (BInt b)
law_Graded_pred b1 _ = fromEnum (pred b1) == fromEnum b1-one
                     || fromEnum (pred b1) == fromEnum b1

defn_Graded_predN :: (Graded b, Natural' (BInt b), P.Show (BInt b)) => (BInt b) -> b -> Logic b
defn_Graded_predN i b = ifThenElse (i < zero) true (go i b == predN i b)
    where
        go :: (Graded b, Natural' (BInt b)) => (BInt b) -> b -> b
        go i' b' = ifThenElse (i' == zero) b' (go (i'-one) $ pred b')

{-# INLINE (<.) #-}
(<.) :: Graded b => b -> b -> Logic b
b1 <. b2 = b1 == pred b2

-- | In a well founded ordering, every element (except possibly the "maxBound" if it exists) has a successor element.
-- We use the "Enum" to represent well founded orderings to maintain consistency with the standard Prelude.
--
-- See <http://ncatlab.org/nlab/show/well-founded+relation ncatlab> for more info.
class (Graded b, Ord b) => Enum b where
    -- | The next element in the ordering
    succ :: b -> b

    -- | Advance many elements into the ordering.
    -- This value may be negative to move backwards.
    succN :: Natural (BInt b) => (BInt b) -> b -> b
    succN i b = toEnum $ fromEnum b + i

    -- | Given an index (also called a rank) of an element, return the element
    toEnum :: Natural (BInt b) => (BInt b) -> b

law_Enum_toEnum :: (Enum b, Natural (BInt b)) => b -> Logic b
law_Enum_toEnum b = toEnum (fromEnum b) == b

law_Enum_succ :: (Enum b, Logic (BInt b) ~ Logic b, Natural (BInt b), Eq (BInt b)) => b -> Logic b
law_Enum_succ b1 = fromEnum (succ b1) == fromEnum b1+one
                || fromEnum (succ b1) == fromEnum b1

defn_Enum_succN :: (Enum b, Natural (BInt b)) => (BInt b) -> b -> Logic b
defn_Enum_succN i b = succN i b == toEnum (fromEnum b + i)


{-# INLINE (>.) #-}
(>.) :: Enum b => b -> b -> Logic b
b1 >. b2 = b1 == succ b2

---------------------------------------

-- | This is the class of total orderings.
--
-- See https://en.wikipedia.org/wiki/Total_order
class Lattice a => Ord a where
    -- FIXME: what else instead of Ordering?
    compare :: a -> a -> Ordering
    default compare :: (Logic a ~ Bool) => a -> a -> Ordering
    compare a1 a2 = case pcompare a1 a2 of
        PLT -> LT
        PGT -> GT
        PEQ -> EQ
        PNA -> P.error "PNA given by pcompare on a totally ordered type"

law_Ord_totality :: Ord a => a -> a -> Logic a
law_Ord_totality a1 a2 = a1 <= a2 || a2 <= a1

law_Ord_min :: Ord a => a -> a -> Logic a
law_Ord_min a1 a2 = min a1 a2 == a1
                 || min a1 a2 == a2

law_Ord_max :: Ord a => a -> a -> Logic a
law_Ord_max a1 a2 = max a1 a2 == a1
                 || max a1 a2 == a2

{-# INLINE min #-}
min :: Ord a => a -> a -> a
min = inf

{-# INLINE max #-}
max :: Ord a => a -> a -> a
max = sup

-- instance Ord ()
instance Ord Char      where compare = P.compare ; {-# INLINE compare #-}
instance Ord Int       where compare = P.compare ; {-# INLINE compare #-}
instance Ord P.Integer   where compare = P.compare ; {-# INLINE compare #-}
instance Ord Float     where compare = P.compare ; {-# INLINE compare #-}
instance Ord Double    where compare = P.compare ; {-# INLINE compare #-}
instance Ord Rational  where compare = P.compare ; {-# INLINE compare #-}
instance Ord Bool      where compare = P.compare ; {-# INLINE compare #-}

-------------------

-- | A Bounded lattice is a lattice with both a minimum and maximum element
--
class (Lattice b, MinBound b) => Bounded b where
    maxBound :: b

law_Bounded_sup :: Bounded b => b -> Logic b
law_Bounded_sup b = sup b maxBound == maxBound

-- | "true" is an lower bound because `a && true = true` for all a.
{-# INLINE true #-}
true :: Bounded b => b
true = maxBound

{-# INLINE otherwise #-}
otherwise :: Bounded b => b
otherwise = true

instance Bounded ()     where maxBound = ()         ; {-# INLINE maxBound #-}
instance Bounded Bool   where maxBound = P.True     ; {-# INLINE maxBound #-}
instance Bounded Char   where maxBound = P.maxBound ; {-# INLINE maxBound #-}
instance Bounded Int    where maxBound = P.maxBound ; {-# INLINE maxBound #-}
instance {-# OVERLAPPABLE #-}(UpperBoundedField a, Lattice a, MinBound a) => Bounded a where maxBound = infinity ; {-# INLINE maxBound #-}

instance Bounded b => Bounded (a -> b) where
    {-# INLINE maxBound #-}
    maxBound = \_ -> maxBound

--------------------

class Bounded b => Complemented b where
    not :: b -> b

law_Complemented_not :: Complemented b => b -> Logic b
law_Complemented_not b = not (true  `P.asTypeOf` b) == false
                      && not (false `P.asTypeOf` b) == true

instance Complemented ()   where
    {-# INLINE not #-}
    not () = ()

instance Complemented Bool where
    {-# INLINE not #-}
    not = P.not

instance Complemented b => Complemented (a -> b) where
    {-# INLINE not #-}
    not f = \x -> not $ f x

-- | Heyting algebras are lattices that support implication, but not necessarily the law of excluded middle.
--
-- ==== Laws
-- There is a single, simple law that Heyting algebras must satisfy:
--
-- prop> a ==> b = c   ===>   a && c < b
--
-- ==== Theorems
-- From the laws, we automatically get the properties of:
--
-- distributivity
--
-- See <https://en.wikipedia.org/wiki/Heyting_algebra wikipedia> for more details.
--
-- Note that while Heyting algebras are abelian semigroups with respect to &&, they are not in general cancellative.
class Bounded b => Heyting b where
    -- | FIXME: think carefully about infix
    infixl 3 ==>
    (==>) :: b -> b -> b

law_Heyting_maxbound :: Heyting b => b -> Logic b
law_Heyting_maxbound b = (b ==> b) == maxBound

law_Heyting_infleft :: Heyting b => b -> b -> Logic b
law_Heyting_infleft b1 b2 = (b1 && (b1 ==> b2)) == (b1 && b2)

law_Heyting_infright :: Heyting b => b -> b -> Logic b
law_Heyting_infright b1 b2 = (b2 && (b1 ==> b2)) == b2

law_Heyting_distributive :: Heyting b => b -> b -> b -> Logic b
law_Heyting_distributive b1 b2 b3 = (b1 ==> (b2 && b3)) == ((b1 ==> b2) && (b1 ==> b3))

-- | FIXME: add the axioms for intuitionist logic, which are theorems based on these laws
--

-- | Modus ponens gives us a default definition for "==>" in a "Boolean" algebra.
-- This formula is guaranteed to not work in a "Heyting" algebra that is not "Boolean".
--
-- See <https://en.wikipedia.org/wiki/Modus_ponens wikipedia> for more details.
modusPonens :: Boolean b => b -> b -> b
modusPonens b1 b2 = not b1 || b2

instance Heyting ()   where
    {-# INLINE (==>) #-}
    () ==> () = ()

instance Heyting Bool where
    {-# INLINE (==>) #-}
    (==>) = modusPonens

instance Heyting b => Heyting (a -> b) where
    {-# INLINE (==>) #-}
    (f==>g) a = f a ==> g a

-- | Generalizes Boolean variables.
--
-- See <https://en.wikipedia.org/wiki/Boolean_algebra_%28structure%29 wikipedia> for more details.
class (Complemented b, Heyting b) => Boolean b where

law_Boolean_infcomplement :: Boolean b => b -> Logic b
law_Boolean_infcomplement b = (b || not b) == true

law_Boolean_supcomplement :: Boolean b => b -> Logic b
law_Boolean_supcomplement b = (b && not b) == false

law_Boolean_infdistributivity :: Boolean b => b -> b -> b -> Logic b
law_Boolean_infdistributivity b1 b2 b3 = (b1 || (b2 && b3)) == ((b1 || b2) && (b1 || b3))

law_Boolean_supdistributivity :: Boolean b => b -> b -> b -> Logic b
law_Boolean_supdistributivity b1 b2 b3 = (b1 && (b2 || b3)) == ((b1 && b2) || (b1 && b3))

instance Boolean ()
instance Boolean Bool
instance Boolean b => Boolean (a -> b)
instance (Boolean a, Boolean b) => Boolean (a,b)

type instance Logic (a,b) = (Logic a, Logic b)

instance (Eq a, Eq b) => Eq (a,b) where
    (==) (a1,b1) (a2,b2) = (a1==a2,b1==b2)

type instance Elem (a,b) = (Elem a, Elem b)

type instance Logic (a,b,c) = Logic a
type instance Elem (a,b) = (Elem a, Elem b)

instance (Container a, Container b) => Container (a,b) where
    elem (ea,eb) (a,b) = (elem ea a, elem eb b)

instance (POrd a, POrd b) => POrd (a,b) where
    inf (a1,b1) (a2,b2) = (inf a1 a2, inf b1 b2)

instance (MinBound a, MinBound b) => MinBound (a,b) where
    minBound = (minBound,minBound)

instance (Lattice a, Lattice b) => Lattice (a,b) where
    sup (a1,b1) (a2,b2) = (sup a1 a2, sup b1 b2)

instance (Bounded a, Bounded b) => Bounded (a,b) where
    maxBound = (maxBound,maxBound)

instance (Complemented a, Complemented b) => Complemented (a,b) where
    not (a,b) = (not a, not b)

instance (Heyting a, Heyting b) => Heyting (a,b) where
    (==>) (a1,b1) (a2,b2) = (a1==>a2, b1==>b2)

instance (Eq a, Eq b, Eq c, Logic a ~ Logic b, Logic b~Logic c) => Eq (a,b,c) where
    (a1,b1,c1)==(a2,b2,c2) = a1==a2 && b1==b2 && c1==c2