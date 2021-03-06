{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module contains any objects relating to order theory
module Backbone.Algebra.Ord where

import qualified Prelude                       as P
import qualified Data.List                     as L

import qualified GHC.Arr                       as Arr
-- import           Data.Array.ST           hiding ( freeze
--                                                 , thaw
--                                                 )
import           Control.Monad
import           Control.Monad.Random
import           Prelude                        ( take )
import           Foreign.Storable

import           Backbone.Algebra.Logic.Logic
import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Additive
import           NumHask.Algebra.Abstract.Multiplicative
import           Data.Array.ST
import           Data.Coerce

--------------------------------------------------------------------------------

instance {-#OVERLAPS#-} (Classical Eq a, Logic a ~ P.Bool) => P.Eq a where
    (==) = (==)

instance {-#OVERLAPS#-} (Classical Ord a, Logic a ~ P.Bool) => P.Ord a where
    compare = compare

-- | This wrapper let's us convert between SubHask's Ord type and the Prelude's.
-- See the "sort" function below for an example.
--
-- FIXME:
-- This should be removed.
-- The overlapping instances above are easier to use.
newtype WithPreludeOrd a = WithPreludeOrd { unWithPreludeOrd :: a }
    deriving Storable

instance P.Show a => P.Show (WithPreludeOrd a) where
    show (WithPreludeOrd a) = P.show a

-- | FIXME: for some reason, our deriving mechanism doesn't work on Show here;
-- It causes's Set's show to enter an infinite loop
-- FIXME: uncomment
-- deriveHierarchyFiltered ''WithPreludeOrd [ ''Eq, ''Enum, ''Boolean, ''Ring, ''Metric] [ ''Show ]

instance (Eq a, ClassicalLogic a, Logic a ~ P.Bool) => P.Eq (WithPreludeOrd a) where
    {-# INLINE (==) #-}
    (==) = coerce @(a -> a -> P.Bool) @(WithPreludeOrd a -> WithPreludeOrd a -> P.Bool) (==)

instance (Ord a, ClassicalLogic a, Logic a ~ P.Bool) => P.Ord (WithPreludeOrd a) where
    {-# INLINE (<=) #-}
    (<=) = coerce @(a -> a -> P.Bool) @(WithPreludeOrd a -> WithPreludeOrd a -> P.Bool) (<=)

-- | A wrapper around the Prelude's sort function.
--
-- FIXME:
-- We should put this in the container hierarchy so we can sort any data type
-- sort :: (Ord a, ClassicalLogic a) => [a] -> [a]
-- sort = map unWithPreludeOrd . L.sort . map WithPreludeOrd

-- | Randomly shuffles a list in time O(n log n).
-- See http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
    let l = L.length xs
    rands <- take l `liftM` getRandomRs (0, l - 1)
    let ar = runSTArray
            (do
                ar' <- Arr.thawSTArray (Arr.listArray (0, l - 1) xs)
                forM_ (L.zip [0 .. (l - 1)] rands) P.$ \(i, j) -> do
                    vi <- Arr.readSTArray ar' i
                    vj <- Arr.readSTArray ar' j
                    Arr.writeSTArray ar' j vi
                    Arr.writeSTArray ar' i vj
                return ar'
            )
    return (Arr.elems ar)
