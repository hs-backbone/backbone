{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operations
module Backbone.Algebra.Linear.Hadamard
  ( 
      HadamardMultiplication(..)
    , Hadamard(..)
  ) where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import Backbone.Algebra.Abstract.Module

-- | element by element multiplication
--
-- > (a .*. b) .*. c == a .*. (b .*. c)
-- > singleton one .*. a = a
-- > a .*. singelton one = a
-- > a .*. b == b .*. a
class (Multiplication (Scalar m), Module m) =>
  HadamardMultiplication m where
  infixl 7 .*.
  (.*.) :: m -> m -> m

-- | element by element division
--
-- > a ./. a == singleton one
class (Group (Product (Scalar m)), HadamardMultiplication m) =>
  Hadamard m where
  infixl 7 ./.
  (./.) :: m -> m -> m