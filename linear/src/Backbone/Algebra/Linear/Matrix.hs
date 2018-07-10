{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Backbone.Algebra.Linear.Matrix
    ( MatMul(..)
    )
where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           Backbone.Algebra.Direct
import           Backbone.Data.Representation
import           Backbone.Algebra.Abstract.Module
import           Prelude                 hiding ( (*) )
import           GHC.Types               hiding ( Module(..) )

class Field a => MatMul rep a where
    infixl 7 |*|
    (|*|) :: (cont ~ (ElCont rep a)
            , Scalar (f (Rep cont '[m, m'])) ~ Scalar (f' (Rep cont '[m, m']))
            , Scalar (f (Rep cont '[m, m'])) ~ a)
        => f  (Rep cont '[m, m'])
        -> f' (Rep cont '[m', m''])
        -> f  (Rep cont '[m, m''])