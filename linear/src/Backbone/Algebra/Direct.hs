{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Backbone.Algebra.Direct
    ()
where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           NumHask.Algebra.Integral
import           Backbone.Algebra.Abstract.Module
import           Backbone.Algebra.Abstract.VectorSpace
import           Backbone.Data.Representation
import           Backbone.Algebra.Linear.Hadamard
import           Backbone.TH.TH
import           Language.Haskell.TH     hiding ( Type )
import           Data.Coerce
import           Unsafe.Coerce
import           Prelude                 hiding ( (*)
                                                , Semigroup
                                                , Integral
                                                )
import           GHC.Types               hiding ( Module(..) )

-- TODO: not expressible yet! Need container-infrastructure!
-- newtype Direct (container :: m) (meta :: [m]) (shape :: Rep [k])
--     = Direct {unwrapDirect :: ContainerRep container (Rep shape)}

-- type instance Scalar (Direct c m s) = Scalar s
-- type instance Direct r prod >< Direct r prod' = Direct r (ZipWithTensor prod prod')

--TODO also provide more general repeated (arbirary rep kind)
--TODO also need container for instances! TODO: kind [Nat] in times?
newtype Repeated (times :: Nat) (meta :: m) (z :: RepShape k) =
    Repeated {unwrapRepeated' :: Default meta (Append times z)}

deriving instance Show (Default meta (Rep z (t ': shape))) => Show (Repeated t meta (Rep z shape))
deriving instance Eq (Default meta (Rep z (t ': shape))) => Eq (Repeated t meta (Rep z shape))

type family Append (t :: Nat) (z :: RepShape k) = (r :: RepShape k) | r -> t z where
    Append t (Rep z shape) = (Rep z (t ': shape))

$(runQ [t|forall t m z. Default m (Append t z) |] >>= (genNum' ''Repeated ["t", "m"] []))

--easier on the eyes
unwrapRepeated :: Repeated times meta (Rep rep shape) -> Default meta (Rep rep (times ': shape))
unwrapRepeated = unwrapRepeated'

type instance Scalar (Repeated times meta z) = Scalar (Default meta z)
--FIXME: wrong!
type instance (Repeated times meta z) >< (Repeated times meta z) = (Default meta z) >< (Default meta z)

