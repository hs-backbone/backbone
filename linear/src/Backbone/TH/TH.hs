{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Backbone.TH.TH
  ( group
  , ring
  , field
  , hadamard
  , genAll
  , genAll'
  , genNum'
  , doGen
  )
where

import           NumHask.Algebra.Abstract.Group
                                               as NH
import           NumHask.Algebra.Abstract.Ring as NH
import           NumHask.Algebra.Abstract.Field
                                               as NH
import           NumHask.Algebra.Abstract.Multiplicative
                                               as NH
import           NumHask.Algebra.Abstract.Additive
                                               as NH
import           Backbone.Algebra.Linear.Basis
import           Backbone.Algebra.Linear.Hadamard
import           Backbone.Algebra.Abstract.Module
import           Prelude                 hiding ( recip
                                                , Semigroup
                                                , properFraction
                                                , round
                                                , floor
                                                , ceiling
                                                )
import           GHC.Types               hiding ( Module(..)
                                                , Type(..)
                                                )
import           Data.Coerce
import           Language.Haskell.TH
import           Data.Char
import           Data.Maybe
import           Control.Monad
import           Debug.Trace


data Class = Class (Type -> Type) Name [Type]
newtype Target = Target (Type -> Type)
-- | the base representation to coerce with and the variable
data BaseVar = BaseVar { base :: Type, var ::Type}

appendT :: Type -> Type -> Type
appendT x y = AppT x y

data Strategy = Normal Class | NormalInherit Class | CoerceI Class

mkNormalInherit x =
  NormalInherit (Class (appendT (ConT $ (mkName x))) (mkName x) [])
mkNormal x = Normal (Class (appendT (ConT $ (mkName x))) (mkName x) [])
mkCoerceI x =
  CoerceI (Class (appendT (ConT $ (mkName x))) (mkName x) [])

group :: [Strategy]
group =
  mkNormalInherit
    <$> [ "Magma"
        , "Unital"
        , "Semigroup"
        , "Commutative"
        , "Absorbing"
        , "Invertible"
        , "Idempotent"
        ]

ring :: [Strategy]
ring = mkNormal <$> ["Distributive", "IntegralDomain"]

field :: [Strategy]
field = norm ++ coerceI
 where
  norm =
    mkNormal
      <$> [ "Field"
          , "ExpField"
          , "UpperBoundedField"
          , "LowerBoundedField"
          , "BoundedField"
          , "TrigField"
          ]
  coerceI = mkQuotientField <$> ["QuotientField"]
  b       = (VarT (mkName "b"))
  mkQuotientField x =
    CoerceI $ Class (\y -> AppT (AppT (ConT $ (mkName x)) y) b) (mkName x) [b]

moduleS :: [Strategy]
moduleS = mkNormal <$> ["Cone", "Module"]

hadamard :: [Strategy]
hadamard = mkNormal <$> ["HadamardMultiplication", "Hadamard"]

allTypes = group ++ ring ++ field ++ moduleS ++ hadamard
numTypes = group ++ ring ++ field

genAll :: Name -> [String] -> Q [Dec]
genAll t apps = doGen allTypes target base
  where
      target = Target $ appendT $ ConT t
      base = (generateBaseType Nothing [] [])


-- | the type gets applied in the middle  
genAll' :: Name -> [String] -> [String] -> Type -> Q [Dec]
genAll' t apps apps' forallBase = doGen allTypes target base
 where
  appsN = map (VarT . mkName) apps
  appsN' = map (VarT . mkName) apps'
  applyFirst name = foldl AppT (ConT name)
  applySecond = foldl AppT
  target      = Target (\y -> applySecond (AppT (applyFirst t appsN) y) appsN')
  base = generateBaseType (Just forallBase) appsN appsN'

-- | the type gets applied in the middle  
genNum' :: Name -> [String] -> [String] -> Type -> Q [Dec]
genNum' t apps apps' forallBase = doGen numTypes target base
 where
  appsN = map (VarT . mkName) apps
  appsN' = map (VarT . mkName) apps'
  applyFirst name = foldl AppT (ConT name)
  applySecond = foldl AppT
  target      = Target (\y -> applySecond (AppT (applyFirst t appsN) y) appsN')
  base = generateBaseType (Just forallBase) appsN appsN'  

generateBaseType :: Maybe Type -> [Type] -> [Type] -> BaseVar
generateBaseType Nothing _ _ = let a = VarT (mkName "a") in BaseVar a a
generateBaseType (Just (ForallT varsT _ fktSig)) appsN appsN' = BaseVar replaced a
  where
    vars      = map (\(PlainTV n) -> n) varsT
    a = VarT (mkName "a")
    toReplcace = zip (appsN ++ (a : appsN')) vars
    replaced  = replaceAllVarT fktSig toReplcace

doGen :: [Strategy] -> Target -> BaseVar -> Q [Dec]
doGen xs target base = concat <$> mapM doGen' xs
 where
  doGen' (Normal        n) = (\a -> [a]) <$> (genNormalInstance n target base)
  doGen' (NormalInherit n) = normalAndInherit n target base
  doGen' (CoerceI       n) = (\a -> [a]) <$> (coerceInstanceDec n target base)

normalAndInherit :: Class -> Target -> BaseVar -> Q [Dec]
normalAndInherit className target base = do
  normal <- genNormalInstance className target base
  inh    <- genInheritInstanceDec className target base
  return [normal, inh]

genNormalInstance :: Class -> Target -> BaseVar -> Q Dec
genNormalInstance (Class nfkt name ns) (Target t) (BaseVar base var) = return
  $ StandaloneDerivD Nothing [nfkt base] (nfkt (t var))

coerceInstanceDec :: Class -> Target -> BaseVar -> Q Dec
coerceInstanceDec (Class nfkt n ns) (Target t) (BaseVar base var) = do
  fkts <- getFunctions n
  decs <- coerceImpls fkts base (t var) ns
  return $ InstanceD Nothing [AppT (AppT EqualityT z) base,nfkt z] (nfkt (t var)) decs
  where
    z = VarT (mkName "z")

genInheritInstanceDec :: Class -> Target -> BaseVar -> Q Dec
genInheritInstanceDec (Class nfkt n ns) (Target t) (BaseVar base var) = do
  fkts <- getFunctions n
  decs <- coerceImpls fkts (AppT f base) (AppT f (t var)) ns
  return $ InstanceD Nothing
                     [from, AppT (AppT coercibleC (AppT f base)) (AppT f (t var))]
                     to
                     decs
 where
  from       = nfkt (AppT f base)
  to         = nfkt (AppT f (t var))
  coercibleC = ConT (mkName "Data.Coerce.Coercible")
  f          = VarT (mkName "f")

coerceImpls :: [(Name, Type)] -> Type -> Type -> [Type] -> Q [Dec]
coerceImpls fkts inherit to unchanged = do
  let cnm = mkName "Data.Coerce.coerce"
  return $ (coerceFkts to inherit unchanged cnm) <$> fkts

coerceFkts :: Type -> Type -> [Type] -> Name -> (Name, Type) -> Dec
coerceFkts to inherit unchanged cnm (name, t) = ValD pat body []
 where
  pat       = (VarP name)
  fktSig    = let (ForallT _ _ fktSig) = t in fktSig
  vars      = let (ForallT vars _ fktSig) = t in map (\(KindedTV n _) -> n) vars
  tos       = zip (to : unchanged) vars
  inherited = zip (inherit : unchanged) vars

  body           = NormalB (AppE coerceImp (VarE name))

  coerceImp      = AppTypeE
    (AppTypeE (VarE cnm) (replaceAllVarT fktSig inherited))
    (replaceAllVarT fktSig tos)

replaceAllVarT :: Type -> [(Type, Name)] -> Type
replaceAllVarT = foldr replaceVarT

replaceVarT :: (Type, Name) -> Type -> Type
replaceVarT with (AppT t1 t2) =
  AppT (replaceVarT with t1) (replaceVarT with t2)
replaceVarT (to, var) (VarT x) = if var == x then to else VarT x
replaceVarT with      x        = x



getFunctions :: Name -> Q [(Name, Type)]
getFunctions name = do
  (ClassI dec inst) <- reify name
  return (listFunction dec)
 where
  listFunction (ClassD _ _ _ _ decs) = catMaybes $ maybeSigD <$> decs

  maybeSigD (SigD name t) = Just (name, t)
  maybeSigD _             = Nothing
