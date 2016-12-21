{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module Sigym4.Units.Accelerate.Internal (deriveQE) where

import           Sigym4.Units as U
import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.Smart (Exp(..))
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.Coerce
import           Data.ExactPi (approximateValue)
import           Data.Typeable
import           Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Coercion as DP
import           Prelude as P
import           Language.Haskell.TH hiding (Exp)
import           Unsafe.Coerce (unsafeCoerce)

type instance Units (Exp a) (Exp b) = Units a b

instance
  ( a ~ Plain a
  , P.Num (Exp a)
  , P.Floating (Exp a)
  , P.Floating a
  , A.Elt a
  )
  => HasUnits (Exp (Quantity u a)) (Exp a)
  where
  p *~ u = toQE (p A.* A.constant u')
    where u' = approximateValue (exactValue u) :: a
  {-# INLINE (*~) #-}
  p /~ u = fromQE p A./ A.constant u'
    where u' = approximateValue (exactValue u) :: a
  {-# INLINE (/~) #-}


type instance EltRepr (DP.Quantity u a) = EltRepr a

instance
  ( Real a
  , Elt a
  , Typeable u
  , HasDimension (Proxy u)
  ) => Elt (DP.Quantity u a)
  where
  eltType _ = eltType (undefined :: a)
  toElt     = toQ . toElt
  fromElt   = fromElt . fromQ

instance
  ( a ~ Plain a
  , Lift Exp a
  ) => Lift Exp (DP.Quantity u a)
  where
  type Plain (DP.Quantity u a) = DP.Quantity u a
  lift = toQE . lift . unQuantity
  {-# INLINE lift #-}

class
  ( Coercible t a
  , HasUnits (Exp a) (Exp m)
  , Units (Exp t) (Exp m) ~ Units t m
  , Units (Exp a) (Exp m) ~ Units t m
  , a ~ Plain a
  ) => CoercibleExp t a m | t -> a, t -> m where
  coerceExp :: Exp a -> Exp t
  coerceExp = unsafeCoerce
  {-# INLINE coerceExp #-}

  unCoerceExp :: Exp t -> Exp a
  unCoerceExp = unsafeCoerce
  {-# INLINE unCoerceExp #-}

ntMulU
  :: forall t a m. CoercibleExp t a m
  => Exp m -> Units (Exp t) (Exp m) -> Exp t
ntMulU p u = (coerceExp :: Exp a -> Exp t) (p U.*~ u)
{-# INLINE ntMulU #-}


ntDivU
  :: forall t a m. CoercibleExp t a m
  => Exp t -> Units (Exp t) (Exp m) -> Exp m
ntDivU p u = (unCoerceExp :: Exp t -> Exp a) p U./~ u
{-# INLINE ntDivU #-}

toQE :: Exp a -> Exp (Quantity u a)
toQE = unsafeCoerce
{-# INLINE toQE #-}

fromQE :: Exp (Quantity u a) -> Exp a
fromQE = unsafeCoerce
{-# INLINE fromQE #-}

toQ :: a -> Quantity u a
toQ = unsafeCoerce
{-# INLINE toQ #-}

fromQ :: Quantity u a -> a
fromQ = unsafeCoerce
{-# INLINE fromQ #-}

liftNewtype :: forall t a m. (Lift Exp a, CoercibleExp t a m) => t -> Exp t
liftNewtype = coerceExp . lift . (coerce :: t -> a)
{-# INLINE liftNewtype #-}

-- | Derives 'Elt', @'Lift' 'Exp'@ and 'HasUnits' for a newtype of 'Quantity'
--
-- Usage:
--
--  >>> deriveQE [t| forall a. Num a => AirTemperature a -> DP.Temperature a|]
--
--
deriveQE :: TypeQ -> DecsQ
deriveQE ta = ta >>= \case
  ForallT [tybind] cst (AppT (AppT ArrowT t') a') ->
    let sig = return $ foldl AppT (TupleT (P.length csts)) csts
        csts = cst P.++ P.map liftCst cst P.++ extraCst
        liftCst (AppT c x) = AppT c (AppT (ConT ''Exp) x)
        liftCst _          = error "expected a type application"
        extraCst =
          [ AppT (ConT ''Elt) ma'
          , EqualityT `AppT` (ConT ''Plain `AppT` ma') `AppT` ma'
          , EqualityT `AppT` (ConT ''Units `AppT` (ConT ''Exp `AppT` t') `AppT` (ConT ''Exp `AppT` ma'))
                      `AppT` (ConT ''Units `AppT` t' `AppT` ma')
          , EqualityT `AppT` (ConT ''Units `AppT` (ConT ''Exp `AppT` a') `AppT` (ConT ''Exp `AppT` ma'))
                      `AppT` (ConT ''Units `AppT` a' `AppT` ma')
          , AppT (AppT (ConT ''Lift) (ConT ''Exp)) ma'
          ]
        ma' = VarT mname
        t = return t'
        a = return a'
        mname = case tybind of {PlainTV x->x; KindedTV x _->x}
        ma = return ma'
    in [d|deriving instance $sig => Elt $t
          type instance EltRepr $t = EltRepr $a
          instance $sig => Lift Exp $t where {type Plain $t = $t; lift = liftNewtype}
          instance $sig => HasUnits (Exp $t) (Exp $ma) where {(*~)=ntMulU; (/~)=ntDivU}
          instance $sig => CoercibleExp $t $a $ma|]
  AppT (AppT ArrowT t') a'@(AppT _ ma') ->
    let t = return t'
        a = return a'
        ma = return ma'
    in [d|deriving instance Elt $t
          type instance EltRepr $t = EltRepr $a
          instance Lift Exp $t where {type Plain $t = $t; lift = liftNewtype}
          instance HasUnits (Exp $t) (Exp $ma) where {(*~)=ntMulU; (/~)=ntDivU}
          instance CoercibleExp $t $a $ma|]
  _ -> fail "deriveQE expects a type of the form: \"NewType -> UnderlyingType\""

