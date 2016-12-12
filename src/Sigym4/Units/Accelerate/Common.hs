{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module Sigym4.Units.Accelerate.Common (deriveQE) where

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

type instance Units       (Exp a) = Units       a
type instance MachineType (Exp a) = Exp (MachineType a)

instance
  ( a ~ Plain a
  , P.Num (Exp a)
  , P.Floating (Exp a)
  , P.Floating a
  , A.Lift Exp a
  )
  => HasUnits (Exp (Quantity u a))
  where
  p *~ u = toQE (p A.* A.lift u')
    where u' = approximateValue (exactValue u) :: a
  p /~ u = fromQE p A./ A.lift u'
    where u' = approximateValue (exactValue u) :: a


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

class
  ( Coercible t a
  , HasUnits (Exp a)
  , MachineType t ~ MachineType a
  , Units t ~ Units a
  , a ~ Plain a
  ) => CoercibleExp t a | t -> a where
  coerceExp :: Exp a -> Exp t
  coerceExp = unsafeCoerce

  coerceMExp :: t -> MachineType (Exp t) -> MachineType (Exp a)
  coerceMExp _ = unsafeCoerce :: MachineType (Exp t) -> MachineType (Exp a)

  unCoerceExp :: Exp t -> Exp a
  unCoerceExp = unsafeCoerce

  unCoerceMExp :: t -> MachineType (Exp a) -> MachineType (Exp t)
  unCoerceMExp _ = unsafeCoerce :: MachineType (Exp a) -> MachineType (Exp t)

ntMulU
  :: forall t a.
  ( CoercibleExp t a
  )
  => Exp (MachineType t) -> Units t -> Exp t
ntMulU p u =
  (coerceExp :: Exp a -> Exp t)
  ((coerceMExp (undefined :: t) :: MachineType (Exp t) -> MachineType (Exp a)) p U.*~ u)


ntDivU
  :: forall t a.
  ( CoercibleExp t a
  )
  => Exp t -> Units t -> Exp (MachineType t)
ntDivU p u =
  (unCoerceMExp (undefined :: t) :: Exp (MachineType a) -> Exp (MachineType t))
  ((unCoerceExp :: Exp t -> Exp a) p U./~ u)

toQE :: Exp a -> Exp (Quantity u a)
toQE = unsafeCoerce

fromQE :: Exp (Quantity u a) -> Exp a
fromQE = unsafeCoerce

toQ :: a -> Quantity u a
toQ = unsafeCoerce

fromQ :: Quantity u a -> a
fromQ = unsafeCoerce

liftNewtype :: forall t a. (Lift Exp a, CoercibleExp t a) => t -> Exp t
liftNewtype = coerceExp . lift . (coerce :: t -> a)
{-# INLINE liftNewtype #-}

-- | Derives 'Elt', 'Lift Exp' and 'HasUnits' for a monomorphic
-- newtype of a 'Quantity'.
--
-- Usage:
-- >>> newtype AirTemperature = AirTemperature (DP.Temperature Double)
-- >>> deriveQE [t| AirTemperature -> DP.Temperature Double|]
deriveQE :: TypeQ -> DecsQ
deriveQE ta = ta >>= \case
  AppT (AppT ArrowT t') a' ->
    let t = return t'; a = return a'
    in [d|deriving instance Elt $t
          type instance EltRepr $t = EltRepr $a
          instance Lift Exp $t where {type Plain $t = $t; lift = liftNewtype}
          instance HasUnits (Exp $t) where {(*~)=ntMulU; (/~)=ntDivU}
          instance CoercibleExp $t $a|]
  _ -> fail "deriveQE expects a type of the form: \"NewType -> UnderlyingType\""

