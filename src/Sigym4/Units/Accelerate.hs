{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Units.Accelerate() where

import           Sigym4.Units as U
import           Sigym4.Units.Meteo as U
import           Control.Newtype
import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.ExactPi (approximateValue)
import           Data.Typeable
import           Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Coercion
import           Prelude as P
import           Unsafe.Coerce (unsafeCoerce)

type instance Units       (Exp a) = Units       a
type instance MachineType (Exp a) = Exp (MachineType a)

instance
  ( Units q ~ (DP.Unit k u (MachineType q))
  , (MachineType q) ~ Plain (MachineType q)
  , P.Num (Exp (MachineType q))
  , P.Floating (Exp (MachineType q))
  )
  => HasUnits (Exp q)
  where
  p *~ u = (unsafeCoerce :: Exp (MachineType q) -> Exp q) (p A.* approximateValue (exactValue u))
  p /~ u = (unsafeCoerce :: Exp q -> Exp (MachineType q)) p A./ approximateValue (exactValue u)

type instance EltRepr (DP.Quantity u a) = EltRepr a

instance (Real a, Elt a, Typeable u, HasDimension (Proxy u)) => Elt (DP.Quantity u a) where
  eltType _ = eltType (undefined :: a)
  toElt = Quantity . toElt
  fromElt = fromElt . unQuantity

instance Lift Exp a => Lift Exp (DP.Quantity u a)
  where
  type Plain (DP.Quantity u a) = DP.Quantity u a
  lift = (unsafeCoerce :: Exp (Plain a) -> Exp (Quantity u a)) . lift . unQuantity


deriving instance Elt Distance
type instance EltRepr Distance = EltRepr (DP.Length Double)
instance Lift Exp Distance where
  type Plain Distance = Distance
  lift = (unsafeCoerce :: Exp (Plain (Length Double)) -> Exp Distance) . lift . unpack

deriving instance Elt Height
type instance EltRepr Height = EltRepr (DP.Length Double)
instance Lift Exp Height where
  type Plain Height = Height
  lift = (unsafeCoerce :: Exp (Plain (Length Double)) -> Exp Height) . lift . unpack

deriving instance Elt Ratio
type instance EltRepr Ratio = EltRepr (DP.Dimensionless Double)
instance Lift Exp Ratio where
  type Plain Ratio = Ratio
  lift = (unsafeCoerce :: Exp (Plain (Dimensionless Double)) -> Exp Ratio) . lift . unpack

deriving instance Elt NormRatio
type instance EltRepr NormRatio = EltRepr (DP.Dimensionless Double)
instance Lift Exp NormRatio where
  type Plain NormRatio = NormRatio
  lift = (unsafeCoerce :: Exp (Plain (Dimensionless Double)) -> Exp NormRatio) . lift . unpack

deriving instance Elt CloudCover
type instance EltRepr CloudCover = EltRepr (DP.Dimensionless Double)
instance Lift Exp CloudCover where
  type Plain CloudCover = CloudCover
  lift = (unsafeCoerce :: Exp (Plain NormRatio) -> Exp CloudCover) . lift . unpack
