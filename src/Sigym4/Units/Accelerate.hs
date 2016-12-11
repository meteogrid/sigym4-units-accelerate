{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Units.Accelerate() where

import           Sigym4.Units as U
import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.Smart as A
import           Data.Array.Accelerate.Product as A
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.Typeable
import           Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Coercion
import           Prelude as P

type instance Units       (Exp a) = Units       a
type instance MachineType (Exp a) = Exp (MachineType a)

instance
  ( HasUnits a
  , Plain a ~ a
  , Plain (MachineType a) ~ MachineType a
  , Unlift Exp a
  , Unlift Exp (MachineType a)
  )
  => HasUnits (Exp a)
  where
  p *~ u = lift1 ((U.*~ u) :: MachineType a -> a)             p
  p /~ u = lift1 ((U./~ u) :: a             -> MachineType a) p
  {-# INLINE (*~) #-}
  {-# INLINE (/~) #-}

type instance EltRepr (DP.Quantity u a) = EltRepr a

instance (Real a, Elt a, Typeable u, HasDimension (Proxy u)) => Elt (DP.Quantity u a) where
  eltType _ = eltType (undefined :: a)
  toElt = Quantity . toElt
  fromElt = fromElt . unQuantity

instance cst a => IsProduct cst (DP.Quantity u a) where
  type ProdRepr (DP.Quantity u a) = ((), a)
  fromProd _ x = ((), unQuantity x)
  toProd _ ((), x) = Quantity x
  prod _ _ = ProdRsnoc ProdRunit

instance
  ( Lift Exp a
  , Elt (Plain a)
  , Real (Plain a)
  , Typeable u
  , HasDimension (Proxy u)
  ) => Lift Exp (DP.Quantity u a)
  where
  type Plain (DP.Quantity u a) = Quantity u (Plain a)
  lift x = Exp . Tuple $ NilTup `SnocTup` lift (unQuantity x)

instance
  ( Elt a
  , Real a
  , Typeable u
  , HasDimension (Proxy u)
  ) => Unlift Exp (DP.Quantity u (Exp a)) where
  unlift t = Quantity $ Exp $ ZeroTupIdx `Prj` t
