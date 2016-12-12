{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Sigym4.Units.Accelerate.Common (deriveQE) where

import           Sigym4.Units as U
import           Control.Newtype
import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.Smart (Exp(..))
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.Coerce
import           Data.Typeable
import           Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Internal as DP
import           Numeric.Units.Dimensional.Coercion as DP
import           Prelude as P
import           Language.Haskell.TH hiding (Exp)
import           Unsafe.Coerce (unsafeCoerce)

type instance Units       (Exp a) = Units       a
type instance MachineType (Exp a) = Exp (MachineType a)

instance
  (
    a ~ Plain a
  , P.Num (Exp a)
  , P.Floating (Exp a)
  , A.Lift Exp a
  )
  => HasUnits (Exp (Quantity u a))
  where
  p *~ Unit _ _ u = toQE (p A.* A.lift u)
  p /~ Unit _ _ u = fromQE p A./ A.lift u


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




ntMulU
  :: forall t a.
  ( HasUnits (Exp a)
  , Coercible t a
  , Newtype t a -- GHC thinks its redundant but it propagates the fundep a -> t
  , MachineType t ~ MachineType a
  , Units t ~ Units a
  )
  => Exp (MachineType t) -> Units t -> Exp t
ntMulU p u =
  (unsafeCoerce :: Exp a -> Exp t)
  ((unsafeCoerce :: MachineType (Exp t) -> MachineType (Exp a)) p U.*~ u)

ntDivU
  :: forall t a.
  ( HasUnits (Exp a)
  , Coercible t a
  , Newtype t a -- GHC thinks its redundant but it propagates the fundep a -> t
  , MachineType t ~ MachineType a
  , Units t ~ Units a
  )
  => Exp t -> Units t -> Exp (MachineType t)
ntDivU p u =
  (unsafeCoerce :: Exp (MachineType a) -> Exp (MachineType t))
  ((unsafeCoerce :: Exp t -> Exp a) p U./~ u)

toQE :: Exp a -> Exp (Quantity u a)
toQE = unsafeCoerce

fromQE :: Exp (Quantity u a) -> Exp a
fromQE = unsafeCoerce

toQ :: a -> Quantity u a
toQ = unsafeCoerce

fromQ :: Quantity u a -> a
fromQ = unsafeCoerce

liftNewtype :: forall t a. (Lift Exp a, Newtype t a) => t -> Exp t
liftNewtype = (unsafeCoerce :: Exp (Plain a) -> Exp t) . lift . unpack
{-# INLINE liftNewtype #-}

-- | Derives 'Elt', 'Lift Exp' and 'HasUnits' for a monomorphic
-- newtype of a 'Quantity'.
--
-- Usage:
-- >>> newtype AirTemperature = AirTemperature (DP.Temperature Double)
-- >>> deriveQE [t| AirTemperature -> DP.Temperature Double|]
deriveQE :: TypeQ -> DecsQ
deriveQE t = do
  AppT (AppT ArrowT n') a' <- t
  let n = return n'; a = return a'
  [d|
    deriving instance Elt $n
    type instance EltRepr $n = EltRepr $a
    instance Lift Exp $n where {type Plain $n = $n; lift = liftNewtype}
    instance HasUnits (Exp $n) where {(*~)=ntMulU; (/~)=ntDivU}
    |]

