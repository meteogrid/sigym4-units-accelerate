{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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
module Sigym4.Units.Accelerate.Internal ( (*~), (/~) ) where

import           Sigym4.Units as U  hiding  ( (*~), (/~) )
import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.Typeable
import           Numeric.Units.Dimensional (HasDimension)
import qualified Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Coercion as DP
import           Prelude as P
import           Unsafe.Coerce (unsafeCoerce)

infixl 7 *~
(*~) :: forall d k a. (P.Num (Exp a), Elt a) => Exp a -> Unit k d (Exp a) -> Exp (Quantity d a)
a *~ u = A.lift1 ((DP.*~ u) :: Exp a -> Quantity d (Exp a)) a

infixl 7 /~
(/~) :: forall d k a. (P.Fractional (Exp a), Elt a) => Exp (Quantity d a) -> Unit k d (Exp a) -> Exp a
a /~ u = A.lift1 ((DP./~ u) :: Quantity d (Exp a) -> Exp a) a



type instance EltRepr (DP.Quantity u a) = EltRepr a

instance
  ( Real a
  , Elt a
  , Typeable u
  , HasDimension (Proxy u)
  ) => Elt (DP.Quantity u a)
  where
  eltType _ = eltType (undefined :: a)
  toElt     = Quantity . toElt
  fromElt   = fromElt . unQuantity

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quantity d a) where
  type Plain (DP.Quantity d a) = Quantity d (Plain a)
  lift = (unsafeCoerce :: Exp (Plain a) -> Exp (Quantity d (Plain a)))
       . lift
       . unQuantity


instance (Unlift Exp a, Elt (Plain a)) => Unlift Exp (Quantity d a) where
  unlift = Quantity . unlift . (unsafeCoerce :: Exp (Quantity d (Plain a)) -> Exp (Plain a))
