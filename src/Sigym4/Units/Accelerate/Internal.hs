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
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.Product
import           Data.Coerce
import           Data.ExactPi (approximateValue)
import           Data.Typeable
import           Numeric.Units.Dimensional (HasDimension, exactValue)
import qualified Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Coercion as DP
import           Prelude as P
import           Language.Haskell.TH hiding (Exp)
import           Unsafe.Coerce (unsafeCoerce)

infixl 7 *~
(*~) :: (Unlift Exp a, P.Num a, Elt (Plain a)) => Exp (Plain a) -> Unit k d a -> Exp (Plain (Quantity d a))
a *~ u = A.lift1 (DP.*~ u) a

infixl 7 /~
(/~) :: (Unlift Exp a, P.Fractional a, Elt (Plain a)) => Exp (Plain (Quantity d a)) -> Unit k d a -> Exp (Plain a)
a /~ u = A.lift1 (DP./~ u) a



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
