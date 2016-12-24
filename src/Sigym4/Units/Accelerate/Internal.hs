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
(*~) :: forall k d a. (P.Num (Exp a), P.Floating a, A.Lift Exp a, a ~ Plain a)
     => Exp a
     -> Unit k d a
     -> Quantity d (Exp a)
p *~ u = coerce (p A.* A.lift u')
  where u' = approximateValue (exactValue u) :: a


infixl 7 /~
(/~) :: forall k d a. (P.Fractional (Exp a), P.Floating a, A.Lift Exp a, a ~ Plain a)
     => Quantity d (Exp a)
     -> Unit k d a
     -> Exp a
p /~ u = coerce p A./ A.lift u'
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
  toElt     = Quantity . toElt
  fromElt   = fromElt . unQuantity

instance cst a => IsProduct cst (Quantity u a) where
  type ProdRepr (Quantity u a) = ((), a)
  fromProd _ x = ((), unQuantity x)
  toProd _ ((), x) = Quantity x
  prod _ _ = ProdRsnoc ProdRunit

instance (Real (Plain a), Lift Exp a, Elt (Plain a), Typeable u, HasDimension (Proxy u))
  => Lift Exp (DP.Quantity u a) where
  type Plain (DP.Quantity u a) = DP.Quantity u (Plain a)
  lift x = Exp . Tuple $ NilTup `SnocTup` lift (unQuantity x)

instance (Real a, Elt a, Typeable u, HasDimension (Proxy u))
  => Unlift Exp (Quantity u (Exp a)) where
  unlift t = Quantity $ Exp $ ZeroTupIdx `Prj` t
