-- Para los tests de cosas que no deben compilar
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Sigym4.Units.Accelerate.CommonSpec ( spec, main ) where

import           Sigym4.Units
import           Sigym4.Units.Accelerate ()
import           Sigym4.Units.Accelerate.Common (deriveQE)
import           Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
import           Data.List
import           Control.Newtype
import           Unsafe.Coerce

import           Test.Hspec
import           Test.ShouldNotTypecheck (shouldNotTypecheck)
import           Prelude as P

main :: IO ()
main = hspec spec




spec :: Spec
spec =
  it "does not allow deriving unsafe Exp Quantity newtype instances" $ shouldNotTypecheck $
    let asMeters = A.lift (Dangerous 2) :: Exp Dangerous
        asFeet   = asMeters /~ foot :: Exp Double
    in show (CPU.run (unit asFeet))

  it "works for valid newtypes" $ do
    let asMeters = A.lift (Distance (2 *~ weaken meter)) :: Exp Distance
        asFeet   = asMeters /~ foot :: Exp Double
    show (CPU.run (unit asFeet)) `shouldSatisfy` isInfixOf "6.5616"


newtype Dangerous = Dangerous Int
type instance MachineType Dangerous = MachineType Distance
type instance Units Dangerous = Units Distance
-- Broken and unsafe instance of Newtype
instance Newtype Dangerous Distance where
  pack = unsafeCoerce
  unpack = unsafeCoerce


deriveQE [t|Dangerous -> Distance|]


