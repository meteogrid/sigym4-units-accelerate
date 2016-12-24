-- Para los tests de cosas que no deben compilar
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Units.AccelerateSpec ( spec, main ) where

import           Sigym4.Units.Accelerate

import           Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
import           Data.List
import           Data.Word (Word8)

import           Test.Hspec
import           Test.ShouldNotTypecheck (shouldNotTypecheck)
import           Prelude as P

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "works" $ do
    let asMeters = 2 *~ weaken meter :: Length (Exp Double)
        asFeet   = asMeters /~ foot
    show (CPU.run (unit asFeet)) `shouldSatisfy` isInfixOf "6.5616"


