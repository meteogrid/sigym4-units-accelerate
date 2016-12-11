{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sigym4.Units.AccelerateSpec ( spec, main ) where

import           Sigym4.Units
import           Sigym4.Units.Accelerate ()
import           Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
import           Data.List

import           Test.Hspec

main :: IO ()
main = hspec spec




spec :: Spec
spec =
  it "converts units" $ do
    let asMeters = A.lift (Distance (2 *~ weaken meter)) :: Exp Distance
        asFeet   = asMeters /~ foot :: Exp Double
    show (CPU.run (unit asFeet)) `shouldSatisfy` isInfixOf "6.5616"
