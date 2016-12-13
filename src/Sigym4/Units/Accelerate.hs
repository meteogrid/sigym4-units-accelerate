{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Sigym4.Units.Accelerate (deriveQE) where

import           Sigym4.Units.Accelerate.Internal (deriveQE)
import           Sigym4.Units
import           Sigym4.Units.Meteo

deriveQE [t|forall a. (Real a, Floating a, Eq a) => Distance   a -> Length a|]
deriveQE [t|forall a. (Real a, Floating a, Eq a) => Height     a -> Length a|]
deriveQE [t|forall a. (Real a, Floating a, Eq a) => Ratio      a -> Dimensionless a|]
deriveQE [t|forall a. (Real a, Floating a, Eq a) => NormRatio  a -> Dimensionless a|]
deriveQE [t|forall a. (Real a, Floating a, Eq a) => CloudCover a -> NormRatio a|]
