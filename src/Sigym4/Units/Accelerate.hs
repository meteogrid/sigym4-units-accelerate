{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Sigym4.Units.Accelerate (deriveQE) where

import           Sigym4.Units.Accelerate.Internal (deriveQE)
import           Sigym4.Units
import           Sigym4.Units.Meteo

deriveQE [t|Distance   -> Length Double|]
deriveQE [t|Height     -> Length Double|]
deriveQE [t|Ratio      -> Dimensionless Double|]
deriveQE [t|NormRatio  -> Dimensionless Double|]
deriveQE [t|CloudCover -> NormRatio|]
