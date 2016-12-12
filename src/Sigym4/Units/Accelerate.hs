{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
