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
module Sigym4.Units.Accelerate() where

import           Sigym4.Units.Accelerate.Common (deriveQE)
import           Sigym4.Units as U
import           Sigym4.Units.Meteo as U
import           Numeric.Units.Dimensional as DP


deriveQE [t|Distance   -> DP.Length Double|]
deriveQE [t|Height     -> DP.Length Double|]
deriveQE [t|Ratio      -> DP.Dimensionless Double|]
deriveQE [t|NormRatio  -> DP.Dimensionless Double|]
deriveQE [t|CloudCover -> NormRatio|]
