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
{-# LANGUAGE Trustworthy #-}
module Sigym4.Units.Accelerate (
  (*~)
, (/~)
, module Sigym4.Units.Meteo
, module Sigym4.Units
) where

import           Sigym4.Units.Accelerate.Internal ( (*~), (/~) )
import           Sigym4.Units hiding  ( (*~), (/~) )
import           Sigym4.Units.Meteo
