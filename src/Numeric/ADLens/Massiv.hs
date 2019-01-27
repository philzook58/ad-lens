{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, RankNTypes #-}
module Numeric.ADLens.Massiv where
-- import Numeric.ADLens.Lens
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow ((***))
import Data.Functor.Const