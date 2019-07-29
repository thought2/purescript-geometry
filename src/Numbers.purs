module Numbers where

import Prelude

import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, (!!))

remap :: Vec D2 Number -> Vec D2 Number -> Number -> Number
remap pair1 pair2 value =
  pivot2 + (pct * dist2)
  where
    pivot1 = pair1!!d0
    pivot2 = pair2!!d0
    dist1 = pair1!!d0 - pair1!!d1
    dist2 = pair2!!d0 - pair2!!d1
    pct = (value - pivot1) / dist1
