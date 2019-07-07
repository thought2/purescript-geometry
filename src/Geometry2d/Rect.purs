module Geometry2d.Rect where

import Prelude
import Data.Vec (Vec)
import Data.Typelevel.Num.Reps (D2)
import Default(class Default)

newtype Rect a = Rect
  { center :: Vec D2 a
  , size :: Vec D2 a
  , rotation :: Number
  }

instance defaultRect :: Semiring a => Default (Rect a) where
  def = Rect $
    { center : zero
    , size : one
    , rotation : zero
    }

-- TRANSFORM

move :: forall a. Semiring a => Vec D2 a -> Rect a -> Rect a
move v (Rect rect) = Rect (rect { center = rect.center + v })

rotate :: forall a. Semiring a => Number -> Rect a -> Rect a
rotate rad (Rect rect) = Rect (rect { rotation = rect.rotation + rad })

scale :: forall a. Semiring a => Vec D2 a -> Rect a -> Rect a
scale vec (Rect rect) = Rect (rect { size = rect.size * vec })

resize :: forall a. Semiring a => Vec D2 a -> Rect a -> Rect a
resize size (Rect rect) = Rect (rect { size = size })

-- QUERY

getUpperLeft :: Rect Number -> Vec D2 Number
getUpperLeft (Rect {center, size}) = center - (size * pure 0.5)

getCenter :: forall a. Semiring a => Rect a -> Vec D2 a
getCenter (Rect {center}) = center

getSize :: forall a. Semiring a => Rect a -> Vec D2 a
getSize (Rect {size}) = size
