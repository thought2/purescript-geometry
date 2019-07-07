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

move :: forall a. Semiring a => Vec D2 a -> Rect a -> Rect a
move v (Rect rect) = Rect (rect { center = rect.center + v })

rotate :: forall a. Semiring a => Number -> Rect a -> Rect a
rotate rad (Rect rect) = Rect (rect { rotation = rect.rotation + rad })

scale :: forall a. Semiring a => a -> Rect a -> Rect a
scale fac (Rect rect) = Rect (rect { size = rect.size * (pure fac) })

resize :: forall a. Semiring a => Vec D2 a -> Rect a -> Rect a
resize size (Rect rect) = Rect (rect { size = size })
