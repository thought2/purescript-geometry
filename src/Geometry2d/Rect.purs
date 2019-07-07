module Geometry2d.Rect (Rect, _center, _size, _rotation, _upperLeft) where

import Prelude

import Data.Lens (Lens', lens)
import Data.Typelevel.Num.Reps (D2)
import Data.Vec (Vec)
import Default (class Default)

newtype Rect a = Rect (RectData a)

type RectData a =
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

_Rect :: forall a. Lens' (Rect a) (RectData a)
_Rect = lens (\(Rect rect) -> rect) (\(Rect rect) x -> Rect x)

_center :: forall a. Lens' (Rect a) (Vec D2 a)
_center = _Rect <<< (lens _.center $ _ { center = _ })

_size :: forall a. Lens' (Rect a) (Vec D2 a)
_size = _Rect <<< (lens _.size $ _ { size = _ })

_rotation :: forall a. Lens' (Rect a) Number
_rotation = _Rect <<< (lens _.rotation $ _ { rotation = _ })

_upperLeft :: Lens' (Rect Number) (Vec D2 Number)
_upperLeft = _Rect <<< lens getter setter
  where
    getter {center, size} = center - (size * pure 0.5)
    setter all@{center, size} ul = all { center = ul + (size * pure 0.5) }
