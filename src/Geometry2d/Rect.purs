module Geometry2d.Rect (Rect(..), RectData) where

import Prelude
import Data.Lens (Lens', lens, set, view, (^.))
import Data.Typelevel.Num.Reps (D2)
import Data.Vec (Vec, vec2)
import Default (class Default, def)
import Geometry2d.Class (class Move, class Scale, class UniformScale, _size, _center, Vec2n)

newtype Rect = Rect RectData

type RectData =
  { center :: Vec2n
  , size :: Vec2n
  , rotation :: Number
  }

instance moveRect :: Move Rect where
  _center = _Rect <<< (lens _.center $ _ { center = _ })

instance uniformScaleRect :: UniformScale Rect where
  _length = _Rect <<< (lens _.size $ \r x -> r { size = vec2 x x })

instance scaleRect :: Scale Rect where
  _size = _Rect <<< (lens _.size $ _ { size = _ })

instance defaultRect :: Default Rect where
  def = Rect $
    { center : zero
    , size : one
    , rotation : zero
    }

_Rect :: forall a. Lens' Rect RectData
_Rect = lens (\(Rect rect) -> rect) (\_ x -> Rect x)

fromBB :: forall a. Scale a => Move a => a -> Rect
fromBB x =
  Rect { size : vec2 0.0 0.0, center : vec2 0.0 0.0, rotation: 0.0}
    # set _size (x^._size)
    # set _center (x^._center)
