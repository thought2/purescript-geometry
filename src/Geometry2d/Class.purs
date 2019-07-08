module Geometry2d.Class where

import Prelude
import Data.Lens (Lens', lens, set, (^.), Lens)
import Data.Typelevel.Num.Reps (D2)
import Data.Vec (Vec)
import Default (class Default)

type Vec2n = Vec D2 Number
type Rad = Number


class Move a where
  _center :: Lens' a Vec2n

class UniformScale a where
  _length :: Lens a a Vec2n Number

class Scale a where
  _size :: Lens' a Vec2n

class Rotate a where
  _rotation :: Lens' a Rad


_corner :: forall a. Move a => Scale a => Vec2n -> Lens' a Vec2n
_corner dir = lens getter setter
  where
    getter x =
      x^._center + (dir * x^._size / pure 2.0)
    setter x vec =
      set _center (vec + (-dir * x^._size / pure 2.0)) x


scaleAbout :: forall a. Move a => Scale a => Vec2n -> Vec2n -> a -> a
scaleAbout origin scale x =
  x
    # set _center (origin + ((x^._center - origin) * scale))
    # set _size (x^._size * scale)
