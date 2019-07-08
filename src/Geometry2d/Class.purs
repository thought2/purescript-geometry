module Geometry2d.Class where

import Prelude

import Data.Lens (Lens', lens, set, view, (^.))
import Data.Typelevel.Num.Reps (D2)
import Data.Vec (Vec)
import Default (class Default)


left :: forall a. Ring a => a
left = -one

right :: forall a. Semiring a => a
right = one

up :: forall a. Ring a => a
up = -one

down :: forall a. Semiring a => a
down = one

type Direction = forall a. Ring a => Vec D2 a

class Ring a <= BoundingBox f a where
  _center :: Lens' (f a) (Vec D2 a)
  _size :: Lens' (f a) (Vec D2 a)

class CanRotateAround f a where
  rotateAround :: Vec D2 a -> Number -> f a -> f a


class ( BoundingBox f a
      , CanRotateAround f a
      , Default (f a)
      )
      <= Geometry  f a

_corner :: forall f. BoundingBox f Number => Direction -> Lens' (f Number) (Vec D2 Number)
_corner dir = lens getter setter
  where
    getter x =
      center x + (dir * size x * pure 0.5)
    setter x vec =
      set _center (vec + (-dir * size x * pure 0.5)) x
    center x = view _center x
    size x = view _size x


scaleAbout :: forall f a. BoundingBox f a => Vec D2 a -> Vec D2 a -> f a -> f a
scaleAbout origin scale x =
  x
    # set _center (origin + ((x^._center - origin) * scale))
    # set _size (x^._size * scale)
