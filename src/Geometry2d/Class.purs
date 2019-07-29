module Geometry2d.Class where

import Prelude

import Data.Lens (Lens', lens, set, (^.), Lens, over)
import Data.Typelevel.Num.Reps (D2, d0, d1)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (Vec, vec2, (!!))
import Data.Vec as Vec
import Default (class Default)
import Math (cos, sin)
import Numbers as Numbers

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
scaleAbout origin scale x = x
  # set _center (origin + ((x^._center - origin) * scale))
  # set _size (x^._size * scale)

remap :: forall a. Move a => Scale a => Vec D2 Vec2n -> Vec D2 Vec2n -> a -> a
remap field1 field2 x =
  x
    # over _center (remapVec field1 field2)
    # over _size (remapVec field1 field2)
  where
    remapVec :: Vec D2 Vec2n -> Vec D2 Vec2n -> Vec2n -> Vec2n
    remapVec field1 field2 v =
      pure Numbers.remap
        # Vec.zipWithE (#) field1
        # Vec.zipWithE (#) field2
        # Vec.zipWithE (#) v

ferrisWheel :: forall a. Move a => Vec2n -> Rad -> a -> a
ferrisWheel origin rad x = x
  # over _center (_ - origin)
  # over _center (rotateVec rad)
  # over _center (_ + origin)

rotateVec :: Rad -> Vec2n -> Vec2n
rotateVec rad p = vec2
  (x * cos rad - y * sin rad)
  (y * cos rad + x * sin rad)
  where x = p!!d0
        y = p!!d1

rotateAround :: forall a. Move a => Rotate a => Vec2n -> Rad -> a -> a
rotateAround origin rad x = x
    # over _center (_ - origin)
    # over _rotation (_ + rad)
    # over _center (rotateVec rad)
    # over _center (_ + origin)
