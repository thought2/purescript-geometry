module Geometry2d.Rect (Rect(..), RectData) where

import Prelude
import Data.Lens (Lens', lens, set, view)
import Data.Typelevel.Num.Reps (D2)
import Data.Vec (Vec)
import Default (class Default, def)
import Geometry2d.Class (class BoundingBox, _size, _center)

newtype Rect a = Rect (RectData a)

type RectData a =
  { center :: Vec D2 a
  , size :: Vec D2 a
  , rotation :: Number
  }

instance boundingBoxRect :: Field a => BoundingBox Rect a where
  _center = _Rect <<< (lens _.center $ _ { center = _ })
  _size = _Rect <<< (lens _.size $ _ { size = _ })

instance defaultRect :: Field a => Default (Rect a) where
  def = Rect $
    { center : zero
    , size : one
    , rotation : zero
    }


_Rect :: forall a. Lens' (Rect a) (RectData a)
_Rect = lens (\(Rect rect) -> rect) (\_ x -> Rect x)

fromBB :: forall f. BoundingBox f Number => f Number -> Rect Number
fromBB x =
  def
    # set _size (view _size x)
    # set _center (view _center x)
