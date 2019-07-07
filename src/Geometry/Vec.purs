module Geometry.Vec where

import Prelude

import Data.CommutativeRing (class CommutativeRing)
import Data.Distributive (class Distributive)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over)
import Data.Semiring (class Semiring)
import Data.Show (class Show)
import Data.Traversable (class Traversable)
import Data.Typelevel.Num (class GtEq)
import Data.Typelevel.Num.Ops (class MaxP)
import Data.Typelevel.Num.Reps (D1, D2, D3, D4, d0, d1, d2, d3, d4)
import Data.Typelevel.Num.Sets (class Nat)
import Data.Typelevel.Undefined (undefined)
import Data.Vec as Vec

newtype Vec s a = Vec (Vec.Vec s a)

instance semiringVec :: (Semiring a, Nat s) => Semiring (Vec s a) where
  add (Vec v1) (Vec v2) = Vec $ Vec.zipWithE add v1 v2
  zero = Vec $ pure zero
  mul (Vec v1) (Vec v2) = Vec $ Vec.zipWithE mul v1 v2
  one = Vec $ pure one

instance ringVec :: (Ring a, Nat s) => Ring (Vec s a) where
  sub (Vec v1) (Vec v2) = Vec $ Vec.zipWithE sub v1 v2

instance commtativeRingVec :: (Ring a, Nat s) => CommutativeRing (Vec s a)

derive instance newtypeVec :: Newtype (Vec s a) _
derive instance genericVec :: Generic (Vec s a) _

derive newtype instance functorVec :: (Nat s) => Functor (Vec s)
derive newtype instance applyVec :: (Nat s) => Apply (Vec s)
derive newtype instance applicativeVec :: (Nat s) => Applicative (Vec s)
derive newtype instance foldableVec :: (Nat s) => Foldable (Vec s)
derive newtype instance traversableVec :: (Nat s) => Traversable (Vec s)
derive newtype instance distributiveVec :: (Nat s) => Distributive (Vec s)
derive newtype instance eqVec :: (Nat s, Eq a) => Eq (Vec s a)
derive newtype instance showVector :: (Nat s, Show a) => Show (Vec s a)

-- SHORTHANDS

type Vec1 a = Vec D1 a
type Vec2 a = Vec D2 a
type Vec3 a = Vec D3 a
type Vec4 a = Vec D4 a

type Vec1n = Vec1 Number
type Vec2n = Vec2 Number
type Vec3n = Vec3 Number
type Vec4n = Vec4 Number

type Vec1i = Vec1 Int
type Vec2i = Vec2 Int
type Vec3i = Vec3 Int
type Vec4i = Vec4 Int
