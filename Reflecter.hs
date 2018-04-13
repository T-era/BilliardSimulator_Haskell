module Reflecter(
    DividedMotion(DividedMotion, my, others),
    Positional(toPos, divide, reflect),
    (+++), (-+-)) where

import Determinant

type Motion d = (d, d)
type Position d = (d, d)

data DividedMotion d = DividedMotion { my :: Motion d, others :: Motion d } deriving Show

class Positional p where
  toPos :: p d -> (Position d, Position d -> p d)
  divide :: (Num d, Fractional d) => p d -> p d -> Motion d -> DividedMotion d
  divide p1 p2 motion@(dx,dy) = DividedMotion mMy mOthers
      where
          ((x1,y1), _) = toPos p1
          ((x2,y2), _) = toPos p2
          vMy = (y2-y1,x1-x2)
          vOthers = (x2-x1,y2-y1)
          revSqr = rev (SqrDet (x2-x1,y2-y1) (y2-y1,x1-x2))
          (a,b) = cross revSqr motion
          mOthers = (a*(x2-x1),a*(y2-y1))
          mMy = (b*(y2-y1),b*(x1-x2))
  reflect :: (Num d, Fractional d) => ((p d, Motion d), (p d, Motion d)) -> ((p d, Motion d), (p d, Motion d))
  reflect ((pn1, m1), (pn2, m2)) = (v1, v2)
      where
        (p1, f1) = toPos pn1
        (p2, f2) = toPos pn2
        DividedMotion m11 m21 = divide pn1 pn2 m1
        DividedMotion m12 m22 = divide pn1 pn2 m2
        v1 = (f1 p1, m11 +++ m22)
        v2 = (f2 p2, m21 +++ m12)

(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

(x1, y1) -+- (x2, y2) = (x1 - x2, y1 - y2)
