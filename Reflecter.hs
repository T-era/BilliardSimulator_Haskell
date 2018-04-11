module Reflecter(DividedMotion(DividedMotion, my, others), divide) where
import Determinant

type Motion d = (d, d)
type Position d = (d, d)
type Moving d = (Position d, Motion d)

data DividedMotion d = DividedMotion { my :: Motion d, others :: Motion d } deriving Show
data CO d = CO { a :: Moving d, b :: Moving d} deriving Show
divide :: (Num d, Fractional d) => Position d -> Position d -> Motion d -> DividedMotion d
divide (x1,y1) (x2,y2) motion@(dx,dy) = DividedMotion mMy mOthers
    where
        vMy = (y2-y1,x1-x2)
        vOthers = (x2-x1,y2-y1)
        revSqr = rev (SqrDet (x2-x1,y2-y1) (y2-y1,x1-x2))
        (a,b) = cross revSqr motion
        mOthers = (a*(x2-x1),a*(y2-y1))
        mMy = (b*(y2-y1),b*(x1-x2))

reflect :: (Num d, Fractional d) => CO d -> CO d
reflect (CO (p1, m1) (p2, m2)) = CO v1 v2
  where
    DividedMotion m11 m21 = divide p1 p2 m1
    DividedMotion m12 m22 = divide p1 p2 m2
    v1 = (p1, m11 +++ m22)
    v2 = (p2, m21 +++ m12)
    (x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)
