module Reflecter(DividedMotion(DividedMotion, my, others), divide) where
import Determinant

type Motion d = (d, d)
type Position d = (d, d)
data DividedMotion d = DividedMotion { my :: Motion d, others :: Motion d } deriving Show

divide :: (Num d, Fractional d) => Position d -> Position d -> Motion d -> DividedMotion d
divide (x1,y1) (x2,y2) motion@(dx,dy) = DividedMotion mMy mOthers
    where
        vMy = (y2-y1,x1-x2)
        vOthers = (x2-x1,y2-y1)
        revSqr = rev (SqrDet (x2-x1,y2-y1) (y2-y1,x1-x2))
        (a,b) = cross revSqr motion
        mOthers = (a*(x2-x1),a*(y2-y1))
        mMy = (b*(y2-y1),b*(x1-x2))
