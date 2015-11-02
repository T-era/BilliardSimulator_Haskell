module Determinant(SqrDet(SqrDet), LinDet, absSD, rev, cross) where

data SqrDet t = SqrDet (t, t) (t, t)
type LinDet t = (t,t)

instance (Num t) => Num (SqrDet t) where
    (+) (SqrDet (a,b) (c,d)) (SqrDet (e,f) (g,h)) = SqrDet (a+e,b+f) (c+g,d+h)
    (*) (SqrDet (a,b) (c,d)) (SqrDet (e,f) (g,h)) = SqrDet (a*e+b*g,a*f+b*h) (c*e+d*g,c*f+d*h)
    fromInteger n = SqrDet (fromInteger n,0) (0,fromInteger    n)
    abs s = SqrDet (absSD s,0) (0,absSD s)
    signum s = SqrDet (signum$absSD s,0) (0,signum$absSD s)

absSD :: (Num t) => SqrDet t -> t
absSD (SqrDet (a, b) (c, d)) = a * d - b * c

rev :: (Fractional t) => SqrDet t -> SqrDet t
rev x@(SqrDet (a,b) (c,d)) = SqrDet (d/absX, -b/absX) (-c/absX, a/absX)
    where
        absX = absSD x

cross :: (Fractional t) => SqrDet t -> LinDet t -> LinDet t
cross (SqrDet (a,b) (c,d)) (e,f) = (a*e+b*f,c*e+d*f)
