module BillModel(Shot, Ball, Cond(Cond), chain, tick, move, ballSize, width, height) where

import Determinant
import Reflecter

type Ball d = (d, d, Int)
type Motion d = (d, d)
type Shot d = (Ball d, Motion d)
data Cond d = Cond d [Shot d] deriving Show
data HitSnapShot d = HitSnapShot { ball1::Shot d, ball2::Shot d, dTime::d }
            | HitWithWall { reflectVertical::Bool, ballId::Int, dTime::d }
            | NotHit
            deriving Show

ballSize = 25
width = 800
height = 400

nTimes :: a -> (a -> a) -> Int -> a
nTimes x _ 0 = x
nTimes start f n = nTimes (f start) f (n-1)

chain :: (Ord d, Floating d) => Cond d -> Cond d
chain (Cond timestamp shots) = Cond timeNext (when shots)
    where
        timeNext = timeOf firstHit
        timeOf NotHit = timestamp -- TODO how can??
        timeOf h      = timestamp + dTime h
        firstHit = foldr first NotHit allHits
        first :: (Ord d, Floating d) => HitSnapShot d -> HitSnapShot d -> HitSnapShot d
        first a NotHit = a
        first NotHit a = a
        first a b
            | (dTime b) < (dTime a) = b
            | otherwise             = a
        allHits = (concat$map (\s1 -> map (\s2 -> makeHit s1 s2) shots) shots) ++ (map wallHits shots)
        when shots = hitAction$map (move (timeNext - timestamp)) shots -- TODO
        hitAction = map (reflectOrNot firstHit)

tick :: (Ord d, Floating d) => d -> Cond d -> Cond d
tick dt (Cond timestamp shots)
    | dt < timeNext = Cond (timestamp + dt) (map (move dt) shots)
    | otherwise     = tick (dt - timeNext) (Cond (timestamp + timeNext) (when shots))
    where
        timeNext = timeOf firstHit
--        timeOf NotHit = timestamp -- TODO how can??
        timeOf h      = dTime h
        firstHit = foldr first NotHit allHits
        first :: (Ord d, Floating d) => HitSnapShot d -> HitSnapShot d -> HitSnapShot d
        first a NotHit = a
        first NotHit a = a
        first a b
            | (dTime b) < (dTime a) = b
            | otherwise             = a
        allHits = (concat$map (\s1 -> map (\s2 -> makeHit s1 s2) shots) shots) ++ (map wallHits shots)
        when shots = hitAction$map (move timeNext) shots -- TODO
        hitAction = map (reflectOrNot firstHit)

reflectOrNot :: (Ord d, Num d, Fractional d) => HitSnapShot d -> (Shot d -> Shot d)
reflectOrNot NotHit = id
reflectOrNot (HitWithWall vertical id1 _) = (\s -> reflect s)
    where
        reflect s@(ball@(_,_,idx), (dx, dy))
            | idx == id1 = (ball, (newMotion))
            | otherwise  = s
            where
                newMotion
                    | vertical  = (-dx, dy)
                    | otherwise = (dx, -dy)

reflectOrNot (HitSnapShot shot1@((x1,y1,id1),(dx1,dy1)) shot2@((x2,y2,id2),(dx2,dy2)) _) = (\s -> reflect s)
    where
        reflect s@((x,y,idx),(dx,dy))
            | idx == id1 = reflectWith s shot2
            | idx == id2 = reflectWith s shot1
            | otherwise  = s

reflectWith :: (Num d, Fractional d) => Shot d -> Shot d -> Shot d
reflectWith ((x1,y1,id),(dx1,dy1)) ((x2,y2,_),(dx2, dy2)) = ((x1,y1,id),sumVector myVector otherVector)
    where
        myVector = my$divide (x1,y1) (x2,y2) (dx1,dy1)
        otherVector = others$divide (x1,y1) (x2,y2) (dx2,dy2)
        sumVector (a,b) (c,d) = (a+c,b+d)

move :: (Floating d) => d -> Shot d -> Shot d
move t = (\((x,y,id),(dx,dy)) -> ((x+dx*t,y+dy*t,id),(dx,dy)))

wallHits :: (Ord d, Floating d) => Shot d -> HitSnapShot d
wallHits s@((x, y, ballId), (dx, dy))
    | whenHitV == Nothing && whenHitH == Nothing = NotHit
    | otherwise                                  = earlier
    where
        whenHitV = when dx x width
        whenHitH = when dy y height
        earlier = chooseEarlier whenHitV whenHitH
        chooseEarlier Nothing Nothing      = NotHit
        chooseEarlier Nothing (Just timeH) = HitWithWall False ballId timeH
        chooseEarlier (Just timeV) Nothing = HitWithWall True ballId timeV
        chooseEarlier (Just timeV) (Just timeH)
            | timeH < timeV = HitWithWall False ballId timeH
            | otherwise     = HitWithWall True ballId timeV
        when d p size
            | d < 0 = Just((-(realToFrac (size - ballSize)) - p) / d)
            | d > 0 = Just(((realToFrac (size - ballSize)) - p) / d)
            | otherwise = Nothing

makeHit :: (Ord d, Floating d) => Shot d -> Shot d -> HitSnapShot d
makeHit s1@((_,_,id1),_) s2@((_,_,id2),_)
    | id1 == id2 = NotHit
    | otherwise   = hitOrNot$whenHit s1 s2
    where
        hitOrNot (Just time) = HitSnapShot (move time s1) (move time s2) time
        hitOrNot Nothing     = NotHit

whenHit :: (Ord d, Floating d) => Shot d -> Shot d -> Maybe d
whenHit ((x1, y1, _), (dx1, dy1)) ((x2, y2, _), (dx2, dy2))
    | hit       = Just touchTime
    | otherwise = Nothing
    where
        ax = x1 - x2
        adx = dx1 - dx2
        ay = y1 - y2
        ady = dy1 - dy2
        crossTime = -(ax*adx+ay*ady)/(adx^2+ady^2)
        hit = crossTime >= 0 && (d2 crossTime) < (fromIntegral ballSize * 2) ^ 2
        d2 t = (ax+adx*t)^2 + (ay+ady*t)^2
        touchTime
            | b < 0     = (-b - sqrt(b^2-4.0*a*c))/2.0/a
            | otherwise = (-b + sqrt(b^2-4.0*a*c))/2.0/a
            where
                a = (adx^2+ady^2)
                b = 2.0*(ax*adx+ay*ady)
                c = ax^2+ay^2 - (fromIntegral ballSize * 2)^2

-- test
test1 = whenHit ((0,0,1),(0,0)) ((10,0,2),(1,0))
test2 = whenHit ((0,0,1),(0,0)) ((10,0,2),(0,0))
test3 = whenHit ((0,0,1),(0,0)) ((10,0,2),(-1,0))
test4 = whenHit ((0,0,1),(0,0)) ((10,0,2),(-1,0.1))
test5 = whenHit ((0,0,1),(0,0)) ((5,0,2),(-0.8,0.5999))
start = Cond 0 [((50,100,0),(0,-1))
        , ((49,20,1),(0,0))
        , ((52,19,2),(0,0))
        , ((43,3,3),(0,0))
    ]
start2 = Cond 0 [((0,100,0),(0,-1))
        , ((0,50,1),(0,0))
        , ((0,20,2),(0,0))
        , ((0,0,3),(0,0))]
test shots = concat$map (\s1 -> map (\s2 -> makeHit s1 s2) shots) shots
hoge = test [((50,100,0),(0,-1))
        , ((49,20,1),(0,0))
        , ((52,19,2),(0,0))
    ]
