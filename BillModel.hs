module BillModel(Shot, Ball(Ball), Condition(Cond), tick, move, ballSize, tableWidth, tableHeight) where

import Determinant
import Reflecter
import ListUtils

type Pos d = (d, d)
data Ball d = Ball { pos :: Pos d, num :: Int } deriving (Show)
type Motion d = (d, d)
type Shot d = (Ball d, Motion d)
data Condition d = Cond { timer :: d, shots :: [Shot d] } deriving Show
data HitSnapShot d = NotHit
    | HitBalls { shot1::Shot d, shot2::Shot d, dTime::d }
    | HitWithWall { reflectVertical::Bool, ballId::Int, dTime::d }
    deriving (Show, Eq)

instance Eq (Ball d) where
    (Ball _ id1) == (Ball _ id2) = id1 == id2

instance Positional Ball where
    toPos (Ball p num) = (p, \p2 -> Ball p2 num)

instance (Eq d, Ord d) => Ord (HitSnapShot d) where
    compare NotHit NotHit = EQ
    compare NotHit _ = GT
    compare _ NotHit = LT
    compare a b = compare (dTime a) (dTime b)

ballSize = 25
tableWidth = 800
tableHeight = 400

-- 指定時間分シミュレートを進めます
tick :: (Ord d, Floating d) => d -> Condition d -> Condition d
tick dt (Cond timestamp shots)
    | dt < timeNext = Cond (timestamp + dt) (map (move dt) shots)
    | otherwise     = tick (dt - timeNext) (Cond (timestamp + timeNext) (when shots))
    where
        timeNext = dTime firstHit
        firstHit = foldr min NotHit allHits
        allHits = ballsHit ++ wallHit
        ballsHit = map makeBallsHit (matching shots)
        wallHit = map wallHits shots
        when shots = hitAction$map (move timeNext) shots
        hitAction = map (reflectOrNot firstHit)

reflectOrNot :: (Ord d, Num d, Fractional d) => HitSnapShot d -> (Shot d -> Shot d)
reflectOrNot NotHit = id
reflectOrNot (HitWithWall vertical id1 _) = reflect
    where
        reflect s@(ball@(Ball _ idx), (dx, dy))
            | idx == id1 = (ball, (if vertical then (-dx, dy) else (dx, -dy)))
            | otherwise  = s

reflectOrNot (HitBalls shot1@(b1, _) shot2@(b2, _) _) = reflect
    where
        reflect s@(bs, _)
            | bs == b1 = reflectWith s shot2
            | bs == b2 = reflectWith s shot1
            | otherwise  = s

reflectWith :: (Num d, Fractional d) => Shot d -> Shot d -> Shot d
reflectWith (b1, v1) (b2, v2) = (b1, myVector +++ otherVector)
    where
        myVector = my$divide b1 b2 v1
        otherVector = others$divide b1 b2 v2

move :: (Floating d) => d -> Shot d -> Shot d
move t (Ball (x, y) id, (dx, dy)) = (Ball nextPos id, (dx, dy))
    where
      nextPos = (x + dx*t, y + dy*t)

wallHits :: (Ord d, Floating d) => Shot d -> HitSnapShot d
wallHits s@(Ball (x, y) ballId, (dx, dy)) = earlier
    where
        earlier = chooseEarlier whenHitV whenHitH
        whenHitV = when dx x tableWidth
        whenHitH = when dy y tableHeight
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

makeBallsHit :: (Ord d, Floating d) => (Shot d, Shot d) -> HitSnapShot d
makeBallsHit (s1@(Ball _ id1, _), s2@(Ball _ id2, _))
    | id1 == id2 = NotHit
    | otherwise   = hitOrNot$whenHit s1 s2
    where
        hitOrNot (Just time) = HitBalls (move time s1) (move time s2) time
        hitOrNot Nothing     = NotHit

whenHit :: (Ord d, Floating d) => Shot d -> Shot d -> Maybe d
whenHit (Ball p1 _, v1) (Ball p2 _, v2)
    | hit       = Just touchTime
    | otherwise = Nothing
    where
        (ax, ay) = p1 -+- p2
        (adx, ady) = v1 -+- v2
        a = (adx^2+ady^2)
        b = 2 * (ax*adx + ay*ady)
        c = ax^2+ay^2 - (fromIntegral ballSize * 2)^2
        crossTime = - b / 2 / a
        hit = crossTime >= 0 && (d2 crossTime) < (fromIntegral ballSize * 2) ^ 2
        d2 t = (ax+adx*t)^2 + (ay+ady*t)^2
        touchTime
            | b < 0     = (-b - sqrt(b^2-4*a*c))/2/a
            | otherwise = (-b + sqrt(b^2-4*a*c))/2/a

-- test
test1 = whenHit (Ball (0,0) 1,(0,0)) (Ball (10,0) 2,(1,0))
test2 = whenHit (Ball (0,0) 1,(0,0)) (Ball (10,0) 2,(0,0))
test3 = whenHit (Ball (0,0) 1,(0,0)) (Ball (10,0) 2,(-1,0))
test4 = whenHit (Ball (0,0) 1,(0,0)) (Ball (10,0) 2,(-1,0.1))
test5 = whenHit (Ball (0,0) 1,(0,0)) (Ball (5,0) 2,(-0.8,0.5999))
start = Cond 0 [(Ball (50,100) 0, (0,-1))
        , (Ball (49,20) 1,(0,0))
        , (Ball (52,19) 2,(0,0))
        , (Ball (43,3) 3,(0,0))
    ]
start2 = Cond 0 [(Ball (0,100) 0,(0,-1))
        , (Ball (0,50) 1,(0,0))
        , (Ball (0,20) 2,(0,0))
        , (Ball (0,0) 3,(0,0))]
test shots = concat$map (\s1 -> map (\s2 -> makeBallsHit (s1, s2)) shots) shots
hoge = test [(Ball (50,100) 0,(0,-1))
        , (Ball (49,20) 1,(0,0))
        , (Ball (52,19) 2,(0,0))
    ]
