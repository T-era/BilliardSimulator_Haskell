module BillModel(Shot, Ball(Ball), tick, ballSize, tableWidth, tableHeight) where

import Determinant
import Reflecter
import ListUtils

data Ball d = Ball { pos :: Pos d, num :: Int } deriving (Show)
type Shot d = (Ball d, Motion d)
type Condition d = [Shot d]
data Hit d = Hit { dTime::d, apply::(Condition d -> Condition d)}
type HitOrNot d = Maybe (Hit d)

instance Eq (Ball d) where
    (Ball _ id1) == (Ball _ id2) = id1 == id2

instance Positional Ball where
    toPos (Ball p num) = (p, \p2 -> Ball p2 num)

instance (Eq d) => Eq (Hit d) where
    a == b = (dTime a) == (dTime b)
instance (Show d) => Show (Hit d) where
    show (Hit a _) = show a

minHit :: (Ord d) => HitOrNot d -> HitOrNot d -> HitOrNot d
minHit Nothing Nothing = Nothing
minHit a Nothing = a
minHit Nothing b = b
minHit (Just a) (Just b)
  | dTime a > dTime b = Just b
  | otherwise         = Just a

ballSize = 25
tableWidth = 800
tableHeight = 400

-- 指定時間分シミュレートを進めます
tick :: (Ord d, Floating d) => d -> Condition d -> Condition d
tick dt shots
    | dt < timeNext         = moveAll dt shots
    | firstOrNot == Nothing = moveAll dt shots
    | otherwise             = tick (dt - timeNext) (when shots)
    where
        timeNext = dTime firstHit
        Just firstHit = firstOrNot
        firstOrNot = foldr minHit Nothing allHits
        allHits = ballsHit ++ wallHit
        ballsHit = map makeBallsHit (matching shots)
        wallHit = map wallHits shots
        when shots = hitAction$moveAll timeNext shots
        hitAction = apply firstHit

-- (衝突がない時間帯のために)指定時間、直線運動を続けます
moveAll :: (Floating d) => d -> Condition d -> Condition d
moveAll time = map (move time)

move :: (Floating d) => d -> Shot d -> Shot d
move t (Ball (x, y) id, (dx, dy)) = (Ball nextPos id, (dx, dy))
    where
        nextPos = (x + dx*t, y + dy*t)

reflectWall :: (Num d) => Bool -> (Shot d -> Shot d)
reflectWall vertical (ball, (dx, dy))
    | vertical  = (ball, (-dx, dy))
    | otherwise = (ball, (dx, -dy))

indexFromNum :: Int -> Condition d -> Int
indexFromNum n l = _findNum l 0
    where
        _findNum ((Ball _ nn, _):ls) temp
            | nn == n   = temp
            | otherwise = _findNum ls (temp + 1)

applyAt num f l = prev ++ (f item:suff)
    where
        index = indexFromNum num l
        prev = take (index) l
        item = l !! index
        suff = drop (index + 1) l

wallHits :: (Ord d, Floating d) => Shot d -> HitOrNot d
wallHits s@(Ball (x, y) ballId, (dx, dy)) = earlier
    where
        earlier = chooseEarlier whenHitV whenHitH
        whenHitV = when dx x tableWidth
        whenHitH = when dy y tableHeight
        chooseEarlier Nothing Nothing      = Nothing
        chooseEarlier Nothing (Just timeH) = Just (Hit timeH (applyAt ballId (reflectWall False)))
        chooseEarlier (Just timeV) Nothing = Just (Hit timeV (applyAt ballId (reflectWall True)))
        chooseEarlier (Just timeV) (Just timeH)
            | timeH < timeV = Just (Hit timeH (applyAt ballId (reflectWall False)))
            | otherwise     = Just (Hit timeV (applyAt ballId (reflectWall True)))
        when d p size
            | d < 0 = Just((-(realToFrac (size - ballSize)) - p) / d)
            | d > 0 = Just(((realToFrac (size - ballSize)) - p) / d)
            | otherwise = Nothing

reflectBalls :: (Fractional d, Num d) => (Int, Int) -> (Condition d -> Condition d)
reflectBalls (n1, n2) cond
    | id1 < id2 = _reflectBalls id1 id2
    | otherwise = _reflectBalls id2 id1
    where
        id1 = indexFromNum n1 cond
        id2 = indexFromNum n2 cond
        _reflectBalls i1 i2 = prev ++ (post1:midd) ++ (post2:suff)
            where
                prev = take i1 cond
                suff = drop (i2 + 1) cond
                midd = take (i2 - i1 - 1) (drop (i1 + 1) cond)
                item1 = cond !! i1
                item2 = cond !! i2
                (post1, post2) = reflect (item1, item2)

makeBallsHit :: (Ord d, Floating d) => (Shot d, Shot d) -> HitOrNot d
makeBallsHit (s1@(Ball _ id1, _), s2@(Ball _ id2, _))
    | id1 == id2 = Nothing
    | otherwise  = hitOrNot$whenHit s1 s2
    where
        idTpl = (id1, id2)
        hitOrNot (Just time) = Just (Hit time (reflectBalls idTpl))
        hitOrNot Nothing     = Nothing

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
start = [(Ball (50,100) 0, (0,-1))
        , (Ball (49,20) 1,(0,0))
        , (Ball (52,19) 2,(0,0))
        , (Ball (43,3) 3,(0,0))
    ] :: [Shot Double]
start2 = [(Ball (0,100) 0,(0,-1))
        , (Ball (0,50) 1,(0,0))
        , (Ball (0,20) 2,(0,0))
        , (Ball (0,0) 3,(0,0))] :: [Shot Double]
start3 = [(Ball (0,5) 0,(1,0))
        , (Ball (60,0) 1,(-1,0))] :: [Shot Double]
test shots = concat$map (\s1 -> map (\s2 -> makeBallsHit (s1, s2)) shots) shots
hoge = test [(Ball (50,100) 0,(0,-1))
        , (Ball (49,20) 1,(0,0))
        , (Ball (52,19) 2,(0,0))
    ]

tickTest n = tick n start3
