module BillGl(startFullScreen, startInWindow) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import Data.IORef
import System.Exit

import BillModel

startFullScreen = showWindow fullScreen'
fullScreen' = do
    fullScreen
    scale (1/width'::GLfloat) (1/height') 1
    keyboardMouseCallback $= Just myKeyMouseEvent
    reshapeCallback $= Just fullscreenResizeEvent
startInWindow = showWindow inWindow
inWindow = do
    scale (1/width'::GLfloat) (1/height') 1

interval = 20
ballSize' = realToFrac ballSize
ballS3 = sqrt 3 * ballSize'
width' = realToFrac width
height' = realToFrac height

startPos :: [Shot GLfloat]
startPos = [((-width'/2, - height' * 0.8, 0),(width' / 100, height' * 0.8 / 100))
        , stopped (width'/2,000,1)
        , stopped (width'/2+ballS3,-ballSize',5)
        , stopped (width'/2+ballS3, ballSize',6)
        , stopped (width'/2+ballS3*2,-ballSize'*2,3)
        , stopped (width'/2+ballS3*2,           0,9)
        , stopped (width'/2+ballS3*2, ballSize'*2,4)
        , stopped (width'/2+ballS3*3,-ballSize',7)
        , stopped (width'/2+ballS3*3, ballSize',8)
        , stopped (width'/2+ballS3*4,0,2)]

stopped :: Ball GLfloat -> Shot GLfloat
stopped (x,y,id) = ((realToFrac x, realToFrac y, id), (0,0))

first = Cond 0 startPos
showWindow :: IO() -> IO()
showWindow addition = do
    cond <- newIORef first

    --colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
    initialWindowSize $= Size (fromIntegral width) (fromIntegral height)
    createWindow "Billiard"
    addition

    clearColor $= Color4 0 0 0 1
    displayCallback $= step cond
    addTimerCallback interval $ timer (step cond)
    mainLoop

timer f = do
    f
    addTimerCallback interval $ timer f
step cond = do
    now <- readIORef cond
    modifyIORef cond (tick 1)
    display now
    flush
    swapBuffers

display (Cond _ balls) = do
    clear [ColorBuffer]
    clearTable
    mapM_ drawBall (map (\(pos, _) -> pos) balls)

myKeyMouseEvent _ _ _ _ = exitWith ExitSuccess
myMouseMotionEvent _    = exitWith ExitSuccess
fullscreenResizeEvent (Size w h) = do
    viewport $= (pos, size)
        where
            rate = min w (h * 2)
            size@(Size w' h') = Size rate (div rate 2)
            pos = Position 1 (div (h' - fromIntegral height) 2)

drawBalls :: [Ball GLfloat] -> IO()
drawBalls = mapM_ drawBall
drawBall :: Ball GLfloat -> IO()
drawBall size@(x, y, i) = do
    fillCircle (x, y) ballSize' 64 (colorBack i)
    when (i > 8) (fillStripe (x, y) ballSize' 64 (colorStrp i))
    when (i > 0) (drawLabel (Vector3 x y 0) (show i))

drawLabel :: Vector3 GLfloat -> String -> IO()
drawLabel pos@(Vector3 x y _) i = do
    lineWidth $= 1.0
    fillCircle (x+(ballSize' / 4), y+(ballSize' / 4)) (ballSize' / 2) 32 white
    preservingMatrix $ do
        translate pos
        w <- stringWidth Roman i
        currentColor $= black
        scale (0.15::GLfloat) 0.15 0.15
        renderString Roman i

white = Color4 1 1 1 1
black = Color4 0 0 0 1
colorBack 0 = white
colorBack 1 = Color4 1 1 0 1
colorBack 2 = Color4 0 0 1 1
colorBack 3 = Color4 1 0 0 1
colorBack 4 = Color4 0.6 0 0.8 1
colorBack 5 = Color4 1 0.5 0 1
colorBack 6 = Color4 0 0.8 0 1
colorBack 7 = Color4 0.5 0 0 1
colorBack 8 = Color4 0 0 0 1
colorBack _ = colorBack 0
colorStrp n = colorBack (n-8)

fillCircle center r det color = fillArc (toArc center r det) color [1..det]
fillStripe center r det color = fillArc (toArc center r det) color ([0..det/4] ++ [det/2..det*3/4])

fillArc genArc color points = do
    currentColor $= color
    renderPrimitive Polygon $ mapM_ vertex (map genArc points)

clearTable = do
    currentColor $= Color4 0.0 0.4 0.1 1
    renderPrimitive Polygon $ do
        vertex$Vertex2 (-width') (-height')
        vertex$Vertex2 (width') (-height')
        vertex$Vertex2 (width') (height')
        vertex$Vertex2 (-width') (height')

toArc (cx,cy) r det i = (Vertex2 x1 y1)
    where
        x1 = x$t i
        y1 = y$t i
        x th = cx + r * (cos th) :: GLfloat
        y th = cy + r * sin th :: GLfloat
        t n = 2 * pi * n / det
drawLine :: (Vertex x) => x -> x -> Color4 GLfloat -> IO()
drawLine p1 p2 color =
    renderPrimitive Lines $ do
        currentColor $= color
        vertex p1
        vertex p2
