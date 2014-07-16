import Control.Monad
import System.Environment
import Data.Char
import Graphics.UI.GLUT

import BillGl

data Mode = Mode (IO()) [String] String

allMode = [modeFullScreen,
	modeConfigure,
	modeShowUsage,
	modeNormal];

modeFullScreen = Mode startFullScreen ["/s", "-s"] "Execute with full screen mode (for Windows screen saver)."
modeConfigure = Mode (return ()) ["/c", "-c"] "Configure application. (for Windows screen saver. But not supported, yet.)"
modeShowUsage = Mode showUsage ["/h", "/?", "-h", "--help"] "Show this usage."
modeNormal = Mode startInWindow [] "Execute with window mode (default)."

instance Show Mode where
	show (Mode _ []     desc) = "\t(no arguments)\n\t\t" ++ desc
	show (Mode _ (o:ls) desc) = "\t" ++ o ++ showOpts ls ++ desc
		where
			showOpts [] = "\n\t\t"
			showOpts (s:ls) = ", " ++ s ++ showOpts ls

main = do
	args <- getArgs
	let (Mode action _ _) = selectMode$map (map toLower) args
	action

showUsage = do
	(progname, _) <- getArgsAndInitialize
	putStrLn ("Usage: " ++ progname ++ " [Options]")
	putStrLn "Options:"
	mapM_ (\m -> putStrLn$show m) allMode

selectMode :: [String] -> Mode
selectMode [] = modeNormal
selectMode (s:ls) = selectModeFirst allMode
        where
                selectModeFirst [] = modeShowUsage
                selectModeFirst (m@(Mode _ options _):ms)
                        | any (s==) options = m
                        | otherwise         = selectModeFirst ms

