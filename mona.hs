
data WithLog a = WithLog String a
	| Init deriving Show

instance Monad WithLog where
	return = WithLog "+"
	Init >>= f = Init
	WithLog log x >>= f = convert (f x)
		where
			convert (WithLog newLog y) = WithLog (log++newLog) y

countUp wl@(WithLog log n)
	| (mod n 3) == 0 = WithLog (log ++ (newLog n)) (n+1)
	| otherwise  = WithLog log (n+1)
	where
		newLog n = show n ++ " is appear! "

main = do
	putStrLn (show loopUp)
	where
		loopUp :: WithLog Int
		loopUp = do
			times 10 countUp
			return 1
			where
				times 0 _ = return 0
				times n f = f $ times (n-1) f