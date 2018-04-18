module ListUtils(matching) where

matching [] = []
matching (a:as) = (map ((,) a) as) ++ (matching as)
