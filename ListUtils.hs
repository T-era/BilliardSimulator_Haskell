module ListUtils(matching) where

matching [] = []
matching [a] = []
matching (a:as) = (map _matching as) ++ (matching as)
  where
    _matching b = (a, b)
