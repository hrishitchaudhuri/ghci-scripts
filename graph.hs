edge :: Char -> Char -> Bool
edge 'A' 'B' = True
edge 'B' 'C' = True
edge 'C' 'A' = True
edge 'A' 'D' = True
edge 'D' 'E' = True
edge 'C' 'E' = True
edge 'F' 'D' = True
edge 'F' 'E' = True
edge _ _ = False

type Path = [Char]
extendpath :: Path -> [Path]
extendpath p = [p ++ [c] | c <- ['A' .. 'F'], edge (last p) c] 

extendall :: [Path] -> [Path]
extendall [[]] = [[c] | c <- ['A' .. 'F']]
extendall l = concat [extendpath p | p <- l]

connectedpairs = [(head p, last p) | l <- firstn, p <- l]
    where (f:firstn) = take 6 (iterate extendall [[]])

connected x y = (elem (x, y) connectedpairs)
