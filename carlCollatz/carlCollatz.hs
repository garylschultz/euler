import qualified Data.Map.Strict as Map

collatzTerm :: Integer -> Integer
collatzTerm n
    | n < 1         = error "Cannot have negative number"
    | mod n 2 == 0  = div n 2
    | otherwise     =  3 * n + 1

collatzList :: Integer -> [Integer]
collatzList n
    | n == 1        = [1]
    | otherwise     = n:collatzList (collatzTerm n)

collatzLen :: Integer -> Int
collatzLen = length . collatzList

collatzMap :: Integer -> Map.Map Int [Integer]
collatzMap n
    | n == 0        = Map.empty
    | otherwise     = Map.alter f len (collatzMap (n-1))
    where
        len = collatzLen n
        f Nothing = Just [n]
        f (Just ms) = Just (n:ms)

consecMap :: Integer -> Map.Map Int [[Integer]]
consecMap n = Map.mapMaybe f (collatzMap n) where
    f x = if null gx then Nothing else Just gx where gx = g [] [] x
    -- g builds work [[Integer]] by keeping partial consecutive sequences
    -- and the rest of the unprocessed list.
    g :: [[Integer]] -> [Integer] -> [Integer] -> [[Integer]]
    g work    []        []    =             work
    g work    [_]       []    =             work
    g work    part      []    =       (part:work)
    g work    []     (j:rest) = g           work  [j]        rest
    g work (k:part)  (j:rest)
        | k == j + 1          = g           work  (j:k:part) rest
        | null part           = g           work  [j]        rest
        | otherwise           = g ((k:part):work) [j]        rest
