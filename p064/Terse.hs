contFracSqrt s
    | s >= 0 = cfg s a0 0 1 [a0]
    | otherwise = [-1] -- error indication
    where a0 = (floor . sqrt $ fromIntegral s)
cfg s a0 mn dn as
    | dn == 0           = [a0]          -- perfect square has d1 == 0
    | 2 * a0 == head as = reverse as    -- periodic
    | otherwise         = cfg s a0 mp dp (ap:as)
        where
            mp = dn * (head as) - mn
            dp = ( s - mp * mp ) `div` dn
            ap = floor ( (fromIntegral ( a0 + mp )) / (fromIntegral dp) )
oddPd s = 0 == (toInteger $ length $ s) `mod` 2
main = print $ toInteger $ length $ filter oddPd (map contFracSqrt [1..10000])
