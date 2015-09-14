import Data.Set hiding (filter)

-- given an integer, tell whether it is a non-divisor of every member
-- of the tribonicci sequence
-- relies on the fact that we can do the sequence mod base, and that
-- the sequence therefore must end in a cycle (at most base^2 tuples)
isNonDiv :: Int -> Bool
isNonDiv a = compute a 1 1 1 empty where
    compute :: Int -> Int -> Int -> Int -> Set (Int,Int,Int) -> Bool
    compute base tlast tpen tprev tuples
        | tnext == 0        = False -- is a divisor
        | member key tuples = True  -- gone in a cycle without falsifying
        | otherwise         = compute base tnext tlast tpen (insert key tuples)
        where
            tnext = (tlast + tpen + tprev) `mod` base
            key = (tlast, tpen, tprev)

oddNonDivs :: [Int]
oddNonDivs = filter isNonDiv [1,3..] -- list of odd non-divisors

main = do
    print (oddNonDivs !! 123)
    print "cycle lengths"
    print $ take 124 (Prelude.map cycleLen [1,3..])

cycleLen :: Int -> (Int,Float)
cycleLen b = compute b 1 1 1 empty where
    compute :: Int -> Int -> Int -> Int -> Set (Int,Int,Int) -> (Int,Float)
    compute base tlast tpen tprev tuples
        | member key tuples = (clen, frac)
        | otherwise         = compute base tnext tlast tpen (insert key tuples)
        where
            tnext = (tlast + tpen + tprev) `mod` base
            key = (tlast, tpen, tprev)
            clen = size tuples
            frac = (fromIntegral clen) / (fromIntegral base^3)

