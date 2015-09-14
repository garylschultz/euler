-- Project Euler problem # 64

-- I use the algorithm from:
--  http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
-- ... along with the properties explained in the article:
--  http://mathworld.wolfram.com/PeriodicContinuedFraction.html
-- In particular, given the non-square integer s, form a sequence of
--  m_n, d_n and a_n, such that
--  m_0 = 0
--  d_0 = 1
--  a_0 = floor( sqrt(s) )
--  m_{n+1} = d_n a_n - m_n
--  d_{n+1} = (s - m_{n+1}^2) / d_n
--  a_{n+1} = floor( (a_0 + m_{n+1}) / d_{n+1} )
-- Then the continued fraction expansion for sqrt(s) is given by
--  [a_0;a_1,a_2,...,a_2,a_1,2*a_0],
-- where the portion after the ; repeats infinitely.
-- Note the special structure of the repeating part.
-- If s is a square, then d_1 == 0, and we use that to return a list with
-- a single element [a_0].

-- Compute continued fraction expansion for sqrt Integer
contFracSqrt :: Integer -> [Integer]
contFracSqrt s
    | s >= 0 = cfg s a0 m0 d0 as
    | otherwise = [-1] -- error indication
    where
        a0 = floor . sqrt $ fromIntegral s
        m0 = 0
        d0 = 1
        as = [a0]

-- Worker for continued fraction sqrt
cfg :: Integer -> Integer -> Integer -> Integer -> [Integer] -> [Integer]
cfg s a0 mn dn as
    | dn == 0           = [a0]          -- perfect square has d1 == 0
    | 2 * a0 == head as = reverse as    -- periodic
    | otherwise         = cfg s a0 mp dp (ap:as)
        where
            mp = dn * (head as) - mn
            dp = ( s - mp * mp ) `div` dn
            ap = floor ( (fromIntegral ( a0 + mp )) / (fromIntegral dp) )

-- The length of the period is all but the first element for square roots.
periodLen :: Integer -> Integer
periodLen s = (toInteger $ length $ contFracSqrt s) - 1

-- Determine whether it has odd period.
oddPeriod :: Integer -> Bool
oddPeriod s = 1 == (periodLen s) `mod` 2

-- Count the number of Integers in the list 1..n that have odd periods.
countOddPeriod :: Integer -> Integer
countOddPeriod n = toInteger $ length $ filter id (map oddPeriod [1..n])

-- Answer the question.
main =
    print $ countOddPeriod 10000

