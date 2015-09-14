-- project Euler # 160 solution by GLSchultz
-- 2015-Sep-12

-- Prelude> :set +s
-- Prelude> :l do.hs
-- [1 of 1] Compiling Main             ( do.hs, interpreted )
-- Ok, modules loaded: Main.
-- (0.10 secs, 48623976 bytes)
-- *Main> factorial' 5 1000000000000
-- Ltd 5 9376
-- (1.83 secs, 552894216 bytes)


-- our normal factorial uses arbitrarily large integers
factorial :: Integer -> Integer
factorial n
    | n <= 1    = 1
    | otherwise = n * factorial (n-1)


-- utility to find the last k non-zero digits of an Integral type
lastk :: Integral a => a -> a -> a
lastk k n
    | r == 0    = lastk k m
    | otherwise = n `mod` (10^k)
    where
        (m, r) = n `divMod`  10


-- the Integral type we will use -- pick Int or Integer
type UseInt = Integer
-- Int works fine for small numbers of requested digits, but leads to
-- an overflow if 10^k overflows. Integer always seems to work, and isn't
-- really much slower (and uses less memory) for k == 5.


-- limited integer type contains the number of digits to keep (first)
-- and the number which is at most that many digits (second)
data LTD = Ltd UseInt UseInt deriving Show
--         Ltd ndigits number


-- construct a limited integer
ltd :: UseInt -> UseInt -> LTD
ltd k n
    | k < 1     = ltd 1 n
    | n < 1     = ltd k 1
    | otherwise = Ltd k (lastk k n)


-- multiply two limited integers to give another
infixl 7 .*.
(.*.) :: LTD -> LTD -> LTD
(Ltd k1 n1) .*. (Ltd k2 n2) = ltd (min k1 k2) (n1 * n2)


-- multiply a limited integer and a regular integer to give another limited
infixl 7 .*
(.*) :: LTD -> UseInt -> LTD
(Ltd k1 n1) .* n2 = ltd k1 (n1 * n2)


-- power uses log_2 n multiplications
infixr 8 .^
(.^) :: LTD -> UseInt -> LTD
lim@(Ltd k n) .^ x
    | x <= 0    = ltd k 1       -- lim.^0 == 1
    | x == 1    = lim           -- lim.^1 == lim
    | r == 0    = sq            -- lim.^2h == lim.^h .*. lim.^h
    | otherwise = sq .*. lim    -- lim.^(2h+1)
    where
        (h, r) = x `divMod` 2   -- half of exponent and remainder
        m = lim .^ h
        sq = m .*. m


-- factorial breaks into n = x + y * 10^k
-- so that n! = x! * ((10^k)!)^y
-- this means the largest "small" factorial you need is for 10^k
factorial' :: UseInt -> UseInt -> LTD
factorial' k n
    | y == 0    = xx
    | otherwise = xx .*. bb .^ y
    where
        b = 10^k
        (y, x) = n `divMod` b
        xx = smallfact k x
        bb = smallfact k (b-1)
        -- Could optimize this so it isn't factuple-ing over the range 1..x
        -- but that would make the code less readable and more error prone.


-- compute small factorials -- for n < 10^k
smallfact :: UseInt -> UseInt -> LTD
smallfact k n = balance .*. (ltd k 2) .^ n2m5s where
    (n2m5s, balance) = factuple n (0, (ltd k 1))
    -- can prove that n2m5s >= 0


-- n2m5s contains number of powers of two minus powers of five
--  thus powers of ten disappear naturally
-- balance contains the final k digits of n!/(2^α 5^β)
factuple :: UseInt -> (UseInt, LTD) -> (UseInt, LTD)
factuple n (n2m5s, balance)
    | n <= 1            = (n2m5s, balance)
    | otherwise         = factuple (n-1) (n2m5s + n2 - n5, balance .* bal)
    where
        (n2, n5, bal) = sepTwoFive n


-- separate into n = 2^α 5^β γ, where γ is not divisible by 2 or 5
sepTwoFive :: UseInt -> (UseInt, UseInt, UseInt)
sepTwoFive n = work n (0, 0, 1) where
    work n (α, β, γ)
        | n <= 1    = (α, β, γ)
        | a == 0    = work aa (α+1, β, γ)
        | b == 0    = work bb (α, β+1, γ)
        | otherwise = (α, β, γ * n)
        where
            (aa, a) = n `divMod` 2
            (bb, b) = n `divMod` 5


main = do
    print $ "some examples..."
    print $ factorial  20
    print $ lastk 5 $ factorial  20
    print $ factorial' 5 20
    print $ factuple 20 (0, (ltd 5 1))
    print $ "answer to the question is..."
    print $ factorial' 5 (10^12)
