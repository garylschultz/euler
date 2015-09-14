main = interact respond
respond = unlines . (map process) . lines
process = int2rom . listint2int . rom2listint

-- given a string Roman numeral, turn it into a reversed list of integers
-- reverses the order of the list for efficiency
rom2listint :: String -> [Integer]
rom2listint cs = parse [] cs where
    parse :: [Integer] -> String -> [Integer]
    parse xs [] = xs    -- done
    -- legal subtractive pairs if they are there
    parse xs ('I':'V':cs) = parse (4:xs) cs
    parse xs ('I':'X':cs) = parse (9:xs) cs
    parse xs ('X':'L':cs) = parse (40:xs) cs
    parse xs ('X':'C':cs) = parse (90:xs) cs
    parse xs ('C':'D':cs) = parse (400:xs) cs
    parse xs ('C':'M':cs) = parse (900:xs) cs
    -- then single characters
    parse xs ('I':cs) = parse (1:xs) cs
    parse xs ('V':cs) = parse (5:xs) cs
    parse xs ('X':cs) = parse (10:xs) cs
    parse xs ('L':cs) = parse (50:xs) cs
    parse xs ('C':cs) = parse (100:xs) cs
    parse xs ('D':cs) = parse (500:xs) cs
    parse xs ('M':cs) = parse (1000:xs) cs
    -- anything else doesn't follow the rules
    parse _ cs  = error $ "cannot parse " ++ cs

-- given a list of Integers, make sure it is ascending order and accumlate
-- int list is reversed relative to text
listint2int :: [Integer] -> Integer
listint2int xs = compute 0 xs where
    compute n [] = n
    compute n [x] = n + x
    compute n (x:y:xs) = if x <= y then compute (n+x) (y:xs)
        else error $ "violation of descending price rule"

-- represent an Integer as a Roman
int2rom :: Integer -> String
int2rom n = build n "" where
    build :: Integer -> String -> String
    build 0 cs = cs
    build n cs
        | n >= 1000 = build (n-1000) (cs ++ "M")
        | n >=  900 = build (n- 900) (cs ++ "CM")
        | n >=  500 = build (n- 500) (cs ++ "D")
        | n >=  400 = build (n- 400) (cs ++ "CD")
        | n >=  100 = build (n- 100) (cs ++ "C")
        | n >=   90 = build (n-  90) (cs ++ "XC")
        | n >=   50 = build (n-  50) (cs ++ "L")
        | n >=   40 = build (n-  40) (cs ++ "XL")
        | n >=   10 = build (n-  10) (cs ++ "X")
        | n >=    9 = build (n-   9) (cs ++ "IX")
        | n >=    5 = build (n-   5) (cs ++ "V")
        | n >=    4 = build (n-   4) (cs ++ "IV")
        | n >=    1 = build (n-   1) (cs ++ "I")

