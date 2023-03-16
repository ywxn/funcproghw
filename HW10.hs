--1

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0        = []
    | otherwise     = (x `mod` 10) : toDigitsRev (x `div` 10)

--2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []          = []
doubleEveryOtherRev [x]         = [x]
doubleEveryOtherRev (x:y:xs)    = x:(y * 2):doubleEveryOtherRev xs

--3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x < 10        = x + sumDigits xs
    | otherwise     = sumDigits (toDigits x ++ xs)

--4
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0