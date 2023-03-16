--warmup
showBitString :: [Bool] -> String
showBitString [] = []
showBitString [x]
    | x = "1"
    | not x = "0"
showBitString (x:xs) = showBitString [x] ++ showBitString xs

readBitString :: String -> [Bool]
readBitString "" = []
readBitString x
    | x == "0" = [False]
    | x == "1" = [True]
readBitString (x:xs) = readBitString [x] ++ readBitString xs


-- Part 1
-- 1
hammingDistance :: [Bool] -> [Bool] -> Int
hammingDistance xs ys
    | length xs /= length ys    = error "Non-exhaustive patterns in function hammingDistance"
    | otherwise                = sum [1 | (x, y) <- zip xs ys, x /= y]

-- 2
minDistance :: [[Bool]] -> Int
minDistance lists = minimum [hammingDistance x y | x <- lists, y <- lists, x /= y]

-- 3
xor' :: Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

hammingEncode :: [Bool] -> [Bool]
hammingEncode xs
    | length xs == 4 = xs ++ [xor' (xor' (xs!!1) (xs!!2)) (last xs)]
                        ++ [xor' (xor' (head xs) (xs!!2)) (last xs)]
                        ++ [xor' (xor' (head xs) (xs!!1)) (last xs)]
    | otherwise = error "Non-exhaustive patterns in function hammingEncode"

-- 4
--little help from this https://www.uncarved.com/articles/gray-codes/

allMessages :: Int -> [[Bool]]
allMessages 0 = [[]]
allMessages x = [True:i | i <- allMessages(x-1)] ++ [False:i | i <- allMessages(x-1)]