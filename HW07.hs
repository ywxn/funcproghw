-- HW 07
--1.
powers :: (Num a, Integral b) => a -> b -> [a]
powers b n = [b^n | n <- [0..n]]
-- test with powers 2 10
--2.
sumOfSquares :: Num a => [a] -> a
sumOfSquares xs = sum [x^2 | x <- xs]
-- test with sumOfSquares [1,3,5]
--3.
acronym :: [String] -> String
acronym words = [head x | x <- words]
-- test with acronym ["Automatic", "Teller", "Machine"] 
--4.
keepOnly :: (Foldable t, Eq a) => t a -> [a] -> [a]
keepOnly ys xs = [x | x <- xs, x `elem` ys]
-- test with keepOnly "aeiou" "supercalifragilisticexpialidocious"
--5
-- subtraction set operation
removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll ys xs = [x | x <- xs, x `notElem` ys]
-- test with removeAll "aeiou" "supercalifragilisticexpialidocious"
--6
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs,  y <- ys]
-- cartesianProduct "abc" [1..5]
--7
partition :: Ord a => a -> [a] -> ([a], [a])
partition p xs = ([x | x <- xs, x < p], [y | y <- xs, y >= p])
-- test with partition 10 [3,17,42,8,12,1,0,10] 
    -- break (> p) (sort xs) -- needs :m Data.List for sort
