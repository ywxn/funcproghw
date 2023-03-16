-- Examlet 2 Redo

-- 1
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n xs@(_:xs')
   | n > 0     = drop' (n-1) xs'
   | otherwise = xs


count :: Eq a => a -> [a] -> Int
count e [] = 0
count e (x:xs)
    | e == x = 1 + (count e xs)
    | otherwise = count e xs

unique :: Eq a => [a] -> [a]
unique xs = [x | x <- xs, count x xs == 1]

replaceDuplicates :: Eq a => a -> [a] -> [a]
replaceDuplicates e xs = [if count x xs == 1 then x else e | x <- xs]

countListComp :: Eq a => a -> [a] -> Int
countListComp e [] = 0
countListComp e xs = length ys
   where ys = [x | x <- xs, x == e]

split :: [ a ] -> ([ a ] ,[ a ])
split xs = ( take half xs , drop half xs )
   where half = ( length xs ) `div` 2
merge :: ( Ord a ) => [ a ] -> [ a ] -> [ a ]
merge xs [] = xs
merge [] ys = ys
merge ( x : xs ) ( y : ys )
   | x < y = x :( merge xs ( y : ys ))
   | otherwise = y :( merge ( x : xs ) ys )
mergesort :: ( Ord a ) => [ a ] -> [ a ]
mergesort [] = []
mergesort [ x ] = [ x ]
mergesort xs = merge ( mergesort left ) ( mergesort right )
   where ( left , right ) = split xs
