-- 1.
powers :: (Integral a, Num b) => b -> a -> [b]
powers b n = map (b^) [0..n]

-- 2.
sumOfSquares :: Num a => [a] -> a
sumOfSquares xs = sum(map (^2) xs)

--3.
acronym :: [[b]] -> [b]
acronym = map head

--4.
keepOnly :: (Foldable t, Eq a) => t a -> [a] -> [a]
keepOnly xs = filter (`elem` xs)

--5.
removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll xs = filter (`notElem` xs)

--6.
takePrefix :: (a -> Bool) -> [a] -> [a]
takePrefix f [] = []
takePrefix f (x:xs) = if f x then x : takePrefix f xs else []

--7.
factors :: Int -> [Int]
factors n =
    let helper 0 accum = accum
        helper k accum = if n `mod` k == 0 then helper (k-1) (k:accum) else helper (k-1) accum
    in helper n []

--8.
prime :: Int -> Bool
prime n = factors n == [1,n]

--a.
primesToHundred :: [Int]
primesToHundred = filter prime [1..100]

--b.
firstHundredPrimes :: [Int]
firstHundredPrimes = take 100 (filter prime [1..])

--9.
-- since the claim in excersize 4.83 states that
-- for all n, the claim is true we just need to 
-- find one value that disproves it for the claim
-- to be proven false
counter :: Int -> [Int]
counter x = take x $ filter isCounterExample [0..]
    where isCounterExample n = not (any (prime . (n +)) [0..9])

findCompositeRun :: Int -> [Int]
findCompositeRun n = head [takePrefix (not . prime) [k..] | k<-[1..], length (takePrefix (not . prime) [k..])==n]


-- thanks for your help!