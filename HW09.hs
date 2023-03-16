-- HW09 Haskell Function Syntax
--1.
--a

thirdA :: [a] -> a
thirdA xs = xs!!2
--b
thirdB :: [a] -> a
thirdB xs = head (drop 2 xs)
--c
thirdC :: [a] -> a
thirdC (_:_:z:xs) = z

--2.
data RPS = Rock | Paper | Scissors
beats :: RPS -> RPS -> Bool
beats Rock Paper = False
beats Rock Scissors = True
beats Scissors Rock = False
beats Scissors Paper = True
beats Paper Rock = True
beats Paper Scissors = False
beats _ _ = False


--3.
-- no case for y = 0, since Haskell will print infinity
quotient :: Fractional a => (a, a) -> a
quotient (x,y) = x/y

--4.
winner :: Ord a => a -> a -> String
winner x y
    | x > y = "Candidate 1"
    | x < y = "Candidate 2"
    | x == y = "Tie"

--5.
finalGrade :: Foldable t => t Int -> Char
finalGrade xs
    | grade >= 90 = 'A'
    | grade >= 80 = 'B'
    | grade >= 70 = 'C'
    | grade >= 60 = 'D'
    | grade < 60 = 'F'
    where grade = fromIntegral (sum xs) / fromIntegral (length xs)

--6.
and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False


or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' False False = False

if' ::  Bool -> Bool -> Bool
if' True False = False
if' _   _      = True

iff' ::  Bool -> Bool -> Bool
iff' True  True   = True
iff' False False = True
iff' _ _ = False


xor' ::  Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

