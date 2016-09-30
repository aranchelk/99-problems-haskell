-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
module Problems where

import Data.List
import Data.Ord
import Data.Tuple
import Debug.Trace
import System.Random
import Control.Monad
import Data.Function


-- Problem 1
-- Find the last element of a list.
myLast = last

myLast' :: [a] -> a
myLast' [] = error "that didn't work"
myLast' [x] = x
myLast' (_:xs) = myLast' xs


-- Problem 2
-- Find the last but one element of a list.
secondLast :: [a] -> a
secondLast [] = error "no elements"
secondLast [x] = error "need two elements at least"
secondLast (x:y:[]) = x
secondLast (x:y:xs) = secondLast (y:xs)


-- Problem 3
-- Find the K'th element of a list.
nTh :: [a] -> Int -> a
nTh []  _ = error "no empty lists"
nTh (x:_) 0 = x 
nTh (x:xs) n = nTh xs (n - 1)


-- Problem 4
-- Find the number of elements of a list.
len :: [a] -> Int
len xs = len_acc xs 0
    where
        len_acc :: [a] -> Int -> Int
        len_acc [] n = n
        len_acc (x:xs) n = len_acc xs (n + 1)


-- Problem 5
-- Reverse a list
myRev :: [a] -> [a]
myRev [] = []
myRev (x:xs) = (myRev xs) ++ [x]  


-- Problem 6
-- Find out whether a list is a palindrome.
pali :: Eq a => [a] -> Bool
pali [] = True
pali xs = foldr zipEq True (zip xs $ reverse xs)
    where
        zipEq (x,y) acc = x == y && acc


-- Problem 7
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a] deriving Show

flatElem (Elem x) = [x] 
flatElem (List xs) = foldr (++) [] (map flatElem xs) 

-- Problem 8
-- Eliminate consecutive duplicates of list elements.

dedup :: Eq a => [a] -> [a]
dedup xs = removeDup xs []
    where
        removeDup [] ys = ys 
        removeDup (x:xs) [] = removeDup xs [x]
        removeDup (x:xs) ys =
            if x == (last ys) 
                then removeDup xs (ys)
                else removeDup xs (ys ++ [x])
        

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- import Data.List
pack x = group x

-- Without lib
pack' :: Eq a => [a] -> [[a]]
pack' xs = consPack nestedList
    where 
        nestedList = map (:[]) xs
        consPack :: Eq a => [[a]] -> [[a]]
        consPack [] = []
        consPack (x:[]) = [x]
        consPack (x:y:[]) = if (last x) == (head y)
            then [x++y]
            else [x,y]
        consPack (x:y:ys) = if (last x) == (head y)
            then consPack ((x++y):ys)
            else x : consPack (y:ys) 

-- Problem 10
-- Run-length encoding
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs =
    zip counts vals 
    where
        counts = map length $ group xs
        vals = nub xs


-- Problem 11
-- Run-length encoding, if no duplicates it is simply copied into the result list
data MRE a = Single a | Multiple Int a deriving Show

getMRE :: (Int, a) -> MRE a
getMRE (i, a) = if i == 1
                  then Single a
                  else Multiple i a

modifiedEncoding :: Eq a => [a] -> [MRE a]
modifiedEncoding xs = map getMRE $ encode xs


-- Problem 12
-- Decode modified run-length encoded data
expandMRE :: MRE a -> [a]
expandMRE (Single x) = [x]
expandMRE (Multiple i x) = replicate i x


decodeMREList :: [MRE a] -> [a]
decodeMREList = flatten . map expandMRE
    where flatten = foldr (++) [] 

incrementMRE :: MRE a -> MRE a
incrementMRE (Single x) = Multiple 2 x
incrementMRE (Multiple n x) = Multiple (n + 1) x

getMREVal :: MRE a -> a
getMREVal (Single x) = x
getMREVal (Multiple n x) = x


-- Problem 13
-- Run-length encoding data directly. Don't explicitly create the sublists containing the duplicates
-- Only count them. As in problem P11, simplify the result list by replacing the singleton lists.
modifiedEncoding' :: Eq a => [a] -> [MRE a] 
modifiedEncoding' xs = foldr packMRE [] xs
    where
        packMRE x [] = [Single x]
        packMRE x (y:ys) = if x == getMREVal y
                                  then incrementMRE y:ys
                                  else  Single x:y:ys


-- Problem 14
dupList :: [a] -> [a]
dupList xs = foldr (\x acc -> [x,x] ++ acc) [] xs


-- Problem 15
runN :: (a -> a) -> a -> Int -> a  
runN _ x 0 = x
runN fn x n = runN fn (fn x) (n - 1)

upleList :: [a] -> Int -> [a]
upleList [] _ = []
upleList (x:xs) n = runN ((:)x) (upleList xs n) n


-- Problem 16
-- Drop every N'th element from a list.
drop' :: Int -> [a] -> [a]
drop' n xs =
    map snd $ filter (not . isNth n . fst) $ zip [1..] xs
    where
        isNth x = (==0) . flip(mod) x 

 
-- Problem 17
-- SplitAt
splitAt' :: [a] -> Int -> [[a]]
splitAt' xs o = 
        tupToList $ foldr splitAtN ([],[]) (zip [1..] xs)
    where
        tupToList (a, b) = [a, b]
        splitAtN (n, x) (xs, ys)
            | n <= o = (x:xs, ys)
            | otherwise = (xs, x:ys)
    
    
-- Problem 18
-- Extract a slice from a list.
slice' :: [a] -> Int -> Int -> [a]
slice' xs s f = take (f - s + 1) $ drop (s - 1) xs


-- Problem 19
-- Rotate a list N places to the left.
rotate' :: [a] -> Int -> [a]
rotate' xs n = 
    toListR $ splitAt' xs n'  
    where
        toListR (x:y:[]) = y++x
        n' = n `mod` (length xs)


-- Problem 20
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), (take (n - 1) xs) ++ drop n xs)


-- Problem 21
-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
    | n < 1 = error "Index can't be less than 1"
    | n > length xs = error "Index too big"
    | otherwise = (fst listParts) ++ x : (snd listParts)
    where
        listParts = splitAt (n - 1) xs


-- Problem 22
-- Create a list containing all integers within a given range.
-- * (range 4 9)
-- (4 5 6 7 8 9)
range :: Int -> Int -> [Int]
range s f
    | s > f = [s,(s - 1)..f]
    | otherwise = [s..f]


-- Problem 23
-- * (rnd-select '(a b c d e f g h) 3)
-- (E D A) 
rndLessThan :: Int -> IO Int
rndLessThan n = do
    getStdRandom $ randomR(0, (n - 1))

shuttleElement :: ([a],[a]) -> Int -> ([a],[a])
shuttleElement (toXs, fromXs) n = 
    let (x, newFrom) = removeAt (n + 1) fromXs
    in (x:toXs, newFrom)

shuttleRandElement :: IO ([a],[a]) -> IO ([a],[a])
shuttleRandElement xs = do
    xs' <- xs
    r <- rndLessThan $ length $ snd xs'
    return (shuttleElement xs' r)

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    oldAndNew <- iterate shuttleRandElement (return ([], xs)) !! n
    return (fst oldAndNew)


-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
lottoN :: Int -> Int -> IO [Int]
lottoN n r 
    | n > r = error "You've got a pigeon-hole problem."
    | otherwise = do
        let xs = [1..r]
        rndSelect xs n

-- Problem 25
-- Generate a random permutation of the elements of a list.
rPermu :: [a] -> StdGen -> [a]
rPermu xs g = 
    let
        lnXs = length xs
        rIndTup = zip xs $ take lnXs $ nub $ randomRs (0, lnXs - 1) g 
    in
        map fst $ sortBy (compare `on` snd) rIndTup

-- Problem 26
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
combi :: Ord a => Eq a => Int -> [a] -> [[a]]
combi n xs = filter (\x -> length x == n) $ nub $ map sort $ map nub $ replicateM n xs
-- Pretty crappy, slow


-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
-- ... )
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).


getUniqueGroups :: Int -> ([a],[[a]],[a]) -> [([a],[[a]],[a])]
getUniqueGroups 0 (xs, ys, zs) = [(xs++zs, ys, [])]
getUniqueGroups _ ([], yss, zs) = []
getUniqueGroups n (x:xs, yss, zs) = getUniqueGroups (n-1) (xs, yss' ++ [ys++[x]], zs) ++ getUniqueGroups n (xs, yss, zs++[x])
    where
        ys = last yss
        yss' = init yss

toGroupsAddUG :: Int -> [([a],[[a]],[a])] -> [([a],[[a]],[a])]
toGroupsAddUG n xs = collapse $ map (getUniqueGroups n) (map addEmpty xs)
    where
        addEmpty (xs, yss, zs) = (xs, yss ++ [[]], zs)
        collapse xs = foldr (++) [] xs

getAllGroupings :: [Int] -> [a] -> [[[a]]]
getAllGroupings ns xs = map (\(x,y,z) -> y) $ foldl (flip toGroupsAddUG) [(xs, [],[])] ns

-- Problem 28a
-- Sorting a list of lists according to length of sublists
-- Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- Prelude>["o","de","de","mn","abc","fgh","ijkl"]

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)

-- Problem 28b
-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

--sortByRareLength :: Foldable t => [t a] -> [t a]
sortByRareLength xs = foldr (++) [] $ sortByLength $ groupBy ((==) `on` length) xs

-- Problem 31
-- (**) Determine whether a given integer number is prime.
primeTest :: Int -> Int -> Bool
primeTest n x 
    | x == n + 1 = True
    | otherwise = if (x `isDivisibleBy` n)
        then False
        else primeTest (n + 1) x
    where
        isDivisibleBy x n = 0 == x `mod` n

isPrime :: Int -> Bool
isPrime x
    | x == 1 = True
    | x == 2 = True
    | otherwise = primeTest 2 x


-- Problem 32
-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
gcd' :: Int -> Int -> Int
gcd' x y
    | small == 0 = big 
    | otherwise = gcd' small (big - small)
    where
        xy = if x > y
            then (y,x)
            else (x,y)
        small = fst xy
        big = snd xy


-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime x y = 1 == (gcd' x y)


-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
totient :: Int -> Int
totient n = length [x | x <- [1..n], (gcd x n) == 1]


-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

-- Doesn't factor, it lists unique prime factors, didn't read the question properly.
getPrimeFactors :: Int -> [Int]
getPrimeFactors n =
    [x | x <- [1..maxTest], (isPrime x) && ((n `mod` x) == 0) ]
    where maxTest = round $ sqrt $ fromIntegral n


-- Factor
-- Start counting up, if you find a value that mods even, keep dividing, if number isn't zero and mod isn't zero, count further.

primeFactors :: Int -> [Int]
primeFactors x = factorIt [] primes x
    where
        primes = 2: [x | x <- [3,5..], isPrime x]
        factorIt acc _ 1 = acc
        factorIt acc (p:ps) x = if (x `mod` p) == 0
            then factorIt (p:acc) (p:ps) (x `div` p)
            else factorIt acc ps x
        
        
-- Problem 36
-- (**) Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.

packTup x [] = [(x,1)]
packTup x ((valA, quantA):cc) = if x == valA
    then (valA, (quantA + 1)):cc
    else (x,1):(valA, quantA):cc

packTup' :: [Int] -> [(Int,Int)]
packTup' = foldr packTup []

mPrimeFactors :: Int -> [(Int, Int)]
mPrimeFactors = packTup' . primeFactors


-- Problem 37
-- Calculate Euler's totient function phi(m) (improved).
tot (p,m) = (p - 1) * p ^ (m - 1)

--totient' = foldr (*) 0 $ map tot mPrimeFactors
totient' = foldr (*) 1 . map tot . mPrimeFactors


-- Problem 38
-- (no solution required), totient' is an order of mag slower, did I screw that up?

-- Problem 39
-- (*) A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

primeR :: Int -> Int -> [Int]
primeR start finish =  [x | x <- [start..finish], isPrime x] 

-- Problem 40
-- (**) Goldbach's conjecture.
-- (goldbach 28) (5 23)

goldbach :: Int -> (Int, Int)
goldbach x
    | odd x = error "Only goldbach only works for evens!"
    | x == 2 = error "2 is too small"
    | otherwise = gb x primes
    where
        primes = 2:[x | x <- [3,5..], isPrime x]
        gb x (p:ps) = if isPrime $ x - p
            then (p, x - p)
            else gb x ps



-- Problem 41
-- (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

goldbachList :: Int -> Int -> [(Int, (Int,Int))]
goldbachList low high 
    | low == 2 || high == 2 = error "2 is too small"
    | otherwise = map (\x -> (x, goldbach x)) [low,low+2..high]

getBigGoldbachs :: Int -> Int -> Int -> [(Int, (Int, Int))]
getBigGoldbachs low high floor = filter (\ (_, (p,q)) -> (p > floor && q > floor)) $ goldbachList low high


-- Problem 46
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

and' = (&&)
or' = (||)
nand' = (not .) . and'
nor' = (not .) . or'
xor' = (==) . not
impl' x y = x
equ' x y = x == y

infixl 4 `or'`
infixl 6 `and'`

tableTest :: ( Bool -> Bool -> Bool) -> String
tableTest fx =
    let
        testSc = replicateM 2 [True, False]
        runTest (x:x':[]) = x : x' : [fx x x']
    in
        unlines $ map unwords $ map (map show) $ map runTest $ replicateM 2 [True, False]

table :: (Bool -> Bool -> Bool) -> IO ()
table fx = do putStr $ tableTest fx


-- Problem 47
-- answer from 46 produced the results listed without alteration, supposedly I had to change fixity

-- Problem 48
infixl 3 `equ'`
-- infixl 7 `equ'`

ttMap :: (Show a, Show r) => (a -> r) -> [a] -> [[Char]]
ttMap _ [] = []
ttMap f (x:xs) = ((show x) ++ " -> " ++ (show $ f x)) : ttMap f xs

tt' :: Int -> ([Bool] -> Bool) -> [String]
tt' n f =
    let
        testSc = replicateM n [True, False]
    in
        ttMap f testSc


tableN = do (putStr .) . (unlines .) . tt'


-- Problem 49
-- (**) Gray codes.
type Gray = [Bool] 

findGrayIncrIndex :: Int -> Int -> Int
findGrayIncrIndex pX x
    | odd x = 0
    | ((x + 2^(pX - 1)) `mod` (2^pX)) == 0 = pX - 1
    | otherwise = findGrayIncrIndex (pX - 1) x

applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f pos xs
    | pos > (length xs) - 1 = error "Position is out of bounds"
    | otherwise = front ++ [f elemX] ++ back
        where 
            sp = splitAt (pos + 1) xs
            front = init $ fst sp
            elemX = last $ fst sp
            back = snd sp
              
getGrays :: Int -> [(Int, Gray)]
getGrays n = take (2^n) $ iterate (nextGray n) seed
    where
        seed = (0, take n $ repeat False)
        nextGray n (x, pr) = (x + 1, applyAt not (findGrayIncrIndex n (x + 1)) pr)

grayToString :: Gray -> String
grayToString xs = map gchar xs
    where 
        gchar x
            | x == False = '0'
            | x == True = '1'

showGray :: (t, Gray) -> String
showGray (n, xs) = grayToString $ reverse xs

gray :: Int -> [String]
gray n = map showGray $ getGrays n


-- Problem 50
-- (***) Huffman codes.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 

makeLeaf (x, f) = Node ((Just x), f, Nothing) EmptyTree EmptyTree

getTrees xs = map makeLeaf xs

getFreq (Node (_, f, _) _ _) = f
compareFreq x y = compare (getFreq x) (getFreq y)

leastFrequent xs = sortBy compareFreq xs

mergeNode (x:x':xs) = (Node (Nothing, frqSum, Nothing) x x') : xs
   where frqSum = (getFreq x) + (getFreq x')

makeTree (t:[]) = [t]
makeTree ts = makeTree $ mergeNode $ leastFrequent ts

makeTree' = head . makeTree

addEncodings c (Node (val, score, _) EmptyTree EmptyTree) = Node (val, score, Just c) EmptyTree EmptyTree
addEncodings c (Node (val, score, _) l r) = Node (val, score, Just c) (addEncodings ('0':c) l) (addEncodings ('1':c) r)
    
flattenTree EmptyTree = []
flattenTree (Node (Nothing, _, _) l r) = (flattenTree l) ++ (flattenTree r)
flattenTree (Node ((Just val), _, (Just code)) l r) = [(val, (reverse code))] ++ (flattenTree l) ++ (flattenTree r) 


huff xs = sortBy (comparing fst) $ flattenTree $ addEncodings "" $ makeTree' $ getTrees xs


-- Problem 56
-- (**) Construct completely balanced binary trees
-- data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

baseTreeDepth n acc lim =
    let nextAcc = (acc + (2^n))
    in if nextAcc > lim
        then (n, lim - acc)
        else baseTreeDepth (n + 1) nextAcc lim

baseTreeDepth' x = baseTreeDepth 0 0 x

cbt 0 acc = acc
cbt x acc = cbt (x-1) (Node 'x' acc acc)

cbt' x = cbt x EmptyTree
 
addNode (p:[]) (Node a lTr rTr) 
    | p == 'l' = Node a newTree  rTr
    | otherwise = Node a lTr newTree
    where newTree = Node 'x' EmptyTree EmptyTree
addNode (p:ps) (Node a lTr rTr)
    | p == 'l' = Node a (addNode ps lTr)  rTr
    | otherwise = Node a lTr (addNode ps rTr)


data Btree a = Branch a (Btree a) (Btree a) | Empty deriving (Show, Eq)

mirror1 Empty = Empty
mirror1 (Branch x b1 b2) = Branch x b2 b1

mirrorTree Empty = Empty
mirrorTree (Branch x b1 b2) = Branch x (mirrorTree b2) (mirrorTree b1)

isMirror a b = a == b'
    where b' = mirrorTree b

symmetric Empty = True 
symmetric (Branch _ x y) = isMirror x y

-- Problem 57
-- ??

-- Problem 58
-- ??

-- Problem 59
-- (**) Construct height-balanced binary trees
-- 
-- In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
-- 
-- Construct a list of all height-balanced binary trees with the given element and the given maximum height.

hbalTree1 a = tr
    where
        e = Empty
        b = Branch a Empty Empty
        allCombos = replicateM 2 [e, b]
        tCons (x:y:[]) = Branch a x y 
        tr = map tCons allCombos

hbalTree 0 _ = [Empty]
hbalTree 1 a = [Empty, (Branch a Empty Empty)]
hbalTree n a = map tCons allCombos
    where
        allCombos = replicateM 2 $ hbalTree (n - 1) a
        tCons (x:y:[]) = Branch a x y 







