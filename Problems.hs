-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
module Problems
( myLast
, secondLast
, nTh
, len
, myRev
, pali
, NestedList 
, flatElem
, dedup 
, pack
, pack'
) where  

import Data.List
import Data.Tuple


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



