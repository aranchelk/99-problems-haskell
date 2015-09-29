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

