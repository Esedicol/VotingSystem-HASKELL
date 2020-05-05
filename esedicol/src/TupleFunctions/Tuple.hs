-- Author: Emmanuel Sedicol
module TupleFunctions.Tuple where

import Data.List
import Data.List (sortBy)
import Data.Function (on)

addTups :: Num b => (a1, b) -> (a2, b) -> (a1, b)
addTups (a1,b1) (a2,b2) = (a1, (b1 + b2))

addListsOfTups :: Num b => [(a1, b)] -> [(a2, b)] -> [(a1, b)]
addListsOfTups [] [] = []
addListsOfTups [] _ = []
addListsOfTups _ [] = []
addListsOfTups (x:xs) (y:ys) = addTups x y : addListsOfTups xs ys

sortTuplesAscending :: Ord b => [(a, b)] -> [(a, b)]
sortTuplesAscending = sortBy (compare `on` snd)

sortTuplesDescending :: Ord b => [(a, b)] -> [(a, b)]
sortTuplesDescending = sortBy (flip compare `on` snd)

removeBlankVotes :: (Eq b, Num b) => [(a, b)] -> [(a, b)]
removeBlankVotes xs = [(fst x, snd x) | x <- xs, snd x /= 0]

changeToZero :: Num p1 => p2 -> p1
changeToZero x = 0 

changeSndToZero :: Num b => [(a, p2)] -> [(a, b)]
changeSndToZero [] = []
changeSndToZero ((a1, a2) : xs) = (a1, changeToZero a2) : changeSndToZero xs

-- [("D",1),("E",2),("C",3),("B",3),("A",4)] -> [("D",1),("E",2),("C",0),("B",0),("A",0)]
checkDuplicatesInTuple :: (Eq b, Num b) => [(a, b)] -> [(a, b)]  
checkDuplicatesInTuple ((a1, a2) : []) = [(a1, a2)]    
checkDuplicatesInTuple ((a1, a2) : xs) | a2 == snd (head xs) = (a1, changeToZero a2) : changeSndToZero xs
            | otherwise = ((a1, a2) : checkDuplicatesInTuple xs)

-- [("D",1),("E",2),("C",3),("B",5),("A",4)] -> [("D",1),("E",2),("C",3),("B",0),("A",0)]
checkPreferenceOrder :: (Eq b, Num b) => [(a, b)] -> [(a, b)]
checkPreferenceOrder ((a1, a2) : []) = [(a1, a2)]
checkPreferenceOrder ((a1, a2) : xs) | snd (head xs) /= (a2 + 1) = ((a1, a2) : changeSndToZero xs)
            | otherwise = ((a1, a2) : checkPreferenceOrder xs)