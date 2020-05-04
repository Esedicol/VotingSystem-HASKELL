-- Author: Emmanuel Sedicol
import Debug.Trace
import Data.List
import Data.List (sortBy)
import Data.Function
import Data.Function (on)
import Data.List.Split (splitOn)
import System.Environment

import TupleFunc
import DATA

weight :: Int
weight = 1000

numberOfCandidates :: Int
numberOfCandidates = 4

quota :: Int
quota = ((length stdVotes) `div` (numberOfCandidates + 1)) + 1

-- change votes from String to Integer
voteConversion :: String -> Int  
voteConversion a | a == "1" = 1| a == "2" = 2 | a == "3" = 3
                  | a == "4" = 4 | a == "5" = 5| otherwise = 0

--------------------- DATA -------------------------
votes :: [[Int]]
votes = map (map voteConversion) (drop 1 (map (drop 2) dirtyVotes))

candidates :: [String]
-- candidates = [(x) | x <- drop 2 (head dirtyVotes)]
candidates = ["A", "B", "C", "D", "E"]

sortedVotes :: [[(String, Int)]]
sortedVotes = filter (/=[]) $ map sortTuplesAscending $ map removeBlankVotes $ map (zip candidates) (votes)

cleanVotes :: [[(String, Int)]]
cleanVotes = filter (/=[]) $ map removeBlankVotes $ map checkPreferenceOrder (map checkDuplicatesInTuple sortedVotes)

stdVotes :: [([String], Int)]
stdVotes = map (\xs -> ([fst x | x <- xs], weight)) cleanVotes

stdVotes2 :: [[String]]
stdVotes2 = map (\xs -> [fst x | x <- xs]) cleanVotes

-- ["D","E","D","D","E","E","E","B","D","A"] -> [("A",1),("B",1),("D",4),("E",4)]           
countVoteFrequency:: Ord a => [a] -> [(a, Int)]          
countVoteFrequency xs = countElements (unique xs) xs  
    where 
        unique [] = []
        unique (x:xs) = x : unique (filter (/= x) xs)  
        countElements label targetPreference =  
            sortTuplesDescending . zip label $ map length $ map ($ targetPreference) (zipWith ($) (repeat (filter . (==))) label) 
            
--------------------- VOTING FUNCTIONS -------------------------
check x = map (filter (== x))

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- rank :: Ord a => [[a]] -> [a]
-- rank = map snd . result . map head

-- winner' bs = case rank (rmempty bs) of
--                 [c]    -> c
--                 (c:cs) -> winner' (elim c bs)
          
firstVotes = countVoteFrequency (map (take 1) stdVotes2)
secondVotes = countVoteFrequency (map head $ map fst stdVotes)

getVotesByPreference :: Ord a => Int -> [[a]] -> [(a, Int)]
getVotesByPreference n xs = [(head $ fst x, snd x) | x <- freq n xs]
    where
        freq i xs = countVoteFrequency $ rmempty (map (drop (i - 1)) (map (take i) xs))    
        
a = [ 
    (["D. Milliband","E. Milliband","A. Burbhm","E. Balls","D. Abbott"],1000.0),
    (["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],1000.0),
    (["A. Burbhm","D. Milliband","E. Milliband"],1000.0),
    (["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],1000.0),
    (["D. Milliband","E. Milliband"],1000.0)]
x = [
    [("D",1),("E",2),("C",3),("B",3),("A",4)],
    [("D",1),("E",2),("C",2),("B",3),("A",4)],
    [("D",1),("E",2),("C",3),("B",4),("A",4)]]



xx = [
    ["A","B","C"],
    ["D","E","C"],
    ["D","E","B"],
    ["B","D","E"],
    ["A","E","B"],
    ["D","E","C"],
    ["A","E","B"],
    ["C","D","E"],
    ["A","E","B"]]


