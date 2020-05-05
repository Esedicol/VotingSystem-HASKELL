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

quota :: Int
quota = ((length stdVotes) `div` (numberOfCandidates + 1)) + 1

-- ["D","E","D","D","E","E","E","B","D","A"] -> [("A",1),("B",1),("D",4),("E",4)]           
countVoteFrequency:: Ord a => [a] -> [(a, Int)]          
countVoteFrequency xs = countElements (unique xs) xs  
    where 
        unique [] = []
        unique (x:xs) = x : unique (filter (/= x) xs)  
        countElements label targetPreference =  
            sortTuplesDescending . zip label $ map length $ map ($ targetPreference) (zipWith ($) (repeat (filter . (==))) label) 
            
--------------------- VOTING FUNCTIONS -------------------------
check :: Eq a => a -> [[a]] -> [[a]]
check x = map (filter (== x))

elim :: Eq a => a -> [a] -> [a]
elim x = (filter (/= x))

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elected :: [String]
elected = []
 
getVotesByFirstElement :: Eq a => a -> [[a]] -> [[a]]
getVotesByFirstElement element xs = [ x | x <- xs, head x == element]

getVotesByPreference :: Ord a => Int -> [[a]] -> [(a, Int)]
getVotesByPreference n xs = [(head $ fst x, snd x) | x <- freq n xs]
    where
        freq i xs = countVoteFrequency $ rmempty (map (drop (i - 1)) (map (take i) xs))   

-- get a list of votes on selcted preference that goes over the quota
votesOverQuota :: Ord a => [[a]] -> Int -> [(a, Int)]
votesOverQuota votes n = getVotesOverQuotaValue (getVotesByPreference n votes)
    where
        getVotesOverQuotaValue xs = [(fst x, snd x) | x <- xs, snd x > quota]   

-- get a list of votes on selcted preference that goes under the quota
votesUnderQuota :: Ord a => [[a]] -> Int -> [(a, Int)]
votesUnderQuota votes n = sort (getVotesUnderQuotaValue (getVotesByPreference n votes))
    where 
        getVotesUnderQuotaValue xs = [(fst x, snd x) | x <- xs, snd x < quota]    

-- calculate the number of votes over the quota
calculateSurplus :: Ord a => [[a]] -> Int -> [Int]
calculateSurplus xs n = map countSurp $ getVotesOverQuotaValue $ getVotesByPreference n xs
    where
        countSurp ys  = snd ys - quota
        getVotesOverQuotaValue xs = [(fst x, snd x) | x <- xs, snd x > quota]  

-- remove all of a persons votes on a list when person is elected
removeCandidates :: Eq a => [a] -> [[a]] -> [[a]]
removeCandidates (x:xs) votes = rmempty (head (drop ((length (removal (x:xs) votes)) - 1) (removal (x:xs) votes) ))
        where
            removal [] votes = []
            removal (x:xs) votes = map (filter (/= x)) votes : removal xs (map (filter (/= x)) votes)  

-- retrieve items from main list where the input element is at the start
elementList :: Eq a => [a] -> [[a]] -> [[[a]]]
elementList [] xs = []
elementList (y:ys) xs = [ x | x <- xs, head x == y] : elementList ys xs

getVotesByElement :: [[a]] -> [a]
getVotesByElement votes = concat (elements voteLength votes)
    where
        elements [] xs = []
        elements (w:ws) xs = head (take 1 (drop (w - 1) xs)) :  elements ws xs
        voteLength = [1 .. length votes]

transferableVotes :: Ord a => [[a]] -> Int -> [(a, Int)]
transferableVotes votes n = sort (getVotesByPreference n (take surplus $ rmempty $ map (drop 1) $ validVotes))
    where
        surplus = sum (calculateSurplus votes n)
        cand = map fst (votesOverQuota votes n)
        validVotes = removeCandidates cand (getVotesByElement $ elementList cand votes)


-- test n = if length (votesOverQuota stdVotes n) > 0 = add to elected

-- test elec n = if (length (votesOverQuota stdVotes n)) > 0
--     then do
--         t <- elec ++ map fst (votesOverQuota stdVotes 1)
--         t
--         -- rmempty (removeElectedFromVotes (map fst (votesOverQuota stdVotes n)) stdVotes)
--     else []




x = [
    [("D",1),("E",2),("C",3),("B",3),("A",4)],
    [("D",1),("E",2),("C",2),("B",3),("A",4)],
    [("D",1),("E",2),("C",3),("B",4),("A",4)]]

xx = [
    ["A","B","C"],
    ["D","E","C"],
    ["D","E","B"],
    ["B","D","C"],
    ["B","C","C"],
    ["B","D","E"],
    ["A","E","B"],
    ["D","E","C"],
    ["A","E","B"],
    ["C","D","E"],
    ["A","E","B"]]


