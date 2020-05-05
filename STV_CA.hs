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
countVoteFrequency :: Eq a => [a] -> [(a, Int)]
countVoteFrequency xs = countElements (unique xs) xs  
    where 
        unique [] = []
        unique (x:xs) = x : unique (filter (/= x) xs)  
        countElements label targetPreference =  
            sortTuplesDescending . zip label $ map length $ map ($ targetPreference) (zipWith ($) (repeat (filter . (==))) label) 
            
--------------------- VOTING FUNCTIONS -------------------------
rmempty :: [[String]] -> [[String]]
rmempty = filter (/= [])

-- count of all unique value in the selected preference

getVotesByPreference :: [[String]] -> Int -> [(String, Int)]
getVotesByPreference votes n = [(head $ fst x, snd x) | x <- freq votes n]
    where
        freq vs n = countVoteFrequency $ rmempty (map (drop (n - 1)) (map (take n) vs ))      
          
pp :: [(String, Int)]
pp = getVotesByPreference stdVotes 1

-- compare list to quota and create list of items the passed the condition
compareGQuota :: [(a, Int)] -> [(a, Int)]
compareGQuota xs = [(fst x, snd x) | x <- xs, snd x > quota] 

-- compare list to quota and create list of items the passed the condition
compareLQuota :: [(a, Int)] -> [(a, Int)]
compareLQuota xs = [(fst x, snd x) | x <- xs, snd x < quota] 

-- calculate the number of votes over the quota
calculateSurplus :: [(a, Int)] -> [Int]
calculateSurplus pref = map (\x -> snd x - quota) $ compareGQuota $ pref

-- remove all of a persons votes on a list when person is elected
removeCandidates :: [String] -> [[String]] -> [[String]]
removeCandidates (x:xs) votes = rmempty (head (drop ((length (removal (x:xs) votes)) - 1) (removal (x:xs) votes) ))
        where
            removal [] votes = []
            removal (x:xs) votes = map (filter (/= x)) votes : removal xs (map (filter (/= x)) votes)  

-- retrieve items from main list where the input element is at the start
elementList :: Eq a => [a] -> [[a]] -> [[[a]]]
elementList [] xs = []
elementList (y:ys) xs = [ x | x <- xs, head x == y] : elementList ys xs

-- format element list into a sinlge array of tuples
formateElementList :: [[a]] -> [a]
formateElementList votes = concat (elements voteLength votes)
    where
        elements [] xs = []
        elements (w:ws) xs = head (take 1 (drop (w - 1) xs)) :  elements ws xs
        voteLength = [1 .. length votes]

        
-- list of valid transferable votes        
transferableVotes :: [(String, Int)] -> [(String, Int)]
transferableVotes pref = sort (getVotesByPreference (take surplus $ rmempty $ map (drop 1) $ validVotes) 1)
    where
        surplus = sum (calculateSurplus pref)
        cand = map fst (compareGQuota pref)
        validVotes = removeCandidates cand (formateElementList $ elementList cand stdVotes)

firstRoundFinal :: [(a, Int)] -> [a]
firstRoundFinal pref = [] ++ (map fst (compareGQuota pref))

secondRound :: [(String, Int)] -> [(String, Int)]
secondRound pref = addListsOfTups transferVotes carryVotes
    where
        transferVotes = transferableVotes pref
        carryVotes = sort $ compareLQuota pref

secondRoundFinal :: [(String, Int)] -> [String]
secondRoundFinal pref = (firstRoundFinal pref) ++ (map fst $ compareGQuota (secondRound pref))


transferableVotes3 :: [(String, Int)] -> [(String, Int)]
transferableVotes3 pref = sort (getVotesByPreference (take surplus $ rmempty $ map (drop 1) $ validVotes) 1)
    where
        -- value of last vote minus quota
        surplus = head (map snd (compareGQuota (secondRound pref))) - quota
        cand = secondRoundFinal pref
        validVotes = removeCandidates cand (formateElementList $ elementList cand stdVotes)   
       
thirdRound :: [(String, Int)] -> [(String, Int)]
thirdRound pref = addListsOfTups transferVotes carryVotes
    where
        transferVotes = sort $ transferableVotes3 pref
        carryVotes = sort $ [x | x <- (secondRound pref), fst x /= head ((map fst $ compareGQuota (secondRound pref)))]

thirdRoundFinal :: [(String, Int)] -> [String]
thirdRoundFinal pref = (secondRoundFinal pref) ++ (map fst $ compareGQuota (thirdRound pref))