-- Author: Emmanuel Sedicol
import Data.List


import TupleFunc
import DATA

quota :: Int
quota = 54

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
transferableVotes :: [(String, Int)] -> [[String]] -> [(String, Int)]
transferableVotes pref votes = sort (getVotesByPreference (take surplus $ rmempty $ map (drop 1) $ validVotes) 1)
    where
        surplus = sum (calculateSurplus pref)
        cand = map fst (compareGQuota pref)
        validVotes = removeCandidates cand (formateElementList $ elementList cand votes)

firstRoundFinal :: [(a, Int)] -> [a]
firstRoundFinal pref = [] ++ (map fst (compareGQuota pref))

secondRound :: [(String, Int)] -> [[String]] -> [(String, Int)]
secondRound pref votes = addListsOfTups transferVotes carryVotes
    where
        transferVotes = transferableVotes pref votes
        carryVotes = sort $ compareLQuota pref

secondRoundFinal :: [(String, Int)] -> [[String]] -> [String]
secondRoundFinal pref votes = (firstRoundFinal pref) ++ (map fst $ compareGQuota (secondRound pref votes))


transferableVotes3 :: [(String, Int)] -> [[String]] -> [(String, Int)]
transferableVotes3 pref votes = sort (getVotesByPreference (take surplus $ rmempty $ map (drop 1) $ validVotes) 1)
    where
        -- value of last vote minus quota
        surplus = head (map snd (compareGQuota (secondRound pref votes))) - quota
        cand = secondRoundFinal pref votes
        validVotes = removeCandidates cand (formateElementList $ elementList cand votes)   

thirdRound :: [(String, Int)] -> [[String]] -> [(String, Int)]
thirdRound pref votes = addListsOfTups transferVotes carryVotes
    where
        transferVotes = sort $ transferableVotes3 pref votes
        carryVotes = sort $ [x | x <- (secondRound pref votes), fst x /= head ((map fst $ compareGQuota (secondRound pref votes)))]

thirdRoundFinal :: [(String, Int)] -> [[String]] -> [String]
thirdRoundFinal pref votes = (secondRoundFinal pref votes) ++ (map fst $ compareGQuota (thirdRound pref votes))

orderOfVotes :: [[Char]]
orderOfVotes = ["1st", "2nd", "3rd", "4th"]

finalStanding pref votes choice = take choice $ zip orderOfVotes (thirdRoundFinal pref votes)
pp = getVotesByPreference stdVotes 1







