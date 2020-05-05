module Clean.Clean where

import Data.List.Split (splitOn)
import TupleFunctions.Tuple

numberOfCandidates :: Int
numberOfCandidates = 4

convertToList :: String -> [String]
convertToList vote = splitOn "," vote

parseCsv :: String -> [[String]]
parseCsv csvData = map convertToList (tail (splitOn "\n" csvData))

--------------------- FORMAT AND CLEAN DATA -------------------------
-- change votes from String to Integer
voteConversion :: String -> Int  
voteConversion a | a == "1" = 1| a == "2" = 2 | a == "3" = 3
                  | a == "4" = 4 | a == "5" = 5| otherwise = 0

votes :: [[String]] -> [[Int]]
votes dataVotes = map (map voteConversion) (drop 1 (map (drop 2) dataVotes))

candidates :: [String]
-- candidates = [(x) | x <- drop 2 (head dirtyVotes)]
candidates = ["A1", "B2", "C3", "D4", "E5"]

sortedVotes :: [[String]] -> [[(String, Int)]]
sortedVotes dataVotes = filter (/=[]) $ map sortTuplesAscending $ map removeBlankVotes $ map (zip candidates) (votes dataVotes)

cleanVotes :: [[String]] -> [[(String, Int)]]
cleanVotes dataVotes = filter (/=[]) $ map removeBlankVotes $ map checkPreferenceOrder (map checkDuplicatesInTuple (sortedVotes dataVotes))

-- stdVotesWithWeights :: [([String], Int)]
-- stdVotesWithWeights data = map (\xs -> ([fst x | x <- xs], weight)) (cleanVotes data)

stdVotes :: [[String]] -> [[String]]
stdVotes dataVotes = map (\xs -> [fst x | x <- xs]) (cleanVotes dataVotes)


