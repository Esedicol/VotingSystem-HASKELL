module Main where

import Clean.Clean
import Voting.STV
import TupleFunctions.Tuple

import Lib

main :: IO ()
main = do
    csvData <- readFile "votes.csv"
    let csv = parseCsv csvData
    let stdData = stdVotes csv
    let pp = getVotesByPreference stdData 1
    print (thirdRoundFinal pp stdData)


    