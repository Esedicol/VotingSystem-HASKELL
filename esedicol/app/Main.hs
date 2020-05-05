module Main where

import Clean.Clean
import Voting.STV
import TupleFunctions.Tuple

import Lib

main :: IO ()
main = do
    csvData <- readFile "votes.csv"

    let csv = parseCsv csvData
    let cleanData = stdVotes csv
    let preferenceVotes = getVotesByPreference cleanData 1

    putStrLn "\n\t ------------------------------------------------------------------------"
    putStrLn "\t 🐶🐶🐶 Welcome, please specify the number of candidates to elect 🐶🐶🐶"
    putStrLn "\t ------------------------------------------------------------------------\n"
    choice <- getLine

    if choice == "1" then do  
        putStrLn "\t 🤡🤡🤡 SORRY THERE CAN ONLY BE MORE THAN ONE WINNER 🤡🤡🤡 \n\n"
        main
    else if choice =="2" then do
        mapM_ print $ (finalStanding preferenceVotes cleanData 2)
        main
    else if choice =="3" then do
        mapM_ print $ (finalStanding preferenceVotes cleanData 3)
        main
    else if choice =="4" then do
        mapM_ print $ (finalStanding preferenceVotes cleanData 4)
        main
    else do
        putStrLn "\n\t  🤡🤡🤡 EMPTY INPUT OR PASSED MAX CANDIDATES 🤡🤡🤡 \n\n"
        main


        




    