module Main where

import Clean.Clean
import Voting.STV
import Voting.AlternativeVoting
import TupleFunctions.Tuple

import Lib

main :: IO ()
main = do
    csvData <- readFile "votes.csv"

    let csv = parseCsv csvData
    let cleanData = stdVotes csv
    let preferenceVotes = getVotesByPreference cleanData 1
    let altenativeVote = getWinner cleanData

    introduction

    putStrLn "\n\t Select one of the given voting methods"
    putStrLn "\t\t 1) Single Transferable Voting"
    putStrLn "\t\t 2) Alternative Voting \n"

    choice <- getLine

    if choice == "1" then do  
        putStr "\ESC[2J"
        putStrLn "\n------------------------------------------"
        putStrLn "\t Single Transferable Voting"
        putStrLn "------------------------------------------"
        putStrLn "\nWelcome, please specify the number of candidates to elect"

        stv_choice <- getLine

        if stv_choice == "1" then do  
            putStrLn "\t SORRY THERE CAN ONLY BE MORE THAN ONE WINNER \n\n"
            main
        else if stv_choice =="2" then do
            mapM_ print $ (finalStanding preferenceVotes cleanData 2)
            main
        else if stv_choice =="3" then do
            mapM_ print $ (finalStanding preferenceVotes cleanData 3)
            main
        else if stv_choice =="4" then do
            mapM_ print $ (finalStanding preferenceVotes cleanData 4)
            main
        else do
            putStrLn "\n\t  🤡🤡🤡 EMPTY INPUT OR PASSED MAX CANDIDATES 🤡🤡🤡 \n\n"
            main

    else if choice =="2" then do
        putStr "\ESC[2J"
        putStrLn "\n----------------------------------"
        putStrLn "\t Alternative Voting"
        putStrLn "----------------------------------"
        putStrLn "The Winner is"
        putStrLn altenativeVote
        main
    else do
        putStrLn "\n\t  INVALID INPUT \n\n"
        main

introduction :: IO ()
introduction = do
    putStrLn "\n\n-------------------------------------------------------------------------"
    putStrLn "\t 🐶🐶🐶 Welcome to Emmanuel Sedicol Voting System 🐶🐶🐶"
    putStrLn "---------------------------------------------------------------------------\n"




        




    