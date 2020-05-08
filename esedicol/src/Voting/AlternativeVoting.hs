-- Author: Emmanuel Sedicol
module Voting.AlternativeVoting where

import Data.List

import TupleFunctions.Tuple
import Clean.Clean

countFppVoteFrequency :: Eq a => [a] -> [(a, Int)]
countFppVoteFrequency xs = countElements (unique xs) xs  
    where 
        unique [] = []
        unique (x:xs) = x : unique (filter (/= x) xs)  
        countElements label targetPreference =  
            sortTuplesDescending . zip label $ map length $ map ($ targetPreference) (zipWith ($) (repeat (filter . (==))) label) 

getFirstPastPost :: Ord b => [b] -> [(Int, b)]
getFirstPastPost vs = sort [(snd v, fst v) | v <- countFppVoteFrequency vs]

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . getFirstPastPost . map head

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

getWinner :: [[String]] -> String
getWinner bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> getWinner (elim c bs)