module Main where

import WordsByChar
import Data.List
import Data.String
import Data.Function.Memoize

startswith :: String -> Char -> Bool
startswith [] _ = False
startswith x  y = (==) (head x) y
--
getSolution :: String -> Char -> [String]
getSolution x y = nub $ intersect (permutations x) (memoizedWords y)
    where
        memoizedWords = memoize wordsByChar

memoizedgetSolution :: String -> Char -> [String]
memoizedgetSolution = memoize2 getSolution

main :: IO ()
main = do
    putStrLn ("\nEnter words to unscramble and first letter")
    input <- getLine
    let splittedInput = (words input)
    let letters = head splittedInput
    let first = head $ head $ drop 1 splittedInput
    putStrLn ("Finding matches for " ++ letters ++ " starting with (" ++ [first] ++ ")...")
    print (memoizedgetSolution letters first)
    main
