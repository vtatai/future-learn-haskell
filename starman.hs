module Starman where

import System.IO
import System.Random

check :: String -> String -> Char -> (Bool,String)
check word display c = (c `elem` word, [if x==c then c else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn "You lose"
       else if word==display
              then putStrLn "You win!"
              else mkguess word display n

mkguess :: String -> String -> Int -> IO()
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

mkguessfile :: Int -> IO()
mkguessfile n =
    do wordMask <- randomWordMask
       mkguess (fst wordMask) (snd wordMask) n

randomWordMask :: IO (String, String)
randomWordMask =
    do 
        word <- randomWord
        let mask = take (length word) $ repeat '-'
        return (word, mask)

randomWord :: IO String
randomWord =
    do
        words <- readLines "/Users/vtatai/src/future-learn-haskell/words_alpha.txt"
        random <- randomRIO (0, length words)
        let x = words !! random
        return (words !! random)

readLines :: String -> IO [String]
readLines file = (readFile file) >>= return . lines

