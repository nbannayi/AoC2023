-- Advent of Code 2023, Day 15 - Lens Library.
-- Haskell.

import Data.List.Split (splitOn)

-- Hash a single string.
hashString :: String -> Int
hashString = foldl (\acc char -> 17 * (acc + fromEnum char) `mod` 256) 0

-- Hash a list of strings.
hashStringList :: [String] -> Int
hashStringList = foldl (\acc step -> acc + hashString step) 0

main :: IO ()
main = do    
    -- Parse the input into a list of steps.
    inputData <- readFile "Day15Input.txt"
    let steps = splitOn "," inputData    

    -- Part 1.    
    putStrLn $ "Part 1 answer: " ++ show (hashStringList steps)