-- Advent of Code 2023, Day 15 - Lens Library.
-- Haskell.

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HashMap

-- Hash a single string.
hashString :: String -> Int
hashString = foldl (\acc char -> 17 * (acc + fromEnum char) `mod` 256) 0

-- Hash a list of strings.
hashStringList :: [String] -> Int
hashStringList = foldl (\acc step -> acc + hashString step) 0

-- Returns a tuple of lens label and focal length for an add, or lens label 
-- and "" for a remove. 
getLensDetails :: String -> (String, String)
getLensDetails step =
    let (first, second) = 
            case splitOn "=" step of
                (first':second':_) -> (first', second')
                _                  -> ("", "")
    in
        case first of
            "" ->
                case splitOn "-" step of
                    (first':_) -> (first', "")
                    _          -> ("", "")
            _ -> (first, second)

-- Replace a given element in a list.
replaceElement :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
replaceElement newElement list =
    if length (filter (\(elem, _) -> elem == fst newElement) list) == 0
        then
            list ++ [newElement]
        else
            map (\element -> if fst newElement == fst element then newElement else element) list

-- Remove a given element in a list.
removeElement :: Eq a => a -> [(a, b)] -> [(a, b)]
removeElement element list =
    filter (\(elem, _) -> elem /= element) list

-- Update hash map.
updateHashMap :: HashMap.HashMap String [(String, String)] -> String -> HashMap.HashMap String [(String, String)]
updateHashMap hashMap step =
    let (label, focalLength) = getLensDetails step
        key = show (hashString label)
        value = case HashMap.lookup key hashMap of
                    Just v  -> v
                    Nothing -> []
    in
        -- Replace element.
        if focalLength /= ""
        then
            let newValue = replaceElement (label, focalLength) value
            in HashMap.insertWith (\newValue _ -> newValue) key newValue hashMap
        -- Remove element.
        else
            let newValue = removeElement label value
            in HashMap.insertWith (\newValue _ -> newValue) key newValue hashMap

-- Functions for part 2 - focusing power.
mapOverHashMap :: (k -> v1 -> v2) -> HashMap.HashMap k v1 -> HashMap.HashMap k v2
mapOverHashMap f = HashMap.mapWithKey f
getFocusingPower :: String -> [(String,String)] -> (Int, Int)
getFocusingPower key value =
    let boxNoPlus1 = (read key :: Int) + 1
    in foldl (\(acc,total) (_, focalLength) -> (acc+1, total + boxNoPlus1 * (acc+1) * (read focalLength :: Int))) (0,0) value

main :: IO ()
main = do    
    -- Parse the input into a list of steps.
    inputData <- readFile "Day15Input.txt"
    let steps = splitOn "," inputData    

    -- Part 1.    
    putStrLn $ "Part 1 answer: " ++ show (hashStringList steps)

    -- Part 2.    
    let hashMap = HashMap.empty :: HashMap.HashMap String [(String,String)]
    let hashMap' = foldl (\acc step -> updateHashMap acc step) hashMap steps
    let focusingPowerList = HashMap.elems (mapOverHashMap getFocusingPower hashMap')
    let totalFocusingPowers = foldl (\acc (_,fp) -> acc + fp) 0 focusingPowerList
    putStrLn $ "Part 2 answer: " ++ show (totalFocusingPowers)