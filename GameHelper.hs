module GameHelper where

import Data.Char
import TypeDef

---- Helper Functions
-- | converts Board Character to Number
bNameToInt :: Char -> Int
bNameToInt bName = (ord bName) - (ord 'A')

-- | converts Board Number to Character
bNameFromInt :: Int -> Char
bNameFromInt bInt = (chr (bInt + ord 'A'))

-- | returns an indexed tuple for each element in the lst
indexer :: [a] -> [(Int, a)]
indexer lst = zip [0 ..] lst

-- | convert lst to seperate at 3
split3 :: [a] -> [[a]]
split3 [] = []
split3 (x:y:z:lst) = [x, y, z] : (split3 lst)
