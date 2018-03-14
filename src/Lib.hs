module Lib
    ( getLines
    , skew
    , findWord
    , findWords
    , findWordInLine
    , formatGrid
    , outputGrid
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

getLines :: Grid -> Grid
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonalLeft = diagonalize grid
      diagonalRight = diagonalize (map reverse grid)
      lines = vertical ++ horizontal ++ diagonalLeft ++ diagonalRight
  in lines ++ map reverse lines

diagonalize :: Grid -> Grid
-- diagonalize grid =  transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines
