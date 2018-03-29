module Utils where

import Control.Monad
import System.Random
import System.IO

data Difficulty = Easy |  Medium |  Hard deriving (Show, Eq)

getDifficulty :: IO Difficulty
getDifficulty = do
			putLine "Select difficulty: Easy | Medium |  hard"
			diff <- getLine
			return $f diff
			where f x |x == "Easy"   || x == "easy"   = Easy
				  |x == "Medium" || x == "medium" = Medium
				  |x == "Hard"   || x == "hard"   = Hard
				  |otherwise =  error "Invalid Difficulty"
putLine :: String -> IO ()
putLine = putStrLn

readLinesFile :: String -> IO [String]
readLinesFile filePath = do
			text <- readFile filePath
			return $lines text
