module Hangman where

import Utils
import System.IO
import System.Random
import Control.Monad


diffFilter :: Difficulty -> IO [String] -> IO [String]
diffFilter diff | diff == Easy   = liftM $wordFilter (4,6)
		| diff == Medium = liftM $wordFilter (7,9)
		| diff == Hard   = liftM $wordFilter (10,15)

wordFilter :: (Int, Int) -> [String] -> [String]
wordFilter (min, max) = filter (lenFilter min max)

lenFilter :: Int -> Int -> String -> Bool
lenFilter min max str = (length str >= min) && (length str <= max) 

randomWord :: Difficulty -> IO String
randomWord diff = do
		words <- diffFilter diff $readLinesFile "data/dict.txt"
		index <- randomRIO (0, length words -1)
		return $ words!!index

showProgress :: Int -> IO String -> IO String -> IO ()
showProgress tries letters word = do 
					putLine ("[" ++ (take (7 - tries) ['X', 'X' ..]) ++ (take tries ['-', '-' ..]) + "]")
					putLine ("[" ++ (liftM $wordHidden word letters)  ++"]")

wordHidden :: String -> String -> String
wordHidden word letters = [y | x<-word, if x `elem` letters then x else '-']

playGame :: Int -> String -> IO String -> IO ()
playGame 0 _ _ = putLine "You Lost!!"
playGame _ letters word | letters == word = putLine "You Won!!!!"
playGame tries letters word = do
				c <- getChar
				if c `elem` letters then do
							putLine $"Already Guessed this Letter:" ++ c
							playGame tries letters word
						    else do
							letters <- [c] ++ letters
							if c `elem` word then do
										putLine "Correct Guess!!!"
										showProgress tries letters word
									  else do
										putLine "Alas!! wrong guess"
										tries <- tries - 1
										showProgress tries letters word
							playGame tries letters word

newGame :: IO String -> IO  ()
newGame = playGame 7 []

main = do
	diff <- getDifficulty
	word <- randomWord diff
	newGame word
