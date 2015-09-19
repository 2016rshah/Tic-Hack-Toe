module Main where

import TicTacToe
import System.IO


main :: IO ()
main = do
	putStrLn "Welcome to tic tac toe."

	--start recursion
	playerMove emptyBoard

	putStrLn "Thanks for playing!"

promptLine :: String -> IO String
promptLine text = do
    putStr text
    hFlush stdout
    getLine

endRecursion :: Board -> IO ()
endRecursion b = do
	putStrLn (show b)
	putStrLn (winner b) -- end recursion

playerMove :: Board -> IO ()
playerMove board = do
	putStrLn (show board)
	loc <- promptLine "Where do you want to place your O? "
	putStrLn ""
	let moveLoc = Left (read loc)
	let newBoard = findAndReplace board moveLoc (Right O)
	if won newBoard || draw newBoard
		then endRecursion newBoard
		else compMove newBoard			-- continue recursion

compMove :: Board -> IO ()
compMove board = do
	let newBoard = makeXMove board
	if won newBoard || draw newBoard
		then endRecursion newBoard 	-- end recursion
		else playerMove newBoard 	-- continue recursion

