module Main where

import TicTacToe
import System.IO

-- |Greet user, start recursion, and say farewell to user when game ends
main :: IO ()
main = do
	putStrLn "Welcome to tic tac toe."

	--start recursion
	playerMove emptyBoard

	putStrLn "Thanks for playing!"

-- |Helper function to ensure prompt is made before user is expected to input something
promptLine :: String -> IO String
promptLine text = do
    putStr text
    hFlush stdout
    getLine

-- |Code common to both player and computer at the end of the game
endRecursion :: Board -> IO ()
endRecursion b = do
	putStrLn (show b)
	putStrLn (winner b) -- end recursion

-- |Grab the user's move, and feed that to tic-tac-toe. Recurse as needed. 
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

-- |Make a decision based on the board on where to move. Recurse as needed.
compMove :: Board -> IO ()
compMove board = do
	let newBoard = makeXMove board
	if won newBoard || draw newBoard
		then endRecursion newBoard 	-- end recursion
		else playerMove newBoard 	-- continue recursion

