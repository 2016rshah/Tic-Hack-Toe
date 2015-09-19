module Main where

import TicTacToe

main :: IO ()
main = do
	putStrLn "Welcome to tic tac toe. Where do you want to move first?"

	let board1 = emptyBoard
	putStrLn (show board1)
	loc1 <- getLine
	let moveLoc1 = read loc1
	let board2 = findAndReplace board1 (Left moveLoc1) (Right O)

	let board3 = makeXMove board2
	putStrLn (show board3)
	loc2 <- getLine
	let moveLoc2 = read loc2
	let board4 = findAndReplace board3 (Left moveLoc2) (Right O)

	let board5 = makeXMove board4
	putStrLn (show board5)
	loc3 <- getLine
	let moveLoc3 = read loc3
	let board6 = findAndReplace board5 (Left moveLoc3) (Right O)

	let board7 = makeXMove board6
	putStrLn (show board7)
	loc4 <- getLine
	let moveLoc4 = read loc4
	let board8 = findAndReplace board7 (Left moveLoc4) (Right O)


	putStrLn (show board8)
