module TicTacToe where

{-# LANGUAGE FlexibleInstances #-}


import Data.Either
import Safe
import Data.Maybe
import Data.List

-- |Slots in the board can either be filled with Naughts or Crosses
data Symbol = X | O
	deriving (Show, Eq)

-- |Empty slots are referred to by their location on the board
type Piece = Either Int Symbol

-- |A set of three pieces is used to represent rows, columns, and diagonals
type Three = [Piece] -- how do I constrain this to a length of three?

-- |The game board is made up of three rows of three pieces each.
data Board = Board Three Three Three
	deriving (Eq)

--Thank you to http://projects.haskell.org/operational/examples/TicTacToe.hs.html for the show function
instance Show Board where
	show board =
	      unlines . surround "+---+---+---+"
	    . map (concat . surround "|". map showSquare)
	    $ (rows board)
	    where
	    surround x xs = [x] ++ intersperse x xs ++ [x]
	    showSquare = either (\n -> " " ++ show n ++ " ") (\n -> " " ++ show n ++ " ")

-- |Convenience function for constructing an empty board
emptyBoard :: Board
emptyBoard = Board [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]

-- |Given either a row, column, or diagonal, it checks whether it is entirely filled with naughts or crosses
full :: Three -> Bool
full ts@[a,b,c] = noLefts && allEqual
	where
		noLefts = foldl (\acc curr -> acc && (isRight curr)) True ts
		allEqual = a == b && b == c

-- |Given a game board, check whether the game is over because someone won
won :: Board -> Bool
won b = foldl (\acc curr -> acc || (full curr)) False ((rows b) ++ (cols b) ++ (diags b))

-- |Given a game board, check whether the game is over due to a draw
draw :: Board -> Bool
draw b = length (possibleMoves b) == 0

-- |Message to display to the user about the results of the game
winner :: Board -> String
winner b = if length winnerType > 0 then head winnerType else "It was a draw!"
	where
		allConfigs = ((rows b) ++ (cols b) ++ (diags b))
		winnerType = [if a == (Right O) then "The computer wins!" else "You win!" | curr@[a,b,c] <- allConfigs, full curr]

-- |Extract rows from game board
rows :: Board -> [Three]
rows (Board x y z) = [x, y, z]

-- |Extract columns from game board
cols :: Board -> [Three]
cols (Board [a, b, c] [d, e, f] [g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

-- |Extract diagonals from game board
diags :: Board -> [Three]
diags (Board [a, _, c] [_, e, _] [g, _, i]) = [[a, e, i], [c, e, g]]

-- |List of places where a piece can be placed
possibleMoves :: Board -> [Piece]
possibleMoves board = filter isLeft (boardToList board)

-- |Helper function to convert a board into a list of values
boardToList :: Board -> [Piece]
boardToList (Board x y z) = x ++ y ++ z

-- |Helper function to convert a list of values into a board
listToBoard :: [Piece] -> Board
listToBoard [a,b,c,d,e,f,g,h,i] = Board [a,b,c] [d,e,f] [g,h,i]

-- |Function to update the game board with a new value at a specified point
findAndReplace :: Board -> Piece -> Piece -> Board
findAndReplace b p1 p2 = listToBoard [if x==p1 then p2 else x | x <- bl]
	where bl = boardToList b

-- |Check if O's can immediately win, and if so, do it
winningOMove :: Board -> Maybe Board
winningOMove b = headMay [findAndReplace b p (Right O) | p <- (possibleMoves b), won (findAndReplace b p (Right O))]

-- |Check if X's can immediately win, and if so, block it
blockXWin :: Board -> Maybe Board
blockXWin b = headMay [findAndReplace b p (Right O) | p <- (possibleMoves b), won (findAndReplace b p (Right X))]

-- |Check whether a board has been forked
isFork :: Board -> Bool
isFork b = 2 == length [findAndReplace b p (Right O) | p <- (possibleMoves b), won (findAndReplace b p (Right O))]

-- |Check if O's can make a fork, and if so, do it
forkO :: Board -> Maybe Board
forkO b = headMay [findAndReplace b p (Right O) | p <- (possibleMoves b), isFork (findAndReplace b p (Right O))]

-- |Check if X's can make a fork, and if so, block it
blockXFork :: Board -> Maybe Board
blockXFork b = headMay [findAndReplace b p (Right O) | p <- (possibleMoves b), isFork (findAndReplace b p (Right X))]

-- |Decision tree for AI that will go down the list to make its move
makeOMove :: Board -> Board
makeOMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
	| isJust (winningOMove board) 	= fromJust (winningOMove board)
	| isJust (blockXWin board) 		= fromJust (blockXWin board)
	| isJust (forkO board) 			= fromJust (forkO board)
	| isJust (blockXFork board) 	= fromJust (blockXFork board)
	| elem e (possibleMoves board)	= findAndReplace board e (Right O)
	| otherwise 					= if length (possibleMoves board) > 0
		then findAndReplace board (head (possibleMoves board)) (Right O)
		else board --This should not happen
