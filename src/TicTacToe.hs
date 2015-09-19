{-# LANGUAGE FlexibleInstances #-}
module TicTacToe where

import Data.Either
import Safe
import Data.Maybe
import Data.List
import Data.List.Split

data Symbol = X | O 
	deriving (Show, Eq)

type Piece = Either Int Symbol

showHuman :: Piece -> Char
showHuman (Left x) = '.'
showHuman (Right x) = head (show x)

type Three = [Piece] -- how do I constrain this to a length of three?

data Board = Board Three Three Three
	deriving (Eq)

--instance Show Board where
--	show board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) 
--		= intercalate "\n" (chunksOf 3 (map showHuman (boardToList board)))

--Thank you to http://projects.haskell.org/operational/examples/TicTacToe.hs.html for the show function
instance Show Board where
	show board =
	      unlines . surround "+---+---+---+"
	    . map (concat . surround "|". map showSquare)
	    $ (rows board)
	    where
	    surround x xs = [x] ++ intersperse x xs ++ [x]
	    showSquare = either (\n -> " " ++ show n ++ " ") (\n -> " " ++ show n ++ " ")

emptyBoard :: Board
emptyBoard = Board [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]

full :: Three -> Bool
full ts@[a,b,c] = noLefts && allEqual
	where 
		noLefts = foldl (\acc curr -> acc && (isRight curr)) True ts 
		allEqual = a == b && b == c

won :: Board -> Bool
won b = foldl (\acc curr -> acc || (full curr)) False ((rows b) ++ (cols b) ++ (diags b))

draw :: Board -> Bool
draw b = length (possibleMoves b) == 0

--Message to display to the user about the results of the game
winner :: Board -> String
winner b = head [if a == (Right X) then "The computer wins!" else "You win!" | curr@[a,b,c] <- allConfigs, full curr]
	where allConfigs = ((rows b) ++ (cols b) ++ (diags b))

rows :: Board -> [Three]
rows (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [x, y, z]

cols :: Board -> [Three]
cols (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

diags :: Board -> [Three]
diags (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [[a, e, i], [c, e, g]]

possibleMoves :: Board -> [Piece]
possibleMoves board = filter isLeft (boardToList board)

boardToList :: Board -> [Piece]
boardToList (Board x y z) = x ++ y ++ z

listToBoard :: [Piece] -> Board
listToBoard [a,b,c,d,e,f,g,h,i] = Board [a,b,c] [d,e,f] [g,h,i]

findAndReplace :: Board -> Piece -> Piece -> Board
findAndReplace b p1 p2 = listToBoard [if x==p1 then p2 else x | x <- bl]
	where bl = boardToList b

winningXMove :: Board -> Maybe Board
winningXMove b = headMay [findAndReplace b p (Right X) | p <- (possibleMoves b), won (findAndReplace b p (Right X))]

blockOWin :: Board -> Maybe Board
blockOWin b = headMay [findAndReplace b p (Right X) | p <- (possibleMoves b), won (findAndReplace b p (Right O))]

isFork :: Board -> Bool
isFork b = 2 == length [findAndReplace b p (Right X) | p <- (possibleMoves b), won (findAndReplace b p (Right X))]

forkX :: Board -> Maybe Board
forkX b = headMay [findAndReplace b p (Right X) | p <- (possibleMoves b), isFork (findAndReplace b p (Right X))]

blockOFork :: Board -> Maybe Board
blockOFork b = headMay [findAndReplace b p (Right X) | p <- (possibleMoves b), isFork (findAndReplace b p (Right O))]

makeXMove :: Board -> Board
makeXMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
	| isJust (winningXMove board) = fromJust (winningXMove board)
	| isJust (blockOWin board) = fromJust (blockOWin board)
	| isJust (forkX board) = fromJust (forkX board)
	| isJust (blockOFork board) = fromJust (blockOFork board)
	| e `elem` (possibleMoves board) = findAndReplace board e (Right X)
	| otherwise = if length (possibleMoves board) > 0 
		then findAndReplace board (head (possibleMoves board)) (Right X)
		else board 

