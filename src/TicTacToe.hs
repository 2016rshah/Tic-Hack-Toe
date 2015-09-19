module TicTacToe where

import Data.Either

data Symbol = X | O 
	deriving (Show, Eq)

type Piece = Either Int Symbol

type Three = [Piece] -- how do I constrain this to a length of three?

data Board = Board Three Three Three
	deriving (Show, Eq)

emptyBoard :: Board
emptyBoard = Board [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]

full :: Three -> Bool
full ts@[a,b,c] = noLefts && allEqual
	where 
		noLefts = foldl (\acc curr -> acc && (isRight curr)) True ts 
		allEqual = a == b && b == c

won :: Board -> Bool
won b = foldl (\acc curr -> acc || (full curr)) False ((rows b) ++ (cols b) ++ (diags b))

rows :: Board -> [Three]
rows (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [x, y, z]

cols :: Board -> [Three]
cols (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

diags :: Board -> [Three]
diags (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [[a, e, i], [c, e, g]]


