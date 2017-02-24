module Board where

import Data.List (intersperse, intercalate)

data TurnType = Cross | Zero
  deriving Show

data BoardPos = TurnType TurnType | Empty
  deriving Show

getQChar :: BoardPos -> Char
getQChar $ TurnType Cross = 'X'
getQChar $ TurnType Zero = 'O'
getQChar Empty = ' '

getCharBoard :: [[BoardPos]] -> [[Char]]
getCharBoard []      = []
getCharBoard (x: xs) = map getQChar x : getCharBoard xs


{- Print Board -}
_strRow :: [Char] -> [Char]
_strRow xs = (intersperse '|' xs) ++ "\n"

_strBoard :: [[Char]] -> [Char]
_strBoard xs = intercalate (take (2 * (length xs) - 1) (repeat '-') ++ "\n") (map _strRow xs)

printBoard :: [[BoardPos]] -> IO ()
printBoard xs = putStrLn (_strBoard(getCharBoard xs))
{- Print Board -}


initBoard :: [[BoardPos]]
initBoard = [[Empty,Empty,Empty], [Empty,Empty,Empty], [Empty,Empty,Empty]]

{- Validate Board, HumanPlayer Input, Game Loop-}
