module Utils
( printBoard
) where 

    import Board
    import Data.List

    printBoard :: Board -> IO ()
    printBoard xs = putStrLn (_strBoard(getCharBoard xs))

    getCharBoard :: Board -> [[Char]]
    getCharBoard []      = []
    getCharBoard (x:xs) = map getQChar x : getCharBoard xs

    getQChar :: Cell -> Char
    getQChar (TurnType Cross) = 'X'
    getQChar (TurnType Zero) = 'O'
    getQChar Empty = '-'

    _strBoard :: [[Char]] -> [Char]
    _strBoard xs = intercalate (take (2 * (length xs) - 1) (repeat '-') ++ "\n") (map _strRow xs)

    _strRow :: [Char] -> [Char]
    _strRow xs = (intersperse '|' xs) ++ "\n"