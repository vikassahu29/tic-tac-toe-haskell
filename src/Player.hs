module Player where

import Board 

class BasePlayer a where
getMove :: a -> Board -> Cell -> IO (Int, Int)

data HumanPlayer = HumanPlayer
instance BasePlayer HumanPlayer where
getMove _ board t = 
     do 
          putStrLn "Enter x "
          x <- getLine
          putStrLn "Enter y "
          y <- getLine
          return (read x,read y)
