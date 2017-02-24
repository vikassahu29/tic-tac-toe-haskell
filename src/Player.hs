module Player where

import Board

class BasePlayer a where

getMove :: a -> b -> TurnType -> IO (Int, Int)

data HumanPlayer = HumanPlayer

instance BasePlayer HumanPlayer where

getMove _ _ _ = return (0,0)
