module Game where

import Board
import Player
import Utils

startGame :: (BasePlayer a) => Board -> a -> a -> IO ()
startGame board a b = do
  printBoard board
  turn <- getMove a board (TurnType Zero)
  let newBoard = makeTurn board turn Zero
    in do
      turn2 <- getMove b newBoard (TurnType Cross)
      let newBoard2 = makeTurn newBoard2 turn2 Cross
        in printBoard newBoard2
