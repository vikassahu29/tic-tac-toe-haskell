module Game where

import Board
import Player
import Utils

startGame :: (BasePlayer a) => Board -> a -> a -> Bool -> IO ()
startGame board a b chance = do
  let player = selectPlayer a b chance
  let cType = selectTurn chance
  printBoard board
  putStrLn $ "Player " ++ (show cType) ++ " Turn"
  turn <- getMove player board (TurnType cType)
  let newBoard = makeTurn board turn cType
  if hasPlayerAtThisPosWon newBoard turn
    then putStrLn $ "Player " ++ (show cType) ++ " Wins"
    else startGame newBoard a b (not chance)
  where selectPlayer a _ True = a
        selectPlayer _ b False = b
        selectTurn True = Zero
        selectTurn False = Cross
