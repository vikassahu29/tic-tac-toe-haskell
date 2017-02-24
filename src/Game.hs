module Game where

import Board
import Player

startGame :: (BasePlayer a) => [[BoardPos]] -> a -> a -> IO ()
startGame _ _ _ = putStrLn "Game Started"
