module Main where

import Player
import Board
import Game

main :: IO ()
main = do
  putStrLn "Play Tic-Tac-Toe!"
  startGame initBoard HumanPlayer HumanPlayer True
