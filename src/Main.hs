module Main where

import Player
import Board
import Game

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  startGame initBoard HumanPlayer HumanPlayer
