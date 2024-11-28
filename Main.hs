module Main where

import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Board

-- | Converts a player to its character representation.
-- Example: Player 1 -> 'O', Player 2 -> 'X', Empty -> '.'
playerToChar :: Int -> Char
playerToChar p
  | p == 1 = 'O'
  | p == 2 = 'X'
  | otherwise = '.'

-- | Reads a valid (x, y) position from the user.
-- Ensures the position is within the board and not already marked.
readXY :: [[Int]] -> Int -> IO (Int, Int)
readXY bd p = do
  putStrLn $ "Player " ++ [playerToChar p] ++ ", enter your move (x y):"
  input <- getLine
  let parsed = reads input :: [(Int, String)]
  if length parsed == 2
    then do
      let (x, rest) = head parsed
          (y, _) = head (reads rest :: [(Int, String)])
      if x >= 1 && y >= 1 && isEmpty x y bd
        then return (x, y)
        else retry
    else retry
  where
    retry = do
      putStrLn "Invalid input! Try again."
      readXY bd p

-- | Main function to play a game of Omok.
main :: IO ()
main = do
  let board = mkBoard 15
  playGame board mkPlayer mkOpponent
  where
    playGame bd p1 p2 = do
      putStrLn $ boardToStr playerToChar bd
      (x, y) <- readXY bd p1
      let bd' = mark x y bd p1
      if isWonBy bd' p1
        then putStrLn $ "Player " ++ [playerToChar p1] ++ " wins!"
        else if isFull bd'
          then putStrLn "It's a draw!"
          else playGame bd' p2 p1
