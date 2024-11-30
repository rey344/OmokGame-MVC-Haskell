module Main where

import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Board

-- | Converts a player to its character representation.
-- Example: Player 1 -> 'O', Player 2 -> 'X', Empty -> '.'.
playerToChar :: Int -> Char
playerToChar p
  | p == 0 = '.'
  | p == 1 = 'O'
  | p == 2 = 'X'
  | otherwise = error "Invalid player"

-- | Reads a valid (x, y) position from the user.
-- Ensures the position is within the board and not already marked.
readXY :: [[Int]] -> Int -> IO (Int, Int)
readXY bd player = do
  putStrLn $ "Player " ++ [playerToChar player] ++ ", enter your move (x y):"
  input <- getLine
  let parsed = words input
  if length parsed /= 2
    then retry "Invalid input! Enter two numbers separated by a space."
    else do
      let (x:y:_) = map read parsed
      case checkBounds x y bd of
        Left err -> retry err
        Right _ ->
          case isEmpty x y bd of
            Left err -> retry err
            Right True -> return (x, y)
            Right False -> retry "Position is already marked."
  where
    retry err = do
      putStrLn err
      readXY bd player

-- | Main function to play a game of Omok.
main :: IO ()
main = do
  case mkBoard 15 of
    Left err -> putStrLn $ "Error creating board: " ++ err
    Right board -> playGame board mkPlayer mkOpponent
  where
    playGame :: [[Int]] -> Int -> Int -> IO ()
    playGame bd p1 p2 = do
      putStrLn $ boardToStr playerToChar bd
      (x, y) <- readXY bd p1
      case mark x y bd p1 of
        Left err -> do
          putStrLn $ "Error: " ++ err
          playGame bd p1 p2  -- Retry the same move
        Right bd' -> 
          if isWonBy bd' p1
            then putStrLn $ "Player " ++ [playerToChar p1] ++ " wins!"
            else if isDraw bd'
              then putStrLn "It's a draw!"
              else playGame bd' p2 p1
