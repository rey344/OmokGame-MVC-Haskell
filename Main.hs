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
  if length parsed /= 2 || not (all isNumeric parsed)
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
    isNumeric str = all (`elem` "0123456789") str
    retry err = do
      putStrLn err
      readXY bd player

-- | Generates a random valid move for the computer.
getRandomMove :: [[Int]] -> IO (Int, Int)
getRandomMove bd = do
  x <- randomRIO (1, length bd)
  y <- randomRIO (1, length bd)
  case isEmpty x y bd of
    Right True -> return (x, y)
    _          -> getRandomMove bd  -- Retry if the move is invalid

-- | Main function to play a game of Omok.
main :: IO ()
main = do
  putStrLn "Welcome to Omok!"
  putStrLn "Choose a game mode:"
  putStrLn "1. Player vs Player (PvP)"
  putStrLn "2. Player vs Computer (PvC)"
  mode <- getLine
  case mode of
    "1" -> startGame False  -- PvP mode
    "2" -> startGame True   -- PvC mode
    _   -> do
      putStrLn "Invalid option. Please choose 1 or 2."
      main

startGame :: Bool -> IO ()
startGame isPvC = do
  case mkBoard 15 of
    Left err -> do
      putStrLn $ "Error creating board: " ++ err
      return ()  -- Exit gracefully
    Right board -> playGame board mkPlayer mkOpponent isPvC
  where
    playGame :: [[Int]] -> Int -> Int -> Bool -> IO ()
    playGame bd p1 p2 isPvC = do
      putStrLn $ boardToStr playerToChar bd
      if isPvC && p1 == mkOpponent
        then do
          putStrLn "Computer's turn..."
          (x, y) <- getRandomMove bd
          case mark x y bd p1 of
            Left err -> error ("Unexpected error: " ++ err)  -- This shouldn't happen for random valid moves
            Right bd' -> 
              if isWonBy bd' p1
                then putStrLn $ "Player " ++ show p1 ++ " (" ++ [playerToChar p1] ++ ") wins!"
                else if isDraw bd'
                  then putStrLn "It's a draw!"
                  else playGame bd' p2 p1 isPvC
        else do
          (x, y) <- readXY bd p1
          case mark x y bd p1 of
            Left err -> do
              putStrLn $ "Error: " ++ err
              putStrLn "Here's the current state of the board:"
              putStrLn $ boardToStr playerToChar bd  -- Display the board again
              playGame bd p1 p2 isPvC  -- Retry the same move
            Right bd' -> 
              if isWonBy bd' p1
                then putStrLn $ "Player " ++ show p1 ++ " (" ++ [playerToChar p1] ++ ") wins!"
                else if isDraw bd'
                  then putStrLn "It's a draw!"
                  else playGame bd' p2 p1 isPvC
