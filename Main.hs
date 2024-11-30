module Main where

import System.IO (hSetBuffering, hSetEcho, BufferMode(..), stdin)
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

-- | Reads a valid (x, y) position from the user or allows them to quit the game.
-- Handles backspace gracefully.
readXY :: [[Int]] -> Int -> IO (Either String (Int, Int))
readXY bd player = do
  putStrLn $ "Player " ++ [playerToChar player] ++ ", enter your move (x y) or 'q' to quit:"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getInput ""  -- Read user input with backspace handling
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  if input == "q"
    then return $ Left "Game quit by user."
    else do
      let parsed = words input
      if length parsed /= 2 || not (all isNumeric parsed)
        then retry "Invalid input! Enter two numbers separated by a space or 'q' to quit."
        else do
          let (x:y:_) = map read parsed
          case checkBounds x y bd of
            Left err -> retry err
            Right _ ->
              case isEmpty x y bd of
                Left err -> retry err
                Right True -> return $ Right (x, y)
                Right False -> retry "Position is already marked."
  where
    -- Helper function to handle user input character by character.
    getInput :: String -> IO String
    getInput current = do
      c <- getChar
      case c of
        '\DEL' -> if null current
                    then getInput ""  -- Ignore backspace if there's nothing to delete
                    else do
                      putStr "\b \b"  -- Erase last character from the terminal
                      getInput (init current)
        '\n' -> do
          putStrLn ""  -- Move to the next line after Enter
          return current
        _ -> do
          putChar c  -- Echo the character back to the terminal
          getInput (current ++ [c])

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
main = gameLoop

-- | Main game loop to allow replay or exit.
gameLoop :: IO ()
gameLoop = do
  putStrLn "Welcome to Omok!"
  putStrLn "Choose a game mode:"
  putStrLn "1. Player vs Player (PvP)"
  putStrLn "2. Player vs Computer (PvC)"
  putStrLn "q. Quit"
  mode <- getLine
  case mode of
    "1" -> startGame False >> promptReplay
    "2" -> startGame True >> promptReplay
    "q" -> putStrLn "Goodbye!"
    _   -> do
      putStrLn "Invalid option. Please choose 1, 2, or q."
      gameLoop

-- | Start a new game in the selected mode.
startGame :: Bool -> IO ()
startGame isPvC = do
  case mkBoard 15 of
    Left err -> putStrLn $ "Error creating board: " ++ err
    Right board -> playGame board mkPlayer mkOpponent isPvC

-- | Handles the replay prompt after a game ends or quits.
promptReplay :: IO ()
promptReplay = do
  putStrLn "Do you want to play again? (y/n)"
  choice <- getLine
  case choice of
    "y" -> gameLoop
    "n" -> putStrLn "Thank you for playing Omok! Goodbye!"
    _   -> do
      putStrLn "Invalid input. Please enter 'y' or 'n'."
      promptReplay

-- | Core game logic, handles turns and win/draw checks.
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
      result <- readXY bd p1
      case result of
        Left quitMessage -> putStrLn quitMessage
        Right (x, y) -> case mark x y bd p1 of
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
