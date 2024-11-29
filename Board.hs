module Board where

-- | Checks if the given (x, y) indices are within bounds of the board.
checkBounds :: Int -> Int -> [[Int]] -> Either String ()
checkBounds x y bd
  | x <= 0 || x > length bd || y <= 0 || y > length bd = Left "Position out of bounds"
  | otherwise = Right ()

-- | Safely accesses the element at (x, y) on the board.
-- Assumes 1-based indices and valid bounds.
getElement :: Int -> Int -> [[Int]] -> Int
getElement x y bd = (bd !! (y - 1)) !! (x - 1)

-- | Creates an empty nxn board if n is positive; otherwise, returns an error.
mkBoard :: Int -> Either String [[Int]]
mkBoard n
  | n <= 0 = Left "Board size must be a positive integer"
  | otherwise = Right (replicate n (replicate n 0))

-- | Returns the first player.
-- This function encapsulates player 1's identity.
mkPlayer :: Int
mkPlayer = 1

-- | Returns the second player (opponent).
-- This function encapsulates player 2's identity.
mkOpponent :: Int
mkOpponent = 2

-- | Returns the size of the board (nxn) or an error message.
size :: [[Int]] -> Either String Int
size bd
  | null bd = Left "Board cannot be empty"  -- Error: empty board
  | not (isSquare bd) = Left "Board must be square (all rows must have the same length)"  -- Error: non-square board
  | otherwise = Right (length bd)  -- Valid board

-- | Checks if a board is square (all rows have the same length).
isSquare :: [[Int]] -> Bool
isSquare bd = all (\row -> length row == length bd) bd

-- | Returns a specific row of the board.
-- The input y is 1-based; the function adjusts for 0-based indexing.
row :: Int -> [[Int]] -> Either String [Int]
row y bd 
    | y <= 0 || y > length bd = Left "Row index out of bounds"
    | otherwise = Right (bd !! (y - 1))  -- Adjust for 0-based indexing

-- | Returns a specific column of the board.
-- The input x is 1-based; the function adjusts for 0-based indexing.
column :: Int -> [[Int]] -> Either String [Int]
column x bd = 
  case checkBounds x 1 bd of
    Left err -> Left err
    Right _ -> Right [row !! (x - 1) | row <- bd]

-- | Marks a position (x, y) for a player p on the board.
-- Assumes 1-based indices and checks bounds and emptiness.
mark :: Int -> Int -> [[Int]] -> Int -> Either String [[Int]]
mark x y bd p =
  case checkBounds x y bd of
    Left err -> Left err
    Right _ ->
      if getElement x y bd /= 0
        then Left "Position already marked"
        else Right (replace y bd (replace x (bd !! (y - 1)) p))
  where
    replace :: Int -> [a] -> a -> [a]
    replace n xs val = take (n - 1) xs ++ [val] ++ drop n xs

-- | Checks if a specific position (x, y) is empty.
-- Assumes 1-based indices.
isEmpty :: Int -> Int -> [[Int]] -> Either String Bool
isEmpty x y bd =
  case checkBounds x y bd of
    Left err -> Left err
    Right _ -> Right (getElement x y bd == 0)

-- | Checks if a specific position (x, y) is marked.
-- Assumes 1-based indices
isMarked :: Int -> Int -> [[Int]] -> Either String Bool
isMarked x y bd =
  case checkBounds x y bd of
    Left err -> Left err
    Right _ -> Right (getElement x y bd /= 0)

-- | Checks if a specific position (x, y) is marked by a player p.
-- Assumes 1-based indices
isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Either String Bool
isMarkedBy x y bd p =
  case checkBounds x y bd of
    Left err -> Left err
    Right _ -> Right (getElement x y bd == p)

marker :: Int -> Int -> [[Int]] -> Either String Int
marker x y bd =
  case checkBounds x y bd of
    Left err -> Left err
    Right _ -> Right (getElement x y bd)


-- | Checks if the board is completely filled.
isFull :: [[Int]] -> Bool
isFull bd = undefined

-- | Checks if a player p has won the game.
isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p = undefined

-- | Checks if the game ended in a draw.
isDraw :: [[Int]] -> Bool
isDraw bd = undefined

-- | Checks if the game is over.
-- The game is over if it is either won or drawn.
isGameOver :: [[Int]] -> Bool
isGameOver bd = undefined

-- | Converts the board to a string representation for display.
-- playerToChar is a function that maps players to characters ('O', 'X', '.').
boardToStr :: (Int -> Char) -> [[Int]] -> String
boardToStr playerToChar bd = undefined
