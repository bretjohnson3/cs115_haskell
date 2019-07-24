--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
  where
    
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter _ (row, _) | row > 9 = return True
    iter b (row, col) | col > 9 = iter b (row + 1, 1)
    iter board loc@(row, col) = do
       v <- readArray board loc
       if v == 0 then do
           vals <- getOKValues board loc
           iter' board loc vals
       else 
           iter board (row, col + 1)
        



    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' _ _ [] = return False
    iter' board loc (n:ns) = do
        writeArray board loc n
        final <- iter board loc
        if final then return True
        else do
            writeArray board loc 0
            iter' board loc ns
    



    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues board (row, col) = do
        box  <- getBox board (row, col)
        row' <- getRow board row
        col' <- getCol board col
        let bad = union (union box row') col'
        return ([1 .. 9] \\ bad)
    
    
    -- Return the ith row in a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow board row = mapM (\col -> readArray board (row, col)) [ 1.. 9]
    
    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol board col = mapM (\row -> readArray board (row, col)) [1 .. 9]

 
    -- helper function for getBox
    getBoxVals :: Sudoku -> (Int, Int) -> IO [Int]
    getBoxVals board (r, c) = do
        b1 <- readArray board (r, c)
        b2 <- (readArray board (r, c + 1))
        b3 <- (readArray board (r, c + 2))
        b4 <- (readArray board (r + 1, c))
        b5 <- (readArray board (r + 1, c + 1))
        b6 <- (readArray board (r + 1, c + 2))
        b7 <- (readArray board (r + 2, c))
        b8 <- (readArray board (r + 2, c + 1))
        b9 <- (readArray board (r + 2, c + 2))
        let lst = (b1:b2:b3:b4:b5:b6:b7:b8:[b9])
        return (filter (\x-> x /= 0) lst) 
    
    
    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox board (row, col) = do
        let box_row = (((row - 1) `div` 3) * 3) + 1
        let box_col = (((col - 1) `div` 3) * 3) + 1
        getBoxVals board (box_row, box_col)
        
        
    

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

