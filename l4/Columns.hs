module Main where
import Prelude
import System.Environment 
import System.Exit
import Data.Char
import System.IO
import Control.Monad()



main :: IO()
main = do
    lst_args <- getArgs
    if (checkArgs lst_args) || (length lst_args < 2) then do
        putStr "usage: columns n1 n2 .. filename (n1 n2 .. must be a positive integers) \n"
        exitFailure
        else return ()
    
    
    file_contents <- hGetContents stdin
    let cols = getCols lst_args
    
    if ((last lst_args) == "-") then do
        let term_lines = lines file_contents
        toTerminal term_lines cols
    else do
        contents <- readFile (last lst_args)
        let file_lines = lines contents
        toTerminal file_lines cols



isPosInt :: String -> Bool
isPosInt [] = False 
isPosInt [x] = isDigit x
isPosInt (x : xs) = isDigit x && isPosInt xs


checkArgs :: [String] -> Bool
checkArgs [] = True
checkArgs (x:xs) = ((isPosInt x) && (checkArgs xs))


getCols :: [String] -> [Int]
getCols [] = []
getCols (_:xs) | xs == [] = []
getCols (c:cs) = [read c] ++ (getCols cs)



filterCols _ [] = []
filterCols l (x:xs) | x <= (length l) = [l!!(x - 1)] ++ (filterCols l xs)
filterCols l (_:xs) = (filterCols l xs)



toTerminal :: [String] -> [Int] -> IO ()
toTerminal [] _ = return ()
toTerminal (c:cs) num = do
    let row = filterCols (words c) num
    putStr ((unwords row) ++ "\n")
    toTerminal cs num
