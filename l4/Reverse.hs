module Main where
import System.Environment 
import System.Exit

main :: IO ()
main = do
    lst_args <- getArgs
    if (length lst_args /= 1)
    then do 
        putStr "usage: reverse filename \n" 
        exitFailure 
        else return()
    file_content <- readFile (head lst_args)
    let file_lines = lines file_content
    let reversed_lines = reverse file_lines
    reverse_help reversed_lines
    exitSuccess
    
    
reverse_help :: [String] -> IO ()
reverse_help [] = return ()
reverse_help (x:xs) = do 
    (putStr ((x) ++ "\n"))
    reverse_help xs


