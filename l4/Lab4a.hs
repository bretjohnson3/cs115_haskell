---- PART A ----

-- Need to import this to get the definition of toUpper:
import Data.Char

-- A.1

myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = putChar c >> myPutStrLn cs



-- A.2

greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")


-- A.3

-- Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Hello, "
  putStr name
  putStrLn "!"
  
greet2a :: IO ()
greet2a = putStr "Enter your name: " >> 
          getLine >>= \name -> putStr "Hello " >> 
          putStr name >>
          putStr "!"


greet2b :: IO ()
greet2b = (putStr "Enter your name:") >> 
          getLine >>= \name -> case name of
          _ -> putStr ("Hello, " ++ name ++ "! \n")

{- Slightly different desugaring but not really. Don't have to consider
bad pattern matches -}




-- A.4 



-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"




     
greet3way1 = 
    putStr "Enter your name: " >> getLine >>= \(n:ns) ->
        let name = toUpper n:ns in
        putStr "Hello, " >> putStr name >> putStrLn "!"     
     
        
greet3a :: IO ()
greet3a = putStr "Enter your name: " >> 
          getLine >>= \(n:ns) ->
          let name = toUpper n:ns in
          putStr "Hello, " >> putStr name >> putStrLn "!"



greet3b :: IO ()
greet3b = putStr "Enter your name: " >> 
          getLine >>= \name -> 
              case name of 
              (n:ns) -> 
                  let name' = toUpper n:ns in putStr "Hello, " 
                      >> putStr name' >> putStrLn "!"
              _ -> fail "Pattern match failure!"

{- Now the desugarig handles failed pattern matches in a more
predictable way -}
              
