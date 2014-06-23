import System.IO(hFlush, stdout)

import Lisp

prompt :: String
prompt = "λ "

loop :: IO ()
loop = do putStr $ prompt
          hFlush stdout
          s <- getLine
          r <- return $ read' s
          e <- return $ r >>= (flip eval baseContext)
          putStrLn $ case e of
                          Right x -> show x
                          Left  y -> "Error: " ++ show y
          loop

main :: IO ()
main = loop
