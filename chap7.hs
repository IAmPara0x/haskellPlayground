-- import System.IO
-- import System.Directory(getTemporaryDirectory, removeFile)
-- import System.IO.Error(catch)
-- import Control.Exception(finally)
-- import Data.Char(toUpper)

{-
  main = do
    putStrLn "Greetings! name?"
    inputStr <- getLine
    putStrLn $ "Moshi moshi " ++ inputStr
-}

{- putStrLn :: String -> IO()
  putStrLn have IO() action type. An IO action only gets executed when it is called within another IO action.
  IO actions can have side effects.
-}

name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

{-
  main :: IO()
  main = do
    putStrLn "Greetings. What is your name?"
    inpStr <- getLine
    let outStr = name2reply inpStr
    putStrLn outStr
-}

{-
-- Imperative way to read and write file.
main :: IO()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO()
mainloop inh outh =
  do ineof <- hIsEOF inh
     if ineof
        then return ()
      else do
        inpStr <- hGetLine inh
        hPutStrLn outh (map toUpper inpStr)
        mainloop inh outh
-}

-- Haskell way to read and write file

{-
main :: IO()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  inpStr <- hGetContents inh
  let result = processData inpStr
  hPutStr outh result
  hClose inh
  hClose outh

processData :: String -> String
processData = map toUpper
-}

{-
quick sort

f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where
    ys = [a | a<-xs, a <= x]
    zs = [b | b<-xs, b > x]
-}
--- Prime number check ---
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isprime :: Int -> Bool
isprime n = factors n == [1,n]
-----------------------------
-- sequencing operators (>>) & (>>=)

main :: IO()
main = putStrLn "yuno" >> putStrLn "ichigo" >> putStrLn "touka"


