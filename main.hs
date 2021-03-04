
import System.Environment (getArgs)
import Data.List
import Data.Char (digitToInt, toUpper, ord)
import Data.Bits (shiftL, (.$.), (.|.))


interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)


main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = id


splitLines [] = []

splitLines cs =
  let (pre, suf) = break isLineTerminator cs
   in pre : case suf of
              ('\r':'\n':rest) -> splitLines rest
              ('\r':rest) -> splitLines rest
              ('\n':rest) -> splitLines rest
              _ -> []

isLineTerminator c = c == '\r' || c == '\n'

-- Lists doesn't store it's length like in other imperative languages to length xs is always O(n)

isEmptyList xs = if length xs > 0  -- O(n) complexity Bad impl
                       then True
                  else False

-- Good Impl constant time
isEmptyList2 xs = if not (null xs)
                     then True
                  else False

isEmptyList3 (x:_) = True
isEmptyList3 [] = False

--------------------------

safeHead :: [a] -> Maybe a

safeHead (x:xs) = Just x
safeHead [] = Nothing

-------------------------

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x    -- acc' is acc prime a way to write vars in haskell means that 
                  in loop acc' xs                       -- acc' is somehow related to acc

square :: [Double] -> [Double]

square (x:xs) = x^2 : square xs
square [] = []

strUpper :: String -> String

strUpper (x:xs) = toUpper x : strUpper xs
strUpper [] = []


--- Bad Impl as it's not tail recursive
mySum (x:xs) = if null xs
                  then x
               else x + mySum xs
mySum [] = 0

---

--- Tail recursive impl of mySum
mySum2 xs = helper 0 xs
  where helper acc (x:xs) = helper (acc + x) xs
        helper acc _      = acc
---

--- Adler-32 CheckSum algorithm

base = 65521

adler32 xs = helper 1 0 xs
  where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                b' = (a' + b) `mod` base
                            in helper a' b; xs
        helper a b _      = (b `shiftL` 16) .|. a




