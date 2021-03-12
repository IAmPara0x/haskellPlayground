
import System.Environment (getArgs)
import Data.Char (toUpper, digitToInt)
import Data.List (tails)


charToString :: Char -> String
charToString c = [c]

interactWith :: (Char -> Char) -> FilePath -> FilePath -> IO()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile ((map function input))

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = toUpper

splitLines :: String -> [String]

splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
   in pre : case suf of
              ('\r':'\n':rest) -> splitLines rest
              ('\r':rest) -> splitLines rest
              _ -> []


isLineTerminator c = c == '\r' || c == '\n'

--infix functions && data types

a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

-- Lists are algebraic types They doesn't store there own length -> len [a] is O(n)
badEx xs = if length xs > 0 -- complexity is O(n)
                   then head xs
                   else 'Z'

-- Safe functions

safeHead :: [x] -> Maybe x
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail :: [x] -> Maybe [x]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeLast :: [x] -> Maybe x
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [x] -> Maybe [x]
safeInit [] = Nothing
safeInit xs = Just (init xs)

---

splitWith :: (x -> Bool) -> [x] -> [[x]]
splitWith _ [] = []
splitWith function xs =
  let (a,b) = break function xs
   in case (a) of
        (y:ys) -> a : splitWith function (tail b)
        [] -> splitWith function (tail b)

---

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc*10 + digitToInt x
                   in loop acc' xs

--- partial Functions

add a b = a + b


-- as patterns

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

mult :: Int -> Int
mult x = x * 2

--- IMPO:
-- Functions signatures are of this order
-- eg: func a b c = a (b c)
-- func :: (input type for func a -> output type for func a) -> (input type for func b -> output type for func b)
-- -> input type for c -> output type for c

-- compose :: ([[Char]] -> [[Char]]) -> ([Char] -> [[Char]]) -> [Char] -> [[Char]]
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes4 = compose init tails
suffixes5 = init . tails -- (.) is same as compose

capCount = length . filter (isUpper . head) . words
