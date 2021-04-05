
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)}

data ParseState = ParseState {
  string :: L>ByteString,
  offset :: Int64
  } deriving (Show)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m


parsep5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

parseP5 s =
  matchHeader (L8.pack "P5") s        >>?
  \s -> skipSpace ((), s)             >>?
  (getNat . snd)                      >>?
  skipSpace                           >>?
  \(width, s) -> getNat s             >>?
  skipSpace                           >>?
  \(height, s) -> getNat s            >>?
  \(maxGrey, s) -> getBytes 1 s       >>?
  (getBytes, (width * height) . snd)  >>?
  \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v


skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
 = case runParse parser (ParseState initState 0) of
     Left err -> Left Err
     Right (result, _) -> Right result
