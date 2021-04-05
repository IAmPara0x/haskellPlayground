module SimpleFinder where

import Prelude hiding (traverse)
import Control.Monad
import System.Directory hiding (getFileSize)
import Data.Time.Clock (UTCTime(..))
import System.FilePath
import Control.Exception
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath
  -> Permissions
  -> Maybe Integer
  -> UTCTime
  -> Bool

type InfoP a = FilePath
  -> Permissions
  -> Maybe Integer
  -> UTCTime
  -> a

data Info = Info {
  infoPath :: FilePath,
  infoPerms :: Maybe Permissions,
  infoSize :: Maybe Integer,
  infoMidTime :: Maybe UTCTime
  } deriving (Show)

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)


getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)


pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` constP k w x y z

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)

orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

-- myTest2 = liftPath takeExtension ==? ".cpp" &&? sizeP >>? 131072

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents ) $ \info -> do
    if isDirectory info && infoPath info /= path
       then traverse order (infoPath info)
       else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handler (Just `liftM` act)
  where
    handler :: SomeException -> IO (Maybe a)
    handler _ = return Nothing

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
    where
      fold seed subpath = getUsefulContents subpath >>= walk seed
      walk seed (name:names) = do
        let path' = path </> name
        info <- getInfo path'
        case iter seed info of
          done@(Done _) -> return done
          Skip seed'    -> walk seed' names
          Continue seed'
            | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
            | otherwise       -> walk seed' names
      walk seed _ = return (Continue seed)
