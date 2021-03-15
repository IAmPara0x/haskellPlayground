module Prettify where

import Prelude hiding ((<>))

data Doc = Empty | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)

-- For Creating Doc types --
empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

---------------------------

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d : ds) =
      case d of
        Empty -> transform ds
        Char c -> c : transform ds
        Text s -> s ++ transform ds
        Line -> '\n' : transform ds
        a `Concat` b -> transform (a : b : ds)
        _ `Union` b -> transform (b : ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (d : ds) =
      case d of
        Empty -> best col ds
        Char c -> c : best (col + 1) ds
        Text s -> s ++ best (col + length s) ds
        Line -> '\n' : best 0 ds
        a `Concat` b -> best col (a : b : ds)
        a `Union` b ->
          nicest
            col
            (best col (a : ds))
            (best col (b : ds))
    best _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n' : _) = True
w `fits` (c : cs) = (w - 1) `fits` cs

nest indSize x = indPrint indSize 0 [x]
  where
    indPrint indSize indLvl (d : ds) =
      case d of
        Empty -> indPrint indSize indLvl ds
        Line -> case ds of 
          (Char d':_) -> if d' `notElem` closeParams
                             then '\n' : replicate (indLvl * indSize) ' ' ++ indPrint indSize indLvl ds
                           else '\n' : replicate ((indLvl - 1) * indSize) ' ' ++ indPrint indSize indLvl ds
          _ -> '\n' : replicate (indLvl * indSize) ' ' ++ indPrint indSize indLvl ds
        Text s -> s ++ indPrint indSize indLvl ds
        Char c ->
          let newIndLvl = changeIndLvl c indLvl
           in if newIndLvl > indLvl || newIndLvl < indLvl
                then c : '\n' : replicate (newIndLvl * indSize) ' ' ++ indPrint indSize newIndLvl ds
                else c : indPrint indSize newIndLvl ds
        a `Concat` b -> indPrint indSize indLvl (a : b : ds)
        _ `Union` b -> indPrint indSize indLvl (b : ds)
    indPrint _ _ _ = ""

    changeIndLvl c indLvl
      | c `elem` openParams = indLvl + 1
      | c `elem` closeParams = indLvl - 1
      | otherwise = indLvl
    openParams = "({["
    closeParams = ")}]"
