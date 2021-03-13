data Color = Red | Green | Blue
           deriving (Read, Show, Eq, Ord)

-- Creating (==) operator
class BasicEq a where
  isEqual :: a -> a -> Bool
  isEqual x y = not (isNotEqual x y)

  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)

instance BasicEq Color where
  isEqual Red Red = True
  isEqual Green Green = True
  isEqual Blue Blue = True
  isEqual _ _ = False

{-

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Read Color where
  readsPrec _ value =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
      where tryParse [] = []
            tryParse ((attempt, result):xs) =
              if (take (length attempt) value) == attempt
                 then [(result, drop (length attempt) value)]
              else tryParse xs

-}

--- derivation stuff
{-
data CannotShow = CannotShow
data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show) -- will not compile coz CannotShow is not an instance of Show
-}

data Ok = Ok
instance Show Ok where
  show _ = "Ok"

main :: IO()
main = do
        putStrLn "Please enter a Double:"
        inpStr <- getLine
        let inpDouble = (read inpStr)::Double
         in putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))



