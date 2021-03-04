-- Impl of drop

mydrop n xs = if n <= 0 || null xs
                 then xs
            else mydrop (n - 1) (tail xs)


-- Impl of take

mytake n xs = if n <= 0 || null xs
                 then []
              else [head xs] ++ mytake (n-1) (tail xs)

lastButOne xs = if null (tail (tail xs))
                   then head xs
                else lastButOne (tail xs)


-- AckermanFunction

ackF k y = if k == 0
              then y + 1
              else if y == 0 && k > 0
              then ackF (k-1) 1
            else ackF (k-1) (ackF k (y-1))

---------------------------------------------

type CustomerID = Int
type ReviewBody = String

data BookInfo = Book Int String [String]
                deriving (Show)

data BookReview = BookReview BookInfo CustomerID ReviewBody
                  deriving (Show)

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                  | CashOnDelivery
                  | Invoice CustomerID
                    deriving (Show)

data Customer = Customer {
      customerID :: CustomerID,
      customerName :: String,
      customerAddress :: Address
   } deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]


data Cartesian2D = Cartesian2D (Double, Double)
                  deriving (Eq, Show)

data Polar2D = Polar2D Double Double
              deriving (Eq, Show)

data Shape = Circle Cartesian2D Double
            | Poly [Cartesian2D]
            deriving (Show)

-- data Maybe a = MyJust a
--              | MyNothing
--               deriving (Show)

-- Pattern Matching
add a 0 = a -- eq 1 for add
add a b = a + b -- eq 2 for add

sumList (x:xs) = x + sumList xs -- A list is of form (x0:(x1:(x2:[]))) in haskell
sumList [] = 0

third (a, b, c) = c -- returns last element of tuple

complicated (True, a, x:xs, 5) = (a, xs)

myBook = Book 1 "angel beats" ["jun mueda"]

bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

myTail (x:xs) = xs

yuno = Customer 1 "yuno gasai" ["mirrai nikki"]

-- creating List

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons a as) = [a] ++ toList (as)
toList Nil = []

-- binary tree

-- data Tree a = Node a (Tree a) (Tree a)
--             | Empty
--               deriving (Show)
--
-- data Tree a = Node a (Tree (Maybe a)) (Tree (Maybe a))

-- simpletree = node "pa" nothing just (node (node "right child" nothing nothing))

-- simpleTree = Node "P" "L" "R"


data Tree a = Node a (Maybe (Tree a)) (Maybe(Tree a)) deriving (Show)

simpleTree = Node "P" (Just (Node "L" Nothing Nothing)) Nothing

mySecond :: [a] -> Maybe a

mySecond [] = Nothing

mySecond (_:x:_) = Just x
mySecond _ = Nothing


-- Local variables
--
-- lend :: (a)

lend amount balance = let reserve = 100
                          newBalance = balance - amount
                       in if balance < reserve
                            then Nothing
                         else Just newBalance

foo = let a = 1
          b = 2
      in a + b


quux a = let a = "foo"
         in a ++ "eek!"
nodesAreSame (Node a _ _) (Node b _ _) | a == b = Just a

lend3 amount balance
      | amount <= 0            = Nothing
      | amount > reserve * 0.5 = Nothing
      | True                   = Just newBalance
      where reserve = 100
            newBalance = balance - amount

-- my impl of length

mylength (x:xs) = if null xs
                     then 1
                  else 1 + mylength xs

mylength [] = 0

-- returns mean of array

mean (x:xs) = (x + (mySum xs)) / (1 + (mylength xs))
  where mySum (x:xs) = if null xs
                          then x
                        else x + mySum xs
        mySum [] = 0


palindrome :: [x] -> [x]
palindrome (x:xs) = let ns = [x] ++ xs
                    in ns ++ rev ns
                   where rev (x:xs) = if null xs
                                         then [x]
                                      else rev xs ++ [x]
palindrome [] = []

-- mergesort :: [x] -> [x]
-- mergesort xs = if length xs > 1
--                   then take ((length xs)/ )

data Direction = MyLeft
               | MyRight
               | MyStraight
                 deriving(Show)

getDirection :: Cartesian2D  -> Cartesian2D -> Cartesian2D -> Direction

getDirection (Cartesian2D (x1, y1)) (Cartesian2D (x2, y2)) (Cartesian2D (x3, y3)) = if x1 > x3 && y1 > y3
                                                                                       then MyLeft
point1 = Cartesian2D (1.2, 1.2)
point2 = Cartesian2D (1.2, 4)
point3 = Cartesian2D (5,6)


