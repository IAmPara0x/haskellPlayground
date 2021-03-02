-- Impl of drop
{-
mydrop n xs = if n <= 0 || null xs
                 then xs
            else mydrop (n - 1) (tail xs)
-}

-- Impl of take
{-
mytake n xs = if n <= 0 || null xs
                 then []
              else [head xs] ++ mytake (n-1) (tail xs)

lastButOne xs = if null (tail (tail xs))
                   then head xs
                else lastButOne (tail xs)
-}

-- AckermanFunction
{-
ackF k y = if k == 0
              then y + 1
              else if y == 0 && k > 0
              then ackF (k-1) 1
            else ackF (k-1) (ackF k (y-1))
-}

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

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]


data Cartesian2D = Cartesian2D (Double, Double)
                  deriving (Eq, Show)

data Polar2D = Polar2D Double Double
              deriving (Eq, Show)

data Shape = Circle Cartesian2D Double
            | Poly [Cartesian2D]
            deriving (Show)

-- Pattern Matching
add a 0 = a -- eq 1 for add
add a b = a + b -- eq 2 for add

sumList (x:xs) = x + sumList xs -- A list is of form (x0:(x1:(x2:[]))) in haskell























