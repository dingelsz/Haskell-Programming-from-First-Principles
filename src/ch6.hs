import Data.List

data Trivial =
  Trivial'

instance Eq Trivial where
  (==) Trivial' Trivial' = True

data Weekday =
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Ord, Show)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = True

data Date =
  Date Weekday Int deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday'
    && dayOfMonth == dayOfMonth'


data TisAnInteger =
       TisAn Integer deriving Show

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn m) = n == m


data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x1 y1) (Two x2 y2) = x1 == x2 && y1 == y2

data StringOrInt = TisAnInt Int
                 | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt n) (TisAnInt m) = n == m
  (==) (TisAString s) (TisAString t) = s == t
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair e1 e2) (Pair e3 e4) = e1 == e3 && e2 == e4

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2

data Which a =
  ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False


data Mood = Blah
               | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah else x


type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

f :: Fractional a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

jung :: [Int] -> Int
jung xs = head (sort xs)


young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = (f x)


