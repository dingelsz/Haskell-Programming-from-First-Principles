import Data.List
import Data.Char

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)


data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Show, Eq)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price
  | Plane Airline Integer
  deriving (Eq, Show)


isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars cars = map isCar cars

getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer
getManu _ = undefined

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 2

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)


data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill , Mac
  , Windows
  ]
  
allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = nub [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]


newtype Name =
  Name String
  deriving Show

newtype Acres   =
  Acres Int
  deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


insert' :: Ord a => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b<a    = Node (insert' b left) a right
  | b>a    = Node left a (insert' b right)


map' :: (a -> b) -> BinaryTree a -> BinaryTree b
map' f Leaf = Leaf
map' f (Node left a right) = Node (map' f left) (f a) (map' f right)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = inorder l  ++ [a] ++ inorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf xs ys = foldr (\a b -> elem a ys && b) True xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords msg = foldr f [] (words msg)
  where f a@(af:as) b = (a, toUpper af : as) : b

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph ('.':(' ':(x:xs))) =
  '.' : ' ' : toUpper x : capitalizeParagraph xs
capitalizeParagraph (x:xs) = x : capitalizeParagraph xs

data Expr =
  Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = printExpr e1 ++ "+" ++ printExpr e2




  
