newtype Username =
  Username String
  deriving Show

newtype AccountNumber =
  AccountNumber Integer
  deriving Show

data User =
  UnregisteredUser
  | RegisteredUser Username AccountNumber
  deriving Show

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "Unregistered"
printUser (RegisteredUser (Username name) (AccountNumber num)) =
  putStrLn $ name ++ " " ++ show num

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

functionC x y =
  case (x > y) of
    True -> x
    False -> y

ifEvenAdd2 n =
  case (mod n 2) == 0 of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


tensDigit :: Integral a => a -> a
tensDigit x =
  let (xLast, d) = divMod x 10 in d

foldBool :: a -> a -> Bool -> a
foldBool x y f
  | f == True = x
  | f == False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


  

