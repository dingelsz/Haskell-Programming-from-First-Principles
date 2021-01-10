import Control.Applicative
import Data.List (elemIndex)

f x =
  lookup x [ (3, "hello")
           , (4, "julie")
           , (5, "kbai")]
g y =
  lookup y [ (7, "sup?")
           , (8, "chris")
           , (9, "aloha")]
h z =
  lookup z [(2, 3), (5, 6), (7, 8)]
  
m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
-- The secret with this one is to lift +3 into Maybe
-- >>> added 

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
-- >>> y

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
-- >>> z
-- Just 5

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
-- >>> tupled
-- Just (6,5)


x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]
-- >>> x
-- Just 2

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]
-- >>> y'
-- Just 3

max' :: Int -> Int -> Int
max' = max
-- >>> max'

maxed :: Maybe Int
-- (Int -> Int -> Int) (Maybe Int) (Maybe Int)
maxed = max' <$> x <*> y'
-- >>> maxed
-- Just 3

xs = [1, 2, 3]
ys = [4, 5, 6]
-- >>> xs
-- [1,2,3]
-- >>> ys
-- [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys
-- >>> x''
-- Just 6

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys
-- >>> y''
-- Just 5

summed :: Maybe Integer
summed = sum <$> ( (,) <$> x'' <*> y'' )
-- >>> summed


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a
  
instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant b

type M = Maybe

(<*>) :: M (a -> b) -> M a -> M b

pure :: a -> M a







