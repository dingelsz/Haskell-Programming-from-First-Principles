{-
  This is a quick implementation of a type that conforms to a typeclass using
  TDD. The workflow is as follows:

  0. Setup the file (imports and main)

  1. Define the Datatype
  2. Testing
   a. Write properties
   b. Write arbitrary instance
   c. Write test entry point
  3. Implement type class
-}

import Test.QuickCheck

data Bol = T | F deriving (Show, Eq)

instance Semigroup Bol where
  x <> y
    | x == T = T
    | y == T = T
    | otherwise = F

-- Tests -----------------------------------------------------------------------
instance Arbitrary Bol where
  arbitrary = oneof [return T, return F]

prop_BolAssoc :: Bol -> Bol -> Bol -> Bool
prop_BolAssoc x y z =
  ((x <> y) <> z) == (x <> (y <> z))

testBol :: IO ()
testBol = do
  quickCheck prop_BolAssoc


--------------------------------------------------------------------------------
-- | Identity 
newtype Id a =
  Id a
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Id a) where
  (Id x) <> (Id y) = Id (x <> y)

-- Tests -----------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Id a) where
  arbitrary = do
    a <- arbitrary
    return (Id a)

prop_IdAssoc :: (Semigroup a, Eq a) => Id a -> Id a -> Id a -> Bool
prop_IdAssoc x y z =
  ((x <> y) <> z) == (x <> (y <> z))

testId :: IO ()
testId = do
  quickCheck (prop_IdAssoc :: Id String -> Id String -> Id String -> Bool)


--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

-- Tests -----------------------------------------------------------------------
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

prop_TwoAssoc :: (Semigroup a, Semigroup b, Eq a, Eq b) =>
                 Two a b -> Two a b -> Two a b -> Bool
prop_TwoAssoc x y z =
  ((x <> y) <> z) == (x <> (y <> z))

testTwo :: IO ()
testTwo = do
  quickCheck (prop_TwoAssoc :: (Two String String) -> (Two String String) -> (Two String String) -> Bool)

--------------------------------------------------------------------------------
data Three a b c =
  Three a b c
  deriving (Show, Eq)

instance (Semigroup a,Semigroup  b, Semigroup c) => Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

-- Tests -----------------------------------------------------------------------
instance (Arbitrary a, Arbitrary b, Arbitrary c)  =>Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)
    

prop_ThreeAssoc :: (Semigroup a, Semigroup b, Semigroup c, Eq a, Eq b, Eq c) =>
                   Three a b c -> Three a b c -> Three a b c -> Bool
prop_ThreeAssoc x y z =
  ((x <> y) <> z) == (x <> (y <> z))

testThree :: IO ()
testThree = do
  quickCheck (prop_ThreeAssoc :: Three String String String -> Three String String String -> Three String String String -> Bool)
--------------------------------------------------------------------------------

data Four a b c d =
  Four a b c d
  deriving (Show, Eq)

instance (Semigroup a, Semigroup  b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x1 y1 z1 o1) <> (Four x2 y2 z2 o2) = Four (x1 <> x2) (y1 <> y2) (z1 <> z2) (o1 <> o2)

-- Tests -----------------------------------------------------------------------
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)  =>Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)
    

prop_FourAssoc :: (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Eq a, Eq b, Eq c, Eq d) =>
                   Four a b c d -> Four a b c d -> Four a b c d -> Bool
prop_FourAssoc x y z =
  ((x <> y) <> z) == (x <> (y <> z))

testFour :: IO ()
testFour = do
  quickCheck (prop_FourAssoc :: Four String String String String -> Four String String String String -> Four String String String String -> Bool)
