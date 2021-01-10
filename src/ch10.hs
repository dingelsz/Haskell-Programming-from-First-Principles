import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate time):xs) = time : filterDbDate xs
filterDbDate (x:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber n):xs) = n : filterDbNumber xs
filterDbNumber (x:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs
  |  dates == [] = (UTCTime
                    (fromGregorian 0 0 0)
                    (secondsToDiffTime 0))
  |  otherwise   = foldr max firstDate dates
  where dates = filterDbDate xs
        firstDate = dates !! 0

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNumber xs)

fibs = takeWhile (\x -> x < 100) (1 : scanl (+) 1 fibs)
fibsN x = fibs !! x

fact = scanl (*) 1 [1..]

stops  = "pbtdkg"
vowels = "aeiou"

combine :: String -> String -> [(Char, Char, Char)]
combine stops vowels = [(s1, v, s2) | s1 <- stops, s2 <- stops, v <- vowels]

seekritFunc :: Fractional a => String -> a
seekritFunc x = s / l
  where l = fromIntegral . length . words $ x
        s = fromIntegral . sum . map length . words $ x

myOr :: [Bool] -> Bool
myOr xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr (map f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = any ((==) x) xs

myReverse :: [a] -> [a]
myReverse xs = foldl (\b a -> a : b) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a b -> f a : b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\a b -> if f a then a : b else b) [] xs

squish :: [[a]] -> [a]
squish xs = foldr (++) [] xs
