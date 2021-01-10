safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (x:xs) = Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool cur end
  | cur > end = []
  | cur == end = [end]
  | cur < end = cur : (eftBool (succ cur) end)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = undefined

eftInt :: (Eq a, Enum a, Ord a) => a -> a -> [a]
eftInt cur end
  | cur > end = []
  | cur == end = [end]
  | cur < end = cur : (eftInt (succ cur) end)


eftChar :: Char -> Char -> [Char]
eftChar = undefined

splitByDelimiter :: String -> Char -> [String]
splitByDelimiter [] _ = []
splitByDelimiter str del = word : (splitByDelimiter rest del)
  where isntDel c = c /= del
        word = takeWhile isntDel str
        rest = if word == str
               then []
               else tail (dropWhile isntDel str)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 xs ys = myZipWith (\x y -> (x, y)) xs ys


