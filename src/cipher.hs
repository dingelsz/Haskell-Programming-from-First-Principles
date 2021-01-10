module Cipher where

import Data.Char

cesear :: String -> Int -> String
cesear "" _ = ""
cesear (x:xs) n = cesearChar : cesear xs n
  where atoi = ord x
        offset = rem (atoi + n - (ord 'a')) 26
        cesearChar = chr ((ord 'a') + offset)

uncesear :: String -> Int -> String
uncesear str n = cesear str (-n)

boundChr :: Int -> Char
boundChr n = chr ((ord 'a') + (mod (n - ord 'a') 26))

vigenere :: String -> String -> String
vigenere "" _ = ""
vigenere (' ':xs) ks = ' ' : vigenere xs ks
vigenere (x:xs) (k:ks) = secret : vigenere xs (ks ++ [k])
  where secret = boundChr (ord x + offset)
        offset = ord k - ord 'a'

                 


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr . map f $ xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem tar (x:xs)
  | tar == x = True
  | otherwise = myElem tar xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs  ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish . map f $ xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = if f x maxRest == GT then x else maxRest
  where maxRest = myMaximumBy f xs

