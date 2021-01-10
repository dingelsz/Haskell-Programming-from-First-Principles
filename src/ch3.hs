module Ch3 where

excite :: String -> String
excite msg = msg ++ "!"

get5 :: [a] -> a
get5 msg = msg !! 4

thirdLetter :: String -> Char
thirdLetter msg = msg !! 3

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String
rvrs = drop 9 msg ++ take 4 (drop 5 msg) ++ take 5 msg
  where msg = "Curry is awesome"
