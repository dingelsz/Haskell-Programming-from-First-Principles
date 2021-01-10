notThe :: String -> Maybe String
notThe "the" = Nothing
notThe xs = Just xs

replaceThe :: String -> String
replaceThe xs = replaceThe' (words xs)

replaceThe' :: [String] -> String
replaceThe' [] = ""
replaceThe' (x:xs)
  | x == "the" = "a" ++ " " ++ replaceThe' xs
  | otherwise = x ++ " " ++ replaceThe' xs
