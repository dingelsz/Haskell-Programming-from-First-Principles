isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = (((snd t1), (snd t2)), ((fst t1), (fst t2)))

x = (+)
f' xs = w `x` 1
  where w = length xs

f'' (a, b) = a
