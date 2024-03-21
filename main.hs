class Palindromic a where
  isPalindrome :: a -> Bool

instance Palindromic [Char] where
  isPalindrome []     = True
  isPalindrome [_]    = True
  isPalindrome (x:xs) = last xs == x && isPalindrome (init' xs)
    where init' []     = []
          init' [x]    = []
          init' (x:xs) = x : init' xs

instance Palindromic Integer where
  isPalindrome n = isPalindrome (show n)

data NestedList a = Elem a | List [NestedList a]

flatten' :: NestedList a -> [a]
flatten' (Elem x)      = [x]
flatten' (List [])     = []
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs)

compress :: (Eq a) => [a] -> [a]
compress xs = aux xs []
  where aux [ ] acc = acc
        aux [x] acc = (acc ++ [x])
        aux (x:xs@(h:_)) acc
          | x == h       = aux xs acc
          | otherwise    = aux xs (acc ++ [x])

pack :: String -> [[Char]]
pack [] = []
pack (x:xs) = (x:ys) : pack zs
  where
    (ys, zs) = span (==x) xs
