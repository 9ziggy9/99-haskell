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

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first)  : pack rest
  where consume [] = ([], [])
        consume (y:ys)
          | y == x = let (f, r) = consume ys in (y:f, r)
          | otherwise = ([], y:ys)
        (first, rest) = consume xs

encode :: String -> [(Int, Char)]
encode [] = []
encode (x:xs) = (countSame x xs, x) : encode (filter (/= x) xs)
  where
    countSame :: Char -> String -> Int
    countSame c [] = 1
    countSame c (x:xs)
      | c == x = 1 + countSame c xs
      | otherwise = 1

data Encoded a = Multiple Int a | Single a deriving (Eq, Show)
encode' :: Eq a => [a] -> [Encoded a]
encode' [] = []
encode' (x:xs) = let (same, rest) = span (==x) xs in
                   if length same == 0
                   then Single x : encode' rest
                   else Multiple (1 + length same) x : encode' rest

-- decode' :: [Encoded a] -> [a]
-- decode' [] = []
-- decode' (x:xs) = case x of
--   Single a -> a : decode' xs
--   Multiple n a -> replicate n a ++ decode' xs

-- Equivalent using standard guards
decode' :: [Encoded a] -> [a]
decode' [] = []
decode' (x:xs)
  | Single     a <- x = a : decode' xs
  | Multiple n a <- x = replicate n a ++ decode' xs

-- slice :: [a] -> Int -> Int -> [a]
-- slice [ ]    _ _ = [ ]
-- slice [_:xs] _ e = [x]
-- slice (x:xs) n
--   | n > 0 = xs ++ [x]
--   | n < 0 = 

slice :: [a] -> Int -> Int -> [a]
slice lst 1 m = slice' lst m []
  where
    slice' :: [a]->Int->[a]->[a]
    slice' _ 0 acc = reverse acc
    slice' (x:xs) n acc = slice' xs (n - 1) (x:acc)
    slice' [] _ _ = []
slice (x:xs) n m = slice xs (n - 1) (m - 1)
slice []     _ _ = []

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n
  | n > 0 = rotate (xs ++ [x]) (n-1)
  | otherwise = rotate (xs ++ [x]) ((length xs + n) `mod` length xs)

-- remove_at :: [a] -> Int -> (a, [a])
-- remove_at xs n = remove_aux xs n 0 (Nothing, [])
--   where remove_aux :: [a] -> Int -> Int -> (Maybe a, [a])
--         remove_aux [] _ _ = (Nothing, [])
--         remove_aux (x:xs) n m acc
--           | m == n    = (Just x, xs)
--           | otherwise = (Nothing, x : remove_aux xs n (m + 1))

-- remove_at :: [a] -> Int -> (Maybe a, [a])
-- remove_at xs n = remove_aux xs n 0
--   where remove_aux :: [a] -> Int -> Int -> (Maybe a, [a])
--         remove_aux [] _ _ = (Nothing, [])
--         remove_aux (x:xs) n m
--           | m == n    = (Just x, xs)
--           | otherwise = let (removed, rest) =
--                               remove_aux xs n (m + 1)
--                         in (removed, x : rest)

remove_at :: [a] -> Int -> (Maybe a, [a])
remove_at xs n = (if n < length xs then Just (xs !! n) else Nothing, [x | (i, x) <- zip [0..] xs, i /= n])
