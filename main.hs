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

main :: IO ()
main = putStrLn "Hello, world!"
