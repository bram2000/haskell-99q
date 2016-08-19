isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome (x:xs) = if x == last xs
    then isPalindrome (init xs)
    else False



isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 x = x == reverse x
