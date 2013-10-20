import Test.HUnit


-- Turn a list into a palindrome, i.e. it should read the same both backwards
-- and forwards. For example, given the list [1,2,3], your function should
-- return [1,2,3,3,2,1].

palindromize :: [a] -> [a]
palindromize xs = xs ++ reverse xs

palindromizeTest1 = TestCase (assertEqual "[]" [] (palindromize [] :: [Int]))
palindromizeTest2 = TestCase (assertEqual "[1]" [1,1] (palindromize [1]))
palindromizeTest3 = TestCase (assertEqual "[1,2]" [1,2,2,1] (palindromize [1,2]))
palindromizeTests = TestList [TestLabel "[]" palindromizeTest1,
                              TestLabel "[1]" palindromizeTest2,
                              TestLabel "[1,2]" palindromizeTest3]



-- Write a function that determines whether its input list is a palindrome.

lastElement :: [a] -> Maybe a
lastElement [] = Nothing
lastElement (x:[]) = Just x
lastElement (x:xs) = lastElement xs

lastElementTest1 = TestCase(assertEqual "[]" Nothing (lastElement [] :: Maybe Int))
lastElementTest2 = TestCase(assertEqual "[1]" (Just 1) (lastElement [1]))
lastElementTest3 = TestCase(assertEqual "[1,2]" (Just 2) (lastElement [1,2]))
lastElementTests = TestList [TestLabel "lastElementTest1" lastElementTest1,
                             TestLabel "lastElementTest2" lastElementTest2,
                             TestLabel "lastElementTest3" lastElementTest3]

butLast :: [a] -> [a]
butLast (x:[]) = []
butLast [] = []
butLast (x:xs) = x : (butLast xs)

butLastTest1 = TestCase (assertEqual "for butLast []" [] (butLast [] :: [Int]))
butLastTest2 = TestCase (assertEqual "for butLast [1]" [] (butLast [1]))
butLastTest3 = TestCase (assertEqual "for butLast [1,2]" [1] (butLast [1,2]))
butLastTests = TestList [TestLabel "butLastTest1" butLastTest1,
                         TestLabel "butLastTest2" butLastTest2,
                         TestLabel "butLastTest3" butLastTest3]



isPalindrome :: Eq a => [a] -> Bool
isPalindrome (x:[]) = True
isPalindrome [] = True
isPalindrome (x:xs) = Just x == (lastElement xs) && (isPalindrome (butLast xs))

isPalindromeTest1 = TestCase (assertEqual "[]" True (isPalindrome ([] :: [Int])))
isPalindromeTest2 = TestCase (assertEqual "[1]" True (isPalindrome [1]))
isPalindromeTest3 = TestCase (assertEqual "[1,2]" False (isPalindrome [1, 2]))
isPalindromeTest4 = TestCase (assertEqual "[1,2,1]" True (isPalindrome [1, 2, 1]))
isPalindromeTests = TestList [TestLabel "isPalindromeTest1" isPalindromeTest1,
                              TestLabel "isPalindromeTest2" isPalindromeTest2,
                              TestLabel "isPalindromeTest3" isPalindromeTest3,
                              TestLabel "isPalindromeTest4" isPalindromeTest4]



main = do putStrLn "palindromize Tests"
          runTestTT palindromizeTests
          putStrLn ""
          putStrLn "lastElement Tests"
          runTestTT lastElementTests
          putStrLn ""
          putStrLn "butLast Tests"
          runTestTT butLastTests
          putStrLn ""
          putStrLn "isPalindrome Tests"
          runTestTT isPalindromeTests

