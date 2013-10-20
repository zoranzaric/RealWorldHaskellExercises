import Test.HUnit

import Data.Ord
import Data.List

-- Create a function that sorts a list of lists based on the length of each
-- sublist. (You may want to look at the sortBy function from the Data.List
-- module.)

sortLists :: [[a]] -> [[a]]
sortLists xs = sortBy (comparing length) xs

sortListsTest1 = TestCase (assertEqual "[]" [] (sortLists [] :: [[Int]]))
sortListsTest2 = TestCase (assertEqual "[[1]]" [[1]] (sortLists [[1]]))
sortListsTest3 = TestCase (assertEqual "[[1,2 ], [1]]" [[1], [1, 2]] (sortLists [[1,2 ], [1]]))
sortListsTests = TestList [TestLabel "sortListsTest1" sortListsTest1,
                           TestLabel "sortListsTest2" sortListsTest2,
                           TestLabel "sortListsTest3" sortListsTest3]



-- Write a function that computes the number of elements in a list. To test it,
-- ensure that it gives the same answers as the standard length function.

myLength :: [a] -> Int
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = 1 + myLength (head xs : tail xs)

lengthTest1 = TestCase (assertEqual "for mylength []" 0 (myLength []))
lengthTest2 = TestCase (assertEqual "for mylength ['a']" 1 (myLength ['a']))
lengthTests = TestList [TestLabel "lengthTest1" lengthTest1, TestLabel "lengthTest2" lengthTest2]



-- Write a function that computes the mean of a list, i.e. the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer into
-- a floating point number.)

meanOfList :: Fractional a => [a] -> a
meanOfList [] = 0
meanOfList xs = (foldl (+) 0 xs) / (fromIntegral (length xs))

meanOfListTest1 = TestCase (assertEqual "mean of [0, 1]" 0.5 (meanOfList [0, 1]))
meanOfListTests = TestList [TestLabel "meanOfListTest1" meanOfListTest1]



main = do putStrLn "sortLists Tests"
          runTestTT sortListsTests
          putStrLn ""
          putStrLn "length Tests"
          runTestTT lengthTests
          putStrLn ""
          putStrLn "meanOfList Tests"
          runTestTT meanOfListTests

