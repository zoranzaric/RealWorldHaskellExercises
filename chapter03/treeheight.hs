import Test.HUnit


-- Using the binary tree type that we defined earlier in this chapter, write a
-- function that will determine the height of the tree. The height is the
-- largest number of hops from the root to an Empty. For example, the tree Empty
-- has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node
-- "y" Empty Empty) has height two; and so on.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 1
treeHeight (Node _ left right)
          | leftHeight >= rightHeight = 1 + leftHeight
          | otherwise                 = 1 + rightHeight
          where leftHeight = (treeHeight left)
                rightHeight = (treeHeight right)

treeHeightTest1 = TestCase (assertEqual "empty tree" 1 (treeHeight Empty))
treeHeightTest2 = TestCase (assertEqual "single node" 2 (treeHeight (Node "root" Empty Empty)))
treeHeightTests = TestList [TestLabel "empty tree" treeHeightTest1,
                            TestLabel "single node" treeHeightTest2]



main = do putStrLn "treeHeight Tests"
          runTestTT treeHeightTests

