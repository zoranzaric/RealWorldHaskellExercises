import Test.HUnit


-- Consider three two-dimensional points a, b, and c. If we look at the angle
-- formed by the line segment from a to b and the line segment from b to c, it
-- either turns left, turns right, or forms a straight line. Define a Direction
-- data type that lets you represent these possibilities.

data Point a = Point {
    x, y :: a
}

data Direction = LeftTurn
               | RightTurn
               | Straight
                 deriving (Show, Eq)



-- Write a function that calculates the turn made by three 2D points and returns
-- a Direction.

steepness :: (Integral a, Fractional b) => Point a -> Point a -> b
steepness (Point xA yA) (Point xB yB)
         | yA == yB = 0
         | otherwise = (fromIntegral (xB - xA)) / (fromIntegral (yB - yA))

directionForPoints :: Integral a => Point a -> Point a -> Point a -> Direction
directionForPoints a b c
                  | steepnessAB < steepnessBC = LeftTurn
                  | steepnessAB > steepnessBC = RightTurn
                  | steepnessAB == steepnessBC = Straight
                  where steepnessAB = steepness a b
                        steepnessBC = steepness b c

directionForPointsTest1 = TestCase (assertEqual "straight" Straight (directionForPoints (Point 0 0) (Point 1 1) (Point 2 2)))
directionForPointsTest2 = TestCase (assertEqual "left" LeftTurn (directionForPoints (Point 0 0) (Point 1 1) (Point 3 2)))
directionForPointsTest3 = TestCase (assertEqual "right" RightTurn (directionForPoints (Point 0 0) (Point 1 1) (Point 1 1)))
directionForPointsTests = TestList [TestLabel "straight" directionForPointsTest1,
                                    TestLabel "left" directionForPointsTest2,
                                    TestLabel "right" directionForPointsTest3]



-- Define a function that takes a list of 2D points and computes the direction
-- of each successive triple. Given a list of points [a,b,c,d,e], it should
-- begin by computing the turn made by [a,b,c], then the turn made by [b,c,d],
-- then [c,d,e]. Your function should return a list of Direction.

route :: Integral a => [Point a] -> [Direction]
route (a:b:c:xs) = (directionForPoints a b c) : (route (b : c : xs))
route _ = []

routeTest1 = TestCase (assertEqual "none" [] (route []))
routeTest2 = TestCase (assertEqual "one" [] (route [(Point 0 0)]))
routeTest3 = TestCase (assertEqual "two" [] (route [(Point 0 0), (Point 0 0)]))
routeTest4 = TestCase (assertEqual "left" [LeftTurn] (route [(Point 0 0), (Point 1 1), (Point 3 2)]))
routeTest5 = TestCase (assertEqual "straight, straight" [Straight, Straight] (route [(Point 0 0), (Point 1 1), (Point 2 2), (Point 3 3)]))
routeTests = TestList [TestLabel "none" routeTest1,
                       TestLabel "one" routeTest2,
                       TestLabel "two" routeTest3,
                       TestLabel "left" routeTest4,
                       TestLabel "straight, straight" routeTest5]



main = do putStrLn "directionForPoints Tests"
          runTestTT directionForPointsTests
          putStrLn ""
          putStrLn "route Tests"
          runTestTT routeTests

