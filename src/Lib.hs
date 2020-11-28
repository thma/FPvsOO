{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Lib where

import           Data.Function ((&))

-- | a point in the two-dimensional plane
data Point = Point Double Double

-- | The Shape type class. It defines four functions that all concrete Shape types must implement.
class Shape a where
  -- | render a Shape
  draw   :: a -> IO ()
  -- | move a Shape by an x and y amount
  move   :: (Double,Double) -> a -> a
  -- | compute the area of a Shape
  area   :: a -> Double
  -- | compute the circumference of a Shape
  circum :: a -> Double



-- | a rectangle defined by to points (bottom left and top right corners) 
data Rect = Rect Point Point deriving (Show)

-- | a circle defined by the centre point and a radius
data Circle = Circle Point Double deriving (Show)

-- | a triangle defined by three points
data Triangle = Triangle Point Point Point deriving (Show)

-- | making Rect an instance of Shape
instance Shape Rect where
  draw       (Rect a b) = putStrLn $ "Rectangle [" ++ show a ++ ", " ++ show b ++ "]"
  move (x,y) (Rect a b) = Rect a' b'
    where
      a' = movePoint x y a
      b' = movePoint x y b
  area rect   = width * height
    where
      (width, height) = widthAndHeight rect
  circum rect = 2 * (width + height)
    where
      (width, height) = widthAndHeight rect

-- | computes the width and height of a rectangle, returns them as a tuple
widthAndHeight :: Rect -> (Double, Double)
widthAndHeight (Rect (Point x_a y_a) (Point x_b y_b)) = (abs (x_b - x_a), abs (y_b - y_a))

-- | move a Point by an x and y amount
movePoint :: Double -> Double -> Point -> Point
movePoint x y (Point x_a y_a) = Point (x_a + x) (y_a + y)


-- | making Circle an instance of Shape
instance Shape Circle where
  draw       (Circle centre radius) = putStrLn $ "Circle [" ++ show centre ++ ", " ++ show radius ++ "]"
  move (x,y) (Circle centre radius) = Circle (movePoint x y centre) radius
  area   (Circle _ r) = r ^ 2 * pi
  circum (Circle _ r) = 2 * r * pi

-- | making Triangle an instance of Shape
instance Shape Triangle where
  draw       (Triangle a b c) = putStrLn $ "Triangle [" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ "]"
  move (x,y) (Triangle a b c) = Triangle a' b' c'
    where
      a' = movePoint x y a
      b' = movePoint x y b
      c' = movePoint x y c
  area   triangle = sqrt (s * (s - a) * (s - b) * (s - c)) -- using Heron's formula
    where
      s = 0.5 * circum triangle
      (a, b, c) = sides triangle
  circum triangle = a + b + c
    where
      (a, b, c) = sides triangle

-- | computing the length of all sides of a triangle, returns them as a triple
sides :: Triangle -> (Double, Double, Double)
sides (Triangle x y z) = (distance x y, distance y z, distance x z)

-- | compute the distance between two points
distance :: Point -> Point -> Double
distance (Point x_a y_a) (Point x_b y_b) = sqrt ((x_b - x_a) ^ 2 + (y_b - y_a) ^ 2)

-- | provide a dense representation of a point
instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data ShapeType = forall a . Shape a => MkShape a


instance Shape ShapeType where
  area     (MkShape a) = area a
  circum   (MkShape a) = circum a
  draw     (MkShape a) = draw a
  move (x,y) (MkShape i) =  undefined

pack :: Shape a => a -> ShapeType
pack = MkShape

main :: IO ()
main = do
  let rect = Rect (Point 0 0) (Point 5 4)
      circle = Circle (Point 4 5) 4
      triangle = Triangle (Point 0 0) (Point 4 0) (Point 4 3)
      list = [pack rect, pack circle, pack triangle]

  print $ map area list

  --mapM_ (draw . move (1, 2)) list

  mapM_ draw list

  putStrLn "draw all shapes:"
  draw rect
  draw circle
  draw triangle

  putStrLn "\nnow move all shapes:"
  rect
    & move (4,2)
    & draw

  circle
    & move (4,2)
    & draw

  triangle
    & move (4,2)
    & draw

  putStrLn "\ncompute area of all shapes:"
  print (rect & area)
  print (circle & area)
  print (triangle & area)

  putStrLn "\ncompute circumference of all shapes:"
  print (rect & circum)
  print (circle & circum)
  print (triangle & circum)