{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import           Data.Function ((&))

class Shape a where
  draw :: a -> IO ()
  move :: Double -> Double -> a -> a
  area :: a -> Double

data Point = Point Double Double

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data Rect = Rect Point Point deriving (Show)

data Circle = Circle Point Double deriving (Show)

data Triangle = Triangle Point Point Point deriving (Show)

instance Shape Rect where
  draw (Rect a b) = putStrLn $ "Rectangle [" <> show a <> ", " <> show b <> "]"    
  move x y (Rect a b) = Rect a' b'
    where
      a' = movePoint x y a
      b' = movePoint x y b      
  area (Rect (Point x_a y_a) (Point x_b y_b)) = abs ((x_b - x_a) * (y_b - y_a))

instance Shape Circle where
  draw (Circle centre radius) = putStrLn $ "Circle [" <> show centre <> ", " <> show radius <> "]"
  move x y (Circle centre radius) = Circle (movePoint x y centre) radius
  area (Circle _ r) = r^2 * pi
  
instance Shape Triangle where
  draw (Triangle a b c) = putStrLn $ "Triangle [" <> show a <> ", " <> show b <> ", " <> show c <> "]"    
  move x y (Triangle a b c) = Triangle a' b' c'
    where
      a' = movePoint x y a
      b' = movePoint x y b  
      c' = movePoint x y c    
  area (Triangle x y z) = sqrt(s * (s-a) * (s-b) * (s-c)) -- using Heron's formula
    where s = 0.5 * (a + b + c)
          a = distance x y
          b = distance y z
          c = distance x z


distance :: Point -> Point -> Double
distance (Point x_a y_a) (Point x_b y_b) = sqrt ((x_b - x_a)^2 + (y_b - y_a)^2)

movePoint :: Double -> Double -> Point -> Point
movePoint x y (Point x_a y_a) = Point (x_a + x) (y_a + y)

main :: IO ()
main = do
  let circle   = Circle (Point 4 5) 9
      rect     = Rect (Point 4 5) (Point 5 6)
      triangle = Triangle (Point 0 0) (Point 4 0) (Point 4 3)
      
  draw circle
  draw rect
  draw triangle

  rect
    & move 3 1
    & draw
    
  circle
    & move 4 2
    & draw
    
  triangle
    & move 4 2
    & draw
    
  print (rect & area)  
  print (circle & area)
  print (triangle & area)
