{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib
    ( someFunc
    ) where

import Data.Function ((&))

class Shape a where
  draw :: a -> IO ()
  rotate :: Int -> a -> a
  move :: Vector -> a ->  a

data Point = Point Int Int

data Vector = Vector Int Int

instance Show Point where
  show (Point x y) = "Point (" ++ show x ++ "," ++ show y ++ ")"

data Rect = Rect Point Point Int deriving Show
  
data Circle = Circle Point Int  deriving Show
  
instance Shape Rect where
  draw (Rect a b or) = putStrLn $ "rectangle [" <> show a <> " " <> show b <> " or. = " <> show or <> "]"
  move vector (Rect a b or) = Rect a' b' or where 
    a' = movePoint a vector
    b' = movePoint b vector
  rotate rot (Rect a b or)  = Rect a b ((or + rot) `rem` 360)
    
instance Shape Circle where
  draw (Circle centre radius) = putStrLn $ "circle [" <> show centre <> " " <> show radius <> "]"
  rotate _ x = x
  move vector (Circle centre radius)  = Circle (movePoint centre vector) radius
  
  
movePoint :: Point -> Vector -> Point
movePoint (Point x_a y_a) (Vector x y) = Point (x_a + x) (y_a + y)

moveBy :: (Shape a) => Int -> Int -> a -> a
moveBy x y = move (Vector x y)

someFunc :: IO ()
someFunc = do
  let circle = Circle (Point 4 5) 9
      rect   = Rect   (Point 4 5) (Point 5 6) 0
  draw circle
  draw rect
  
  rect 
    & moveBy 3 1
    & rotate 90
    & draw
    
  circle
    & rotate 90
    & moveBy 4 2
    & draw
  
  