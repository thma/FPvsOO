{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib
    ( someFunc
    ) where

import Data.Function ((&))

class Shape a where
  draw :: a -> IO ()
  rotate :: Int -> a -> a
  move :: Int -> Int -> a ->  a

data Point = Point Int Int

instance Show Point where
  show (Point x y) = "Point (" ++ show x ++ "," ++ show y ++ ")"

data Rect = Rect Point Point Int deriving Show
  
data Circle = Circle Point Int  deriving Show
  
instance Shape Rect where
  draw (Rect a b or) = putStrLn $ "rectangle [" <> show a <> " " <> show b <> " or. = " <> show or <> "]"
  move x y (Rect a b or) = Rect a' b' or where 
    a' = movePoint x y a
    b' = movePoint x y b
  rotate rot (Rect a b or)  = Rect a b ((or + rot) `rem` 360)
    
instance Shape Circle where
  draw (Circle centre radius) = putStrLn $ "circle [" <> show centre <> " " <> show radius <> "]"
  rotate _ x = x
  move x y (Circle centre radius)  = Circle (movePoint x y centre) radius
  
  
movePoint :: Int -> Int -> Point -> Point
movePoint x y (Point x_a y_a) = Point (x_a + x) (y_a + y)

someFunc :: IO ()
someFunc = do
  let circle = Circle (Point 4 5) 9
      rect   = Rect   (Point 4 5) (Point 5 6) 0
  draw circle
  draw rect
  
  rect 
    & move 3 1
    & rotate 90
    & draw
    
  circle
    & rotate 90
    & move 4 2
    & draw
  
  