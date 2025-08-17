{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise2

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

data LightState = Go | Stop | Slow | Ready

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0  0 (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 9

trafficLight :: LightState -> Picture
trafficLight Go  = botCircle green & midCircle black & topCircle black & frame
trafficLight Stop = botCircle black & midCircle black & topCircle red   & frame
trafficLight Slow = botCircle black & midCircle yellow & topCircle black   & frame
trafficLight Ready = botCircle black & midCircle yellow & topCircle red   & frame

nextState :: LightState -> LightState

nextState Go = Slow
nextState Slow = Stop
nextState Stop = Ready
nextState Ready = Go

trafficController :: Int -> Picture
trafficController t
  | t >= 0 && t < 3  = trafficLight Go
  | t == 3           = trafficLight Slow
  | t >= 4 && t < 7  = trafficLight Stop
  | otherwise        = trafficLight Ready


trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 8)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree _ 0 = blank
tree b n = polyline [(0,0),(0,1)] & translated 0 1 b & translated 0 1 (
  rotated (pi/10) (tree b (n-1)) & rotated (- pi/10) (tree b (n-1)))
  
blossom :: Double -> Picture
blossom t
    | t <= 10 = colored yellow (solidCircle (t / 50 ))
    | otherwise =colored yellow (solidCircle 0.2)


bloomingTree :: Double -> Picture
bloomingTree t = tree (blossom t) 8

exercise2 :: IO ()
exercise2 = animationOf bloomingTree

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    undefined
ground =  undefined
storage = undefined
box =     undefined

drawTile :: Integer -> Picture
drawTile = undefined

         
pictureOfMaze :: Picture
pictureOfMaze = undefined

exercise3 :: IO ()
exercise3 = undefined
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 