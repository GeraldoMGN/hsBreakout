module Types where 
  import qualified Data.Set as Set
  import Graphics.Gloss.Interface.IO.Game

  screenWidth, screenHeight :: Int
  screenWidth = 600
  screenHeight = 800

  paddleSpeed :: Float
  paddleSpeed = 7.0

  brickHeight, brickWidth :: Float
  brickHeight = 18.0
  brickWidth = 60.0

  backgroundColor, ballColor, paddleColor, brickColor :: Color
  backgroundColor = black
  ballColor = white  
  paddleColor = white 
  brickColor = red 

  type Ball = (Vector2, Vector2)

  -- X position, width
  type Paddle = (Float,Float)

  data Game = Game { 
    ball :: Ball,
    lost :: Bool,
    paddle :: Paddle,
    keys :: Set.Set Char,
    bricks :: [Vector2]
  }

  data Vector2 = Vector2 {
    x :: Float,
    y :: Float
  }