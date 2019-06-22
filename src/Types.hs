module Types where 
  import qualified Data.Set as Set
  import Graphics.Gloss.Interface.IO.Game

  screenWidth, screenHeight :: Int
  screenWidth = 600
  screenHeight = 800

  paddleSpeed :: Float
  paddleSpeed = 7.0

  type Ball = (Vector2, Vector2)

  -- X position, width
  type Paddle = (Float,Float)

  data Game = Game { 
    ball :: Ball,
    lost :: Bool,
    paddle :: Paddle,
    keys :: Set.Set Char
  }

  data Vector2 = Vector2 {
    x :: Float,
    y :: Float
  }