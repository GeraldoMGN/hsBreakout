module Types where 

  screenWidth, screenHeight :: Int
  screenWidth = 600
  screenHeight = 800

  type Ball = (Vector2, Vector2)

  -- X position, width
  type Paddle = (Float,Float)

  data Game = Game { 
    ball :: Ball,
    lost :: Bool,
    paddle :: Paddle
  }

  data Vector2 = Vector2 {
    x :: Float,
    y :: Float
  }