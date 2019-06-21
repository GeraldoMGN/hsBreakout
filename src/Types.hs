module Types where 

  screenWidth, screenHeight :: Int
  screenWidth = 600
  screenHeight = 800

  type Ball = (Vector2, Vector2)

  data Game = Game { 
    ball :: Ball,
    lost :: Bool
  }
  
  data Vector2 = Vector2 {
    x :: Float,
    y :: Float
  }