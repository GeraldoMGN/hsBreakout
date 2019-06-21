module Types where 

  data Game = Game { 
    ball :: (Vector2, Vector2)
  }
  
  data Vector2 = Vector2 {
    x :: Float,
    y :: Float
  }