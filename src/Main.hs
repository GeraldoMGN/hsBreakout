module Main where
  
import Graphics.Gloss

window :: Display
window = InWindow "hsTetris" (1280, 720) (10, 10)

backgroundColor :: Color
backgroundColor = black

initialGame = 42

gameAsPicture _ = Blank

transformGame _ game = game

main :: IO ()
main = play window backgroundColor 60 initialGame gameAsPicture transformGame (const id)

