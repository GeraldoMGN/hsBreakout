module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.STM

import Types

window :: Display
window = InWindow "hsTetris" (600, 800) (10, 10)

backgroundColor, ballColor :: Color
backgroundColor = black
ballColor = white  

newGame :: Game
newGame = Game {
  ball = ((Vector2 0 0), (Vector2 1 1))
}

drawBall :: Game -> Picture
drawBall world = pictures [(uncurry translate (posX,posY) $ color ballColor $ circleSolid 10)]
  where
    ((Vector2 posX posY ),_) = ball world

gameAsPicture :: TVar Game -> IO Picture
gameAsPicture world = do
  temp <- atomically(readTVar world)
  let pics = pictures[drawBall temp] in
    return pics
    

transformGame :: Event -> TVar Game -> IO (TVar Game )
transformGame _ game = do 
  temp <- atomically(readTVar game)
  return game

gameLogic :: TVar Game -> IO()
gameLogic gameStateTVar = do
  (Game ((Vector2 posX posY), (Vector2 velX velY))) <- atomically(readTVar gameStateTVar)
  atomically(
    writeTVar gameStateTVar (Game ((Vector2 (posX + velX) (posY + velY)), (Vector2 velX velY)))  )
  threadDelay (16666)
  gameLogic gameStateTVar

update seconds int = return int

main :: IO ()
main = do
  temp <- atomically(newTVar newGame)
  forkIO $ gameLogic temp
  playIO window backgroundColor 60 temp gameAsPicture transformGame update

