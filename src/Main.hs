module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.STM

import Types

window :: Display
window = InWindow "hsTetris" (screenWidth, screenHeight) (10, 10)

backgroundColor, ballColor :: Color
backgroundColor = black
ballColor = white  

newGame :: Game
newGame = Game {
  ball = (Vector2 0 (-350), Vector2 5 5),
  lost = False
}

drawBall :: Game -> Picture
drawBall world = pictures [(uncurry translate (posX,posY) $ color ballColor $ circleSolid 10)]
  where
    (Vector2 posX posY ,_) = ball world

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
  gameState <- atomically(readTVar gameStateTVar)
  let newGameState = moveBall gameState
  atomically $ writeTVar gameStateTVar $ newGameState
  threadDelay (16666)
  gameLogic gameStateTVar

-- Moves the ball and check if passed the bottom of the screen
moveBall :: Game -> Game
moveBall gameState =
  Game {
  ball = collidesWalls oldBall,
  lost = didLose oldBall
  }
  where
    Game oldBall lost = gameState

collidesWalls :: Ball -> Ball
collidesWalls ball
  | (ballPosX + ballVelX) >=  290 = (Vector2 ballPosX ballPosY, Vector2 (-ballVelX) ballVelY)
  | (ballPosX + ballVelX) <= -290 = (Vector2 ballPosX ballPosY, Vector2 (-ballVelX) ballVelY)
  | (ballPosY + ballVelY) >=  390 = (Vector2 ballPosX ballPosY, Vector2 ballVelX (-ballVelY)) 
  | otherwise                     = (Vector2 (ballPosX + ballVelX) (ballPosY + ballVelY), Vector2 ballVelX ballVelY)
  where
    (Vector2 ballPosX ballPosY, Vector2 ballVelX ballVelY) = ball

didLose :: Ball -> Bool
didLose ball =
  ballPosY < (-390)
  where
    (Vector2 ballPosX ballPosY, _) = ball
    
update seconds int = return int

main :: IO ()
main = do
  temp <- atomically(newTVar newGame)
  forkIO $ gameLogic temp
  playIO window backgroundColor 60 temp gameAsPicture transformGame update

