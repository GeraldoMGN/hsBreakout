module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as Set
import Control.Concurrent
import Control.Concurrent.STM

import Types

{- TODOS
- Use the arrow keys
-}

toInt :: Float -> Int
toInt = round

window :: Display
window = InWindow "hsBreakout" (screenWidth, screenHeight) (10, 10)

backgroundColor, ballColor, paddleColor :: Color
backgroundColor = black
ballColor = white  
paddleColor = white 

newGame :: Game
newGame = Game {
  ball = (Vector2 0 (-350), Vector2 5 5),
  lost = False,
  paddle = (0,100),
  keys = Set.empty
}

drawBall :: Ball -> Picture
drawBall ball = pictures [(uncurry translate (posX,posY) $ color ballColor $ circleSolid 10)]
  where
    (Vector2 posX posY ,_) = ball

drawPaddle :: Paddle -> Picture
drawPaddle paddle = pictures [(uncurry translate (posX, (-370)) $ color paddleColor $ rectangleSolid width 10)]
  where
    (posX, width) = paddle

gameAsPicture :: TVar Game -> IO Picture
gameAsPicture world = do
  gameState <- atomically(readTVar world)
  let Game ball _ paddle _ = gameState
  let pics = pictures[(drawBall ball), (drawPaddle paddle)] in
    return pics

processInput :: Paddle -> Set.Set Char -> Paddle
processInput paddle keys
  | Set.member 'a' keys = movePaddle paddle (-paddleSpeed)
  | Set.member 'd' keys = movePaddle paddle paddleSpeed
  | otherwise = paddle

-- TODO: Check bounds between walls and paddle
movePaddle :: Paddle -> Float -> Paddle
movePaddle paddle d = ((posX + d), width)
  where
    (posX, width) = paddle

updatePressedKeys :: Game -> KeyState -> Char -> Game
updatePressedKeys gameState state key
  | state == Up = (Game ball lost paddle (Set.delete key keys))
  | state == Down = (Game ball lost paddle (Set.insert key keys))
  where
    Game ball lost paddle keys = gameState

transformGame :: Event -> TVar Game -> IO (TVar Game)
transformGame (EventKey (Char key) state _ _)  gameStateTVar = do
  gameState <- atomically(readTVar gameStateTVar)
  let newGameState = updatePressedKeys gameState state key
  atomically $ writeTVar gameStateTVar $ newGameState
  return gameStateTVar

transformGame _ gameStateTVar = do 
  return gameStateTVar

gameLogic :: TVar Game -> IO()
gameLogic gameStateTVar = do
  gameState <- atomically(readTVar gameStateTVar)
  let newGameState = iterateLogic gameState
  atomically $ writeTVar gameStateTVar $ newGameState
  threadDelay (16666)
  gameLogic gameStateTVar

-- Moves the ball and check if passed the bottom of the screen
iterateLogic :: Game -> Game
iterateLogic gameState =
  Game {
  ball = collidesWalls oldBall,
  lost = didLose oldBall,
  paddle = processInput paddle keys,
  keys = keys
  }
  where
    Game oldBall lost paddle keys = gameState

collidesWalls :: Ball -> Ball
collidesWalls ball
  | (ballPosX + ballVelX) >=  290 || (ballPosX + ballVelX) <= -290 = (Vector2 ballPosX ballPosY, Vector2 (-ballVelX) ballVelY)
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
  gameStatusTVar <- atomically(newTVar newGame)
  forkIO $ gameLogic gameStatusTVar

  playIO window backgroundColor 60 gameStatusTVar gameAsPicture transformGame update

