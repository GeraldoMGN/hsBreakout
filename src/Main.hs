module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as Set
import Control.Concurrent
import Control.Concurrent.STM

import Types

{- TODOS
- Use the arrow keys
- Fix ball paddle side collision bug
-}

toInt :: Float -> Int
toInt = round

window :: Display
window = InWindow "hsBreakout" (screenWidth, screenHeight) (10, 10)

newGame :: Game
newGame = Game {
  ball = (Vector2 0 (-340), Vector2 4 4),
  lost = False,
  paddle = (0,100),
  keys = Set.empty,
  bricks = createBricks 10
}

createBricks :: Int -> [Vector2]
createBricks num = [Vector2 x y 
  | x <- [screenLeft, screenLeft + (brickWidth + 2) .. fromIntegral(screenWidth `div` 2) - brickHalfWidth]
  , y <- [screenTop , screenTop  - (brickHeight+ 2) .. screenTop - fromIntegral(num - 1) * (brickHeight + 2)] ]
  where brickHalfWidth  = brickWidth  / 2.0
        brickHalfHeight = brickHeight / 2.0
        screenLeft = brickHalfWidth - fromIntegral(screenWidth `div` 2) + 1
        screenTop = fromIntegral(screenHeight `div` 2) - brickHalfHeight

drawBall :: Ball -> Picture
drawBall ball = pictures [(uncurry translate (posX,posY) $ color ballColor $ rectangleSolid 10 10)]
  where
    (Vector2 posX posY ,_) = ball

drawPaddle :: Paddle -> Picture
drawPaddle paddle = pictures [(uncurry translate (posX, (-370)) $ color paddleColor $ rectangleSolid width 10)]
  where
    (posX, width) = paddle

drawBricks :: [Vector2] -> Picture
drawBricks bricks = pictures [drawBrick brick | brick <- bricks]

drawBrick :: Vector2 -> Picture
drawBrick center = pictures [(uncurry translate (posX, posY) $ color (getColor posY) $ rectangleSolid brickWidth brickHeight)]
  where
    (Vector2 posX posY) = center

getColor :: Float -> Color
getColor posY 
  | posY == auxGetColor 0 = red
  | posY == auxGetColor 1 = red
  | posY == auxGetColor 2 = orange
  | posY == auxGetColor 3 = orange
  | posY == auxGetColor 4 = green
  | posY == auxGetColor 5 = green
  | posY == auxGetColor 6 = yellow
  | posY == auxGetColor 7 = yellow
  | posY == auxGetColor 8 = blue
  | posY == auxGetColor 9 = blue
  | posY == auxGetColor 10 = magenta 
  | posY == auxGetColor 11 = magenta 
  | otherwise = blue
  where auxGetColor num = fromIntegral(screenHeight `div` 2) - (brickHeight)/2.0 - fromIntegral(num)*(brickHeight+2)

gameAsPicture :: TVar Picture ->TVar Game -> IO Picture
gameAsPicture renderBufferTVar gameStateTVar = do
  gameState <- atomically(readTVar gameStateTVar)
  renderBuffer <- atomically(readTVar renderBufferTVar)
  forkIO $ gameRender renderBufferTVar gameState
  return renderBuffer

gameRender :: TVar Picture -> Game -> IO ()
gameRender renderBufferTVar gameState = do
  let Game ball _ paddle _ bricks = gameState
  let pics = pictures[(drawBall ball), (drawPaddle paddle), (drawBricks bricks)] in
    atomically (writeTVar renderBufferTVar pics)

processInput :: Paddle -> Set.Set Char -> Paddle
processInput paddle keys
  | Set.member 'a' keys = movePaddle paddle (-paddleSpeed)
  | Set.member 'd' keys = movePaddle paddle paddleSpeed
  | otherwise = paddle

movePaddle :: Paddle -> Float -> Paddle
movePaddle paddle d 
  | newPosX <= (fromIntegral(screenWidth `div` 2) - (width/2.0)) 
    && newPosX >= ((-fromIntegral(screenWidth `div` 2)) + (width/2.0)) = (newPosX, width)
  | otherwise = paddle
  where
    (posX, width) = paddle
    newPosX = posX + d

updatePressedKeys :: Game -> KeyState -> Char -> Game
updatePressedKeys gameState state key
  | state == Up = (Game ball lost paddle (Set.delete key keys) bricks)
  | state == Down = (Game ball lost paddle (Set.insert key keys) bricks)
  where
    Game ball lost paddle keys bricks = gameState

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

iterateLogic :: Game -> Game
iterateLogic gameState =
  Game {
  ball = newBall,
  lost = didLose oldBall,
  paddle = processInput paddle keys,
  keys = keys,
  bricks = newBricks
  }
  where
    Game oldBall lost paddle keys oldBricks = gameState
    Game newBall _ _ _ newBricks = collide gameState

collide :: Game -> Game
collide gameState = 
  let ballWallCollided = collideWalls ball
      ballPaddleCollided = collidePaddle ballWallCollided paddle
      collidedBricks@(newBall, newBricks) = collideBricks ballPaddleCollided bricks in
        Game { 
          ball = newBall,
          lost = lost,
          paddle = paddle,
          keys = keys,     
          bricks = newBricks
      }
  where
    Game ball lost paddle keys bricks = gameState

collideBricks :: Ball -> [Vector2] -> (Ball, [Vector2])
collideBricks ball (brick:bricks)
  | didCollideBrick ball brick == True = (collidedBall, bricks)
  | otherwise = (tailBall, brick:tailBricks)
  where 
    collidedBall = (reboundDirection ball brick)
    (tailBall, tailBricks) = collideBricks collidedBall bricks
collideBricks ball [] = (ball,[])

didCollideBrick :: Ball -> Vector2 -> Bool
didCollideBrick ball brick
  | brickMinX < ballMaxX && brickMaxX > ballMinX &&
    brickMinY < ballMaxY && brickMaxY > ballMinY = True
  | otherwise = False
    where
      (Vector2 ballPosX ballPosY, _) = ball
      (Vector2 brickPosX brickPosY) = brick
      ((Vector2 brickMinX brickMinY),(Vector2 brickMaxX brickMaxY)) = bounds brick brickWidth brickHeight
      ((Vector2 ballMinX ballMinY),(Vector2 ballMaxX ballMaxY)) = bounds (Vector2 ballPosX ballPosY) 10 10

reboundDirection :: Ball -> Vector2 -> Ball
reboundDirection ball brick
  | didCollideBrick ballX brick && 
    didCollideBrick ballY brick = ((undoStep ball),(Vector2 (-ballVelX) (-ballVelY)))
  | didCollideBrick ballX brick = ((undoStep ball),(Vector2 ballVelX (-ballVelY)))
  | didCollideBrick ballY brick = ((undoStep ball),(Vector2 (-ballVelX) ballVelY))
  | otherwise                   = ball
  where
    (Vector2 ballPosX ballPosY, Vector2 ballVelX ballVelY) = ball
    (Vector2 brickPosX brickPosY) = brick
    ballX = (Vector2 (ballPosX - ballVelX) ballPosY, Vector2 0 0)
    ballY = (Vector2 ballPosX (ballPosY - ballVelY), Vector2 0 0)

undoStep :: Ball -> Vector2
undoStep ball = (Vector2 newX newY)
  where (Vector2 posX posY, Vector2 velX velY) = ball
        newX = posX - velX
        newY = posY - velY

bounds :: Vector2 -> Float -> Float -> (Vector2, Vector2)    
bounds (Vector2 posX posY) width height = ((Vector2 (posX - width/2.0) (posY - height/2.0)), (Vector2 (posX + width/2.0) (posY + height/2.0)))

collidePaddle :: Ball -> Paddle -> Ball
collidePaddle ball paddle
  | ballPosY <= -360 && 
    ballPosY >= -370 && 
    ballPosX >= paddlePosX - paddleWidth/2 && 
    ballPosX <= paddlePosX + paddleWidth/2 = (Vector2 ballPosX ballPosY, Vector2 ballVelX (-ballVelY))
  | otherwise = ball
  where
    (Vector2 ballPosX ballPosY, Vector2 ballVelX ballVelY) = ball
    (paddlePosX, paddleWidth) = paddle


collideWalls :: Ball -> Ball
collideWalls ball
  | (ballPosX + ballVelX) >=  295 || (ballPosX + ballVelX) <= -295 = (Vector2 ballPosX ballPosY, Vector2 (-ballVelX) ballVelY)
  | (ballPosY + ballVelY) >=  395 = (Vector2 ballPosX ballPosY, Vector2 ballVelX (-ballVelY)) 
  | otherwise                     = (Vector2 (ballPosX + ballVelX) (ballPosY + ballVelY), Vector2 ballVelX ballVelY)
  where
    (Vector2 ballPosX ballPosY, Vector2 ballVelX ballVelY) = ball

didLose :: Ball -> Bool
didLose ball =
  ballPosY < (-390)
  where
    (Vector2 ballPosX ballPosY, _) = ball
    
update seconds gameState = do
  forkIO $ gameLogic gameState
  return gameState

main :: IO ()
main = do
  gameStatusTVar <- atomically(newTVar newGame)
  renderBufferTVar <- atomically(newTVar (pictures[(rectangleSolid 0 0)]))

  playIO window backgroundColor 60 gameStatusTVar (gameAsPicture renderBufferTVar) transformGame update

