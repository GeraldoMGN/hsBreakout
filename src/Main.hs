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

newEasyGame :: Game
newEasyGame = Game {
  ball = (Vector2 0 (-300), Vector2 2 3),
  conditions = Conditions True False False,
  paddle = (0,100),
  keys = Set.empty,
  bricks = createBricks 6
}

newMediumGame :: Game
newMediumGame = Game {
  ball = (Vector2 0 (-300), Vector2 4 5),
  conditions = Conditions True False False,
  paddle = (0,80),
  keys = Set.empty,
  bricks = createBricks 8
}

newHardGame :: Game
newHardGame = Game {
  ball = (Vector2 0 (-300), Vector2 5 6),
  conditions = Conditions True False False,
  paddle = (0,60),
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
drawPaddle paddle = pictures [(uncurry translate (posX, (-320)) $ color paddleColor $ rectangleSolid width 10)]
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
  atomically (writeTVar renderBufferTVar (chooseScreen gameState))
  
chooseScreen :: Game -> Picture
chooseScreen gameState
  | win  = 
    pictures[(scale 0.4  0.4  $ uncurry translate (-400 ,  200) $ color blue  $ text "Voce ganhou!"),
             (scale 0.18 0.18 $ uncurry translate (-1550,  100) $ color white $ Text "Aperte 1 2 ou 3 para selecionar a dificuldade")]
  | lost = 
    pictures[(scale 0.4  0.4  $ uncurry translate (-400 ,  200) $ color red   $ text "Voce perdeu!"),
             (scale 0.18 0.18 $ uncurry translate (-1550,  100) $ color white $ Text "Aperte 1 2 ou 3 para selecionar a dificuldade")]
  | started == False = 
    pictures[(scale 0.4  0.4  $ uncurry translate (-300 ,  200) $ color red   $ Text "hsBreakout"),
             (scale 0.18 0.18 $ uncurry translate (-1550,  100) $ color white $ Text "Aperte 1 2 ou 3 para selecionar a dificuldade"),
             (scale 0.18 0.18 $ uncurry translate (-950 , -100) $ color white $ Text "1: Facil  2: Medio  3: Dificil"),
             (scale 0.18 0.18 $ uncurry translate (-1100, -300) $ color white $ Text "Aperte durante o jogo para resetar")]
  | otherwise = pictures[(drawBall ball), (drawPaddle paddle), (drawBricks bricks)]
  where Game ball conditions paddle _ bricks = gameState
        Conditions started lost win = conditions

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
  | state == Up = (Game ball conditions paddle (Set.delete key keys) bricks)
  | state == Down = (Game ball conditions paddle (Set.insert key keys) bricks)
  where
    Game ball conditions paddle keys bricks = gameState

transformGame :: Event -> TVar Game -> IO (TVar Game)
transformGame (EventKey (Char '1') Down _ _)  gameStateTVar = do
  atomically $ writeTVar gameStateTVar $ newEasyGame
  return gameStateTVar

transformGame (EventKey (Char '2') Down _ _)  gameStateTVar = do
  atomically $ writeTVar gameStateTVar $ newMediumGame
  return gameStateTVar

transformGame (EventKey (Char '3') Down _ _)  gameStateTVar = do
  atomically $ writeTVar gameStateTVar $ newHardGame
  return gameStateTVar

transformGame (EventKey (Char key) state _ _)  gameStateTVar = do
  gameState <- atomically(readTVar gameStateTVar)
  let newGameState = updatePressedKeys gameState state key in
    atomically $ writeTVar gameStateTVar $ newGameState
  return gameStateTVar

transformGame _ gameStateTVar = do 
  return gameStateTVar

gameLogic :: TVar Game -> IO()
gameLogic gameStateTVar = do
  gameState <- atomically(readTVar gameStateTVar)
  let newGameState = iterateLogic gameState in
    atomically $ writeTVar gameStateTVar $ newGameState

iterateLogic :: Game -> Game
iterateLogic gameState =
  Game {
  ball = ball,
  conditions = Conditions started (didLose ball) (didWin bricks),
  paddle = processInput paddle keys,
  keys = keys,
  bricks = bricks
  }
  where
    Game ball conditions paddle keys bricks = collide gameState
    Conditions started lost win = conditions

collide :: Game -> Game
collide gameState = 
  let ballWallCollided = collideWalls ball
      ballPaddleCollided = collidePaddle ballWallCollided paddle
      collidedBricks@(newBall, newBricks) = collideBricks ballPaddleCollided bricks in
        Game { 
          ball = newBall,
          conditions = conditions,
          paddle = paddle,
          keys = keys,     
          bricks = newBricks
      }
  where
    Game ball conditions paddle keys bricks = gameState

collideBricks :: Ball -> [Vector2] -> (Ball, [Vector2])
collideBricks ball (brick:bricks)
  | didCollideBrick ball brick = (collidedBall, bricks)
  | otherwise = (tailBall, brick:tailBricks)
  where 
    collidedBall = reboundDirection ball brick
    (tailBall, tailBricks) = collideBricks ball bricks
collideBricks ball [] = (ball,[])


didCollideBrick :: Ball -> Vector2 -> Bool
didCollideBrick ball brick
  | brickMinX < ballMaxX && brickMaxX > ballMinX &&
    brickMinY < ballMaxY && brickMaxY > ballMinY = True
  | otherwise = False
    where
      (Vector2 ballPosX ballPosY, _) = ball
      (Vector2 brickPosX brickPosY) = brick
      (Vector2 brickMinX brickMinY, Vector2 brickMaxX brickMaxY) = bounds brick brickWidth brickHeight
      (Vector2 ballMinX ballMinY,Vector2 ballMaxX ballMaxY) = bounds (Vector2 ballPosX ballPosY) 10 10

reboundDirection :: Ball -> Vector2 -> Ball
reboundDirection ball brick
  | didCollideBrick ballX brick = (undoStep ball, Vector2 ballVelX (-ballVelY))
  | didCollideBrick ballY brick = (undoStep ball, Vector2 (-ballVelX) ballVelY)
  | otherwise                   = (undoStep ball, Vector2 (-ballVelX) (-ballVelY))
  where
    (Vector2 ballPosX ballPosY, Vector2 ballVelX ballVelY) = ball
    Vector2 brickPosX brickPosY = brick
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
  | ballPosY <= -310 && 
    ballPosY >= -320 && 
    ballPosX >= paddlePosX - paddleWidth/2 && 
    ballPosX <= paddlePosX + paddleWidth/2 = (Vector2 ballPosX ballPosY, Vector2 ballVelX (abs ballVelY))
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

didWin :: [Vector2] -> Bool
didWin bricks = length bricks == 0
    
update seconds gameState = do
  forkIO $ gameLogic gameState
  return gameState

main :: IO ()
main = do
  gameStatusTVar <- atomically(newTVar Game {
    ball = (Vector2 0 1000, Vector2 0 0),
    conditions = Conditions False False False,
    paddle = (0,0),
    keys = Set.empty,
    bricks = createBricks 1
  })
  renderBufferTVar <- atomically(newTVar (pictures[(rectangleSolid 0 0)]))

  playIO window backgroundColor 60 gameStatusTVar (gameAsPicture renderBufferTVar) transformGame update

