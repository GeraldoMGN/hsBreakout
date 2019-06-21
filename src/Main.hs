module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.STM

window :: Display
window = InWindow "hsTetris" (600, 800) (10, 10)

backgroundColor, ballColor :: Color
backgroundColor = black
ballColor = white  

ball :: Float -> Picture
ball world = pictures [(uncurry translate (0,world) $ color ballColor $ circleSolid 10)]

gameAsPicture :: TVar Float -> IO Picture
gameAsPicture world = do
  return pics
  where
    pics = pictures[ball 0]

transformGame :: Event -> TVar Float -> IO (TVar Float )
transformGame _ game = do 
  temp <- atomically(readTVar game)
  return game

newThread :: TVar Float -> IO()
newThread n = do
  temp <- atomically(readTVar n)
  atomically(writeTVar n (temp + 1))
  threadDelay 10
  newThread n

update seconds int = return int

main :: IO ()
main = do
  temp <- atomically(newTVar 0)
  forkIO $ newThread temp
  playIO window backgroundColor 60 temp gameAsPicture transformGame update

