module Main where

import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.UI.GLFW as GLFW
import Control.Monad (when)
import Control.Concurrent (threadDelay)

type Pos = Point
newtype Player = Player {position :: Pos}

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      Nothing -> pure ()
      Just win -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
    GLFW.terminate
  where
    simpleErrorCallback :: Error -> String -> IO ()
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

loop :: GLFW.Window -> Player -> State -> IO ()
loop win player glossState = do
  threadDelay (2 * 10^4)
  pollEvents
  k <- keyIsPressed win Key'Escape
  l <- keyIsPressed win Key'Left
  r <- keyIsPressed win Key'Right
  u <- keyIsPressed win Key'Up
  d <- keyIsPressed win Key'Down
  let newPlayer = movePlayer (l, r, u, d) player 10
  renderFrame newPlayer win glossState
  if k
    then pure ()
    else loop win newPlayer glossState

keyIsPressed :: GLFW.Window -> GLFW.Key -> IO Bool
keyIsPressed win key = isPress <$> GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

renderFrame :: Player -> GLFW.Window -> State -> IO ()
renderFrame (Player (x, y)) win glossState = do
  displayPicture (width, height) black glossState 1.0 $
    translate x y $ color orange $ rectangleSolid playerSize playerSize
  swapBuffers win

initialPlayer = Player (200, 200)
playerSize = 20

movePlayer dir p@(Player (x, y)) v
  | outsideBounds newPos = p
  | otherwise = Player newPos
  where
    newPos = move dir (x, y) v

outsideBounds (x, y) =
  x > fromIntegral width / 2 - playerSize / 2 ||
  x < (- fromIntegral width / 2 + playerSize / 2) ||
  y > fromIntegral height / 2 - playerSize / 2 ||
  y < (- fromIntegral height / 2 + playerSize / 2)

move (True, _, _, _) (x, y) v = (x - v, y)
move (_, True, _, _) (x, y) v = (x + v, y)
move (_, _, True, _) (x, y) v = (x, y + v)
move (_, _, _, True) (x, y) v = (x, y - v)
move _               p      _ = p

width = 640
height = 480

main :: IO ()
main = do
  glossState <- initState
  withWindow width height "Kabum" $ \win -> do
    loop win initialPlayer glossState
    pure ()
  putStrLn "hello world"
