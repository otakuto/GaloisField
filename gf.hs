import System.Environment
import Codec.Picture
import Debug.Trace

g :: Int -> Int -> Int -> Int
g n x y = mod (x * y) n
main = do
  args <- getArgs
  let n = read $ args !! 0 :: Int
  let red x y = truncate (((fromIntegral (g n x y)) / fromIntegral n) * 255)
  let blue x y = 255 - (red x y)
  let render x y = PixelRGB8 (fromIntegral (red x y)) 0 (fromIntegral (blue x y))
  let rendertrace x y = if x == 0 then trace (show y) render x y else render x y
  writePng ("gf" ++ (show n) ++ ".png") $ generateImage rendertrace n n
