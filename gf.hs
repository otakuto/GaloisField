import System.Environment
import Codec.Picture

gf :: Int -> [[Int]]
gf n = map (\x -> (map (g n x) [0..(n-1)])) [0..(n-1)]

g :: Int -> Int -> Int -> Int
g n x y = mod (x * y) n
main = do
  args <- getArgs
  let n = read $ args !! 0 :: Int
  let red x y = truncate (((fromIntegral ((gf n) !! y !! x)) / fromIntegral n) * 255)
  let blue x y = 255 - (red x y)
  let render x y = PixelRGB8 (fromIntegral (red x y)) 0 (fromIntegral (blue x y))
  writePng ("gf" ++ (show n) ++ ".png") $ generateImage render n n
