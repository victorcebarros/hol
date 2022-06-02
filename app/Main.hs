module Main where

type Red = Integer

type Green = Integer

type Blue = Integer

type Color = (Red, Green, Blue)

spread :: Float
spread = 20.0

redShift :: Float
redShift = 0.0

greenShift :: Float
greenShift = 2 * pi / 3

blueShift :: Float
blueShift = 4 * pi / 3

intensity :: Float -> Float -> Float -> Integer
intensity shift spread index =
  round (sin (index / spread + shift) * amplitude + shifting)
  where
    amplitude = 127.5
    shifting = 127.5

color :: Integer -> Color
color index = (red, green, blue)
  where
    red = intensity redShift spread (fromIntegral index :: Float)
    green = intensity greenShift spread (fromIntegral index :: Float)
    blue = intensity blueShift spread (fromIntegral index :: Float)

colorizeChar :: Color -> Char -> String
colorizeChar (red, green, blue) char =
  "\o33[38;2;" ++ show red ++ ";" ++
  show green ++ ";" ++ show blue ++
  "m" ++ [char] ++ "\o33[0m"

colorizeText :: String -> String
colorizeText string = aux string 0 0
  where
    aux [] _ _ = []
    aux (x:xs) width depth
      | x == '\n' = x : aux xs 0 (depth + 1)
      | otherwise =
        colorizeChar (color (width + depth)) x ++ aux xs (width + 1) depth

main :: IO ()
main = do
  input <- getContents
  putStr $ colorizeText input
