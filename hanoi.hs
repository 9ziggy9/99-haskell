import Graphics.Gloss

drawColoredCircle :: Color -> Float -> Picture
drawColoredCircle clr rad = color clr (circle rad)

hanoiDiskAtPos :: (Float, Float) -> Color -> Picture
hanoiDiskAtPos (x,y) clr = translate x y (color clr (rectangleSolid 80 10))

main :: IO ()
main = display window background drawing
  where
    window = InWindow "hanoi" (400, 300) (100, 100)
    background = black
    drawing = pictures [
      drawColoredCircle red 80,
      hanoiDiskAtPos (20, 20) green
      ]
