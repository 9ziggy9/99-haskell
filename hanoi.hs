import Graphics.Gloss

main :: IO ()
main = display window background drawing
  where
    window = InWindow "Towers of Hanoi" (400, 300) (100, 100)
    background = white
    drawing = circle 80
