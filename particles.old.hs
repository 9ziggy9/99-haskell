import Graphics.Gloss

appColorPalette :: [(String, Color)]
appColorPalette =
  [ ("base03",  makeColorI 0 43 54 255)    -- #002b36
  , ("base02",  makeColorI 7 54 66 255)    -- #073642
  , ("base01",  makeColorI 88 110 117 255) -- #586e75
  , ("base00",  makeColorI 101 123 131 255) -- #657b83
  , ("base0",   makeColorI 131 148 150 255) -- #839496
  , ("base1",   makeColorI 147 161 161 255) -- #93a1a1
  , ("base2",   makeColorI 238 232 213 255) -- #eee8d5
  , ("base3",   makeColorI 253 246 227 255) -- #fdf6e3
  , ("yellow",  makeColorI 181 137 0 255)   -- #b58900
  , ("orange",  makeColorI 203 75 22 255)   -- #cb4b16
  , ("red",     makeColorI 220 50 47 255)   -- #dc322f
  , ("magenta", makeColorI 211 54 130 255)  -- #d33682
  , ("violet",  makeColorI 108 113 196 255) -- #6c71c4
  , ("blue",    makeColorI 38 139 210 255)  -- #268bd2
  , ("cyan",    makeColorI 42 161 152 255)  -- #2aa198
  , ("green",   makeColorI 133 153 0 255)   -- #859900
  ]
appColor :: String -> Color
appColor clr_name = case lookup clr_name appColorPalette of
  Just clr -> clr
  Nothing  -> makeColorI 101 123 131 255

translateUpperLeft :: (Float,Float) -> (Float, Float) -> Picture -> Picture
translateUpperLeft (maxX, maxY) (offX, offY) =
  translate (-(maxX / 2) + offX) (-( maxY / 2) + offY)


pictureBoundary :: Color -> (Float, Float) -> Picture
pictureBoundary clr (x,y) = translate x y (color clr (rectangleWire 618 478))

pictureCircle :: Color -> Float -> Picture
pictureCircle clr rad = color clr (circleSolid rad)

pictureCheckerboard :: Color -> Color -> Float -> (Float, Float) -> Picture
pictureCheckerboard clr1 clr2 size (maxX, maxY) =
  translate (-(maxX/2)) (-(maxY/2)) $ pictures $
  [ translate
    (x * size)
    (y * size)
    (color clr (rectangleSolid size size))
  | x <- [0..lastX - 1], y <- [0..lastY - 1],
    let clr = if even (floor x + floor y) then clr1 else clr2
  ]
  where
    lastX = maxX / size
    lastY = maxY / size

collisionXBound :: Float -> Float -> Float -> Bool
collisionXBound x rad maxX =
  x > (maxX / 2 - rad) || x < (rad - maxX / 2)

collisionYBound :: Float -> Float -> Float -> Bool
collisionYBound y rad maxY =
  y > (maxY / 2 - rad) || y < (rad - maxY / 2)

main :: IO ()
main = simulate window bgVoid 60 state0 model step
  where
    dt     :: Float = 1
    grav   :: Float = -1
    lossY   :: Float = 0.8
    lossX   :: Float = 0.6
    rad0   :: Float = 10
    sizeSq :: Float = 20
    (maxX, maxY) :: (Float, Float) = (620, 480)
    window = InWindow "haskell-particles" (floor maxX, floor maxY) (0, 0)

    bgVoid = appColor "base03"

    bg = translate (sizeSq/2) (sizeSq/2) $
      pictureCheckerboard (appColor "base00") (appColor "base01")
      sizeSq (maxX, maxY)

    state0 :: (Float, Float, Float, Float)
    state0 = (0, 0, 5, 20)

    step _ _ state =
      updatePosition $
      updateVelocity state

    updateVelocity (x, y, dx_dt, dy_dt) =
      let dx = dx_dt * dt
          dy = dy_dt * dt
          vx = if collisionXBound (x + dx) rad0 maxX
               then -(dx_dt * lossX) else dx_dt
          vy = if collisionYBound (y + dy) rad0 maxY
               then -(dy_dt * lossY) else dy_dt + grav * dt
      in (x, y, vx, vy)

    updatePosition (x, y, dx_dt, dy_dt) =
      let dx = dx_dt * dt
          dy = dy_dt * dt
      in (x + dx, y + dy, dx_dt, dy_dt)

    model (x, y, _, _) = pictures [
      bg,
      translate x y $ pictureCircle (appColor "blue") rad0
      ]
