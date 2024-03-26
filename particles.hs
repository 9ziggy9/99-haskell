import Graphics.Gloss
import System.Random (randomR, getStdGen, StdGen, Random)

rangeRand :: (Random a) => (a, a) -> StdGen -> (a, StdGen)
rangeRand range = randomR range

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

-- generalized coordinates
data Vec2 = Vec2 Float Float deriving (Show)

-- phase used to invoke idea of "phase space"
-- this data type holds a particles position and velocities
data Phase = Phase Vec2 Vec2 deriving (Show)

data ParticleState = ParticleState { phase  :: Phase
                                   , radius :: Float
                                   , mass   :: Float
                                   , clr    :: Color } deriving (Show)

data SimSettings = SimSettings { time   :: Float
                               , grav   :: Float
                               , lossY  :: Float
                               , lossX  :: Float
                               , sizeSq :: Float
                               , maxX   :: Float
                               , maxY   :: Float } deriving (Show)

updateVelocity :: SimSettings -> ParticleState -> ParticleState
updateVelocity settings particle =
  let dx = dx_dt * dt
      dy = dy_dt * dt
      vx = if collisionXBound (x + dx) r (maxX settings)
            then -(dx_dt * (lossX settings)) else dx_dt
      vy = if collisionYBound (y + dy) r (maxY settings)
            then -(dy_dt * (lossY settings)) else dy_dt + g * dt
  in particle { phase = Phase (Vec2 x y) (Vec2 vx vy) }
  where
    Phase (Vec2 x y) (Vec2 dx_dt dy_dt) = phase particle
    dt :: Float = time settings
    g  :: Float = grav settings
    r  :: Float = radius particle

updatePosition :: SimSettings -> ParticleState -> ParticleState
updatePosition settings particle =
  let dx = dx_dt * dt
      dy = dy_dt * dt
  in particle { phase = Phase (Vec2 (x + dx) (y + dy)) (Vec2 dx_dt dy_dt) }
  where
    dt :: Float = time settings
    Phase (Vec2 x y) (Vec2 dx_dt dy_dt) = phase particle

collisionXBound :: Float -> Float -> Float -> Bool
collisionXBound x rad maxX =
  x > (maxX / 2 - rad) || x < (rad - maxX / 2)

collisionYBound :: Float -> Float -> Float -> Bool
collisionYBound y rad maxY =
  y > (maxY / 2 - rad) || y < (rad - maxY / 2)

main :: IO ()
main = simulate window bgVoid 60 systemState0 model systemState
  where
    settings = SimSettings { time   = 1
                           , grav   = -1
                           , lossY  = 0.8
                           , lossX  = 0.6
                           , sizeSq = 20
                           , maxX   = 620
                           , maxY   = 480 }

    window = InWindow "haskell-particles" (floor (maxX settings),
                                           floor (maxY settings)) (0, 0)

    bgVoid = appColor "base03"

    bg = translate (sizeSq settings / 2) (sizeSq settings / 2) $
      pictureCheckerboard (appColor "base00") (appColor "base01")
      (sizeSq settings) (maxX settings, maxY settings)

    systemState0 :: [ParticleState]
    systemState0 = [
      ParticleState { phase = Phase (Vec2 0 0) (Vec2 5 0)
                    , radius = 10
                    , mass = 1
                    , clr = appColor "red" },
      ParticleState { phase = Phase (Vec2 0 20) (Vec2 3 10)
                    , radius = 15
                    , mass = 1
                    , clr = appColor "green" },
      ParticleState { phase = Phase (Vec2 (-10) 5) (Vec2 (-5) 5)
                    , radius = 25
                    , mass = 1
                    , clr = appColor "blue" }
      ]

    systemState _ _ substates =
      map (update settings) substates
      where update s = updatePosition s . updateVelocity s

    transition particle = translate x y $ pictureCircle c r
      where r :: Float = radius particle
            c :: Color = clr particle
            Phase (Vec2 x y) _ = phase particle

    model substates = pictures $ bg : map transition substates
