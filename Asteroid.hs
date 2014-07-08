module Asteroid where

import Data.Monoid

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

data Turning = TurningLeft | TurningRight deriving Eq

data Player = Player
    { playerEntity :: Entity
    , playerRotation :: Float
    , playerTurning :: Maybe Turning
    , playerBoost :: Bool
    , playerShoot :: Bool
    }

data Shot = Shot
    { shotEntity :: Entity
    }

data Entity = Entity
    { entityPosition :: Point
    , entityDelta :: Vector
    }

data Game = Game
    { gamePlayer :: Player
    , gameShots :: [Shot]
    , gameOver :: Bool
    }

class Keyable a where
    toKey :: a -> Key

instance Keyable SpecialKey where
    toKey = SpecialKey

instance Keyable Char where
    toKey = Char

type EventBinding a = [((Key, KeyState), a -> a)]

main :: IO ()
main = play
    (InWindow "Asteroids" (500, 500) (500, 500))
    white
    40
    initialGame
    drawGame
    handleEvent
    updateGame

initialGame :: Game
initialGame = Game (Player (Entity (0,0) (0,0)) (pi / 2) Nothing False False) [] False

drawGame :: Game -> Picture
drawGame g = pictures $
    drawPlayer (gamePlayer g)
    : fmap drawShot (gameShots g)

drawShot :: Shot -> Picture
drawShot s = color blue
    . uncurry translate (entityPosition . shotEntity $ s)
    . circleSolid
    $ 2

drawPlayer :: Player -> Picture
drawPlayer p =
    let (px, py) = entityPosition . playerEntity $ p
    in translate px py
        . rotate (radToDeg . normaliseAngle . negate . playerRotation $ p)
        . ifxy (playerBoost p)
            (\x -> pictures [x, color red . line $ [(0, 7), (0, negate 7)]])
            id
        . line
        $ [(0, 7), (0, negate 7), (14, 0), (0, 7)]

ifxy :: Bool -> a -> a -> a
ifxy True a _ = a
ifxy _ _ b = b

handleEvent :: Event -> Game -> Game
handleEvent = createHandler
    $ handlePlayer
        (key KeyLeft Down
            (\p -> p { playerTurning = Just TurningLeft})
        <> key KeyLeft Up
            (\p -> ifxy (playerTurning p == Just TurningLeft)
                (p { playerTurning = Nothing }) p)
        <> key KeyRight Down
            (\p -> p { playerTurning = Just TurningRight })
        <> key KeyRight Up
            (\p -> ifxy (playerTurning p == Just TurningRight)
                (p { playerTurning = Nothing }) p)
        <> key KeySpace Down
            (\p -> p { playerShoot = True })
        <> key KeySpace Up
            (\p -> p { playerShoot = False })
        <> key KeyUp Down
            (\p -> p { playerBoost = True } )
        <> key KeyUp Up
            (\p -> p { playerBoost = False} ))
    <> key 'q' Down (\g -> g { gameOver = True })

handlePlayer :: EventBinding Player -> EventBinding Game
handlePlayer = fmap (\(k, fg) -> (k, modifyPlayer fg))

key :: Keyable k => k -> KeyState -> (a -> a) -> EventBinding a
key k s f = [((toKey k, s), f)]

createHandler :: EventBinding a -> Event -> a -> a
createHandler bs (EventKey k s _ _) =
    case lookup (k, s) bs of
        Nothing -> id
        Just f -> f
createHandler _ _ = id

modifyPlayer :: (Player -> Player) -> Game -> Game
modifyPlayer f g = g { gamePlayer = f (gamePlayer g) }

updateGame :: Float -> Game -> Game
updateGame time g = g
    { gamePlayer = updatePlayer time (gamePlayer g)
    , gameShots = updateShots time (gamePlayer g) (gameShots g)
    }

updateShots :: Float -> Player -> [Shot] -> [Shot]
updateShots time player shots
    = fmap (moveShot time) $ ifxy (playerShoot player) (newShot player:shots) shots

newShot :: Player -> Shot
newShot p = Shot $ accelerate 400 (playerRotation p) 400 (playerEntity p)

moveShot :: Float -> Shot -> Shot
moveShot time s = s { shotEntity = move time (shotEntity s) }

updatePlayer :: Float -> Player -> Player
updatePlayer time
    = stopShoot -- stop shooting (this may later take a "machine gun" flag)
    . movePlayer time -- move player
    . boostPlayer time -- apply boost
    . rotatePlayer time -- apply rotation

stopShoot :: Player -> Player
stopShoot p = p { playerShoot = False }

movePlayer :: Float -> Player -> Player
movePlayer time p = p { playerEntity = move time (playerEntity p) }

move :: Float -> Entity -> Entity
move time e = e { entityPosition =
    entityPosition e `addVector` (mulSV time . entityDelta $ e) }

boostPlayer :: Float -> Player -> Player
boostPlayer time p =
    ifxy (playerBoost p)
        (p { playerEntity = accelerate
            (time * boost)
            (playerRotation p)
            300
            (playerEntity p) })
        p

boost :: Float
boost = 200

accelerate :: Float -> Float -> Float -> Entity -> Entity
accelerate magnitude radians maxmag e =
    e { entityDelta = capVector maxmag
        (entityDelta e `addVector` (mulSV magnitude
        . unitVectorAtAngle
        . normaliseAngle
        $ radians)) }

capVector :: Float -> Vector -> Vector
capVector maxmag v =
    if magV v > maxmag then
        mulSV maxmag . normaliseV $ v
    else v

addVector :: Vector -> Vector -> Vector
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

rotatePlayer :: Float -> Player -> Player
rotatePlayer time p =
    case playerTurning p of
        Just TurningRight -> p { playerRotation = playerRotation p - (pi * time) }
        Just TurningLeft -> p { playerRotation = playerRotation p + (pi * time) }
        Nothing -> p
