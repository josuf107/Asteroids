{-# LANGUAGE TemplateHaskell #-}

module Asteroid where

import Control.Monad.State
import Data.Monoid
import System.Random

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

data Turning = TurningLeft | TurningRight deriving (Show, Eq)

data Player = Player
    { _playerEntity :: Entity
    , _playerRotation :: Float
    , _playerTurning :: Maybe Turning
    , _playerBoost :: Bool
    , _playerShoot :: Bool
    , _playerLives :: Int
    } deriving (Show)

data Shot = Shot
    { _shotEntity :: Entity
    , _shotLife :: Seconds
    } deriving (Show, Eq)

data Asteroid = Asteroid
    { _asteroidEntity :: Entity
    , _asteroidSize :: Int
    } deriving (Show, Eq)

data Entity = Entity
    { _entityPosition :: Point
    , _entityDelta :: Vector
    } deriving (Show, Eq)

data Game = Game
    { _gamePlayer :: Player
    , _gameShots :: [Shot]
    , _gameAsteroids :: [Asteroid]
    , _gameOver :: Bool
    , _gameGen :: StdGen
    } deriving (Show)

type Seconds = Float

(makeLenses ''Player)
(makeLenses ''Shot)
(makeLenses ''Asteroid)
(makeLenses ''Entity)
(makeLenses ''Game)

class Keyable a where
    toKey :: a -> Key

instance Keyable SpecialKey where
    toKey = SpecialKey

instance Keyable Char where
    toKey = Char

type EventBinding a = [((Key, KeyState), a -> a)]

class Draw a where
    draw :: a -> Picture

instance Draw Game where
    draw g = pictures $
        draw (g^.gamePlayer)
        : fmap draw (g^.gameShots)
        ++ fmap draw (g^.gameAsteroids)

instance Draw Asteroid where
    draw a = color white
        . uncurry translate (a^.asteroidEntity.entityPosition)
        . circleSolid
        . fromIntegral
        $ a^.asteroidSize

instance Draw Shot where
    draw s = color blue
        . uncurry translate (s^.shotEntity.entityPosition)
        . circleSolid
        $ 2

instance Draw Player where
    draw p =
        let (px, py) = p^.playerEntity.entityPosition
        in translate px py
            . rotate (radToDeg . normaliseAngle . negate $ p^.playerRotation)
            . ifapp (p^.playerBoost)
                (\x -> pictures [x, color red . line $ [(0, 7), (0, negate 7)]])
            . color white
            . line
            $ [(0, 7), (0, negate 7), (14, 0), (0, 7)]

main :: IO ()
main = play
    (InWindow "Asteroids" (500, 500) (500, 500))
    black
    40
    (initialGame (mkStdGen 4))
    draw
    handleEvent
    updateGame

initialGame :: StdGen -> Game
initialGame g =
    let (as, g') = runState (newAsteroids 5) g
    in
        Game
        (Player (Entity (0,0) (0,0)) (pi / 2) Nothing False False 3)
        []
        as
        False
        g'

newAsteroids :: RandomGen g => Int -> State g [Asteroid]
newAsteroids n = sequence . replicate n $ newAsteroid

newAsteroid :: RandomGen g => State g Asteroid
newAsteroid = do
    px <- nextRandom (-500, 500)
    py <- nextRandom (-500, 500)
    speed <- nextRandom (50, 100)
    angle <- nextRandom (0, 2 * pi)
    return $ Asteroid (accelerate speed angle 10000 $ Entity (px, py) (0,0)) 24

nextRandom :: (RandomGen g, Random a) => (a, a) -> State g a
nextRandom range = state $ randomR range

ifxy :: Bool -> a -> a -> a
ifxy True a _ = a
ifxy _ _ b = b

ifapp :: Bool -> (a -> a) -> a -> a
ifapp True f = f
ifapp False _ = id

handleEvent :: Event -> Game -> Game
handleEvent = createHandler
    $ handlePlayer
        (key KeyLeft Down (playerTurning .~ Just TurningLeft)
        <> key KeyLeft Up
            (\p -> ifapp (p^.playerTurning == Just TurningLeft)
                (playerTurning .~ Nothing) p)
        <> key KeyRight Down (playerTurning .~ Just TurningRight)
        <> key KeyRight Up
            (\p -> ifapp (p^.playerTurning == Just TurningRight)
                (playerTurning .~ Nothing) p)
        <> key KeySpace Down (playerShoot .~ True)
        <> key KeySpace Up (playerShoot .~ False)
        <> key KeyUp Down (playerBoost .~ True)
        <> key KeyUp Up (playerBoost .~ False))
    <> key 'q' Down (gameOver .~ True)

handlePlayer :: EventBinding Player -> EventBinding Game
handlePlayer = fmap (\(k, fg) -> (k, gamePlayer %~ fg))

key :: Keyable k => k -> KeyState -> (a -> a) -> EventBinding a
key k s f = [((toKey k, s), f)]

createHandler :: EventBinding a -> Event -> a -> a
createHandler bs (EventKey k s _ _) =
    case lookup (k, s) bs of
        Nothing -> id
        Just f -> f
createHandler _ _ = id

updateGame :: Seconds -> Game -> Game
updateGame time g = ifxy (g^.gameOver)
    (initialGame $ g^.gameGen) -- restart
    (g
        & checkCollisions
        . (gamePlayer %~ updatePlayer time)
        . (gameShots %~ updateShots time (g^.gamePlayer))
        . (gameAsteroids %~ updateAsteroids time))

checkCollisions :: Game -> Game
checkCollisions g =
    let
        shotAsteroids = getShotAsteroids g
        crashedAsteroids = getCrashedAsteroids (g^.gamePlayer) (g^.gameAsteroids)
        asteroidsToSmash = fmap snd shotAsteroids ++ crashedAsteroids
        (newAsteroids, newGen) = flip runState (g^.gameGen)
            . mapM (\a -> ifxy (a `elem` asteroidsToSmash)
                (smash a)
                (return [a]))
            . (^.gameAsteroids)
            $ g
    in g
        & (gameShots %~ filter (`notElem` (fmap fst shotAsteroids)))
        . (gameAsteroids .~ concat newAsteroids)
        . (gameGen .~ newGen)
        . (gamePlayer %~ ifapp (not . null $ crashedAsteroids) kill)

kill :: Player -> Player
kill p = p
    & (playerEntity .~ (Entity (0, 0) (0, 0)))
    . (playerRotation .~ pi /2)
    . (playerTurning .~ Nothing)
    . (playerBoost .~ False)
    . (playerShoot .~ False)
    . (playerLives -~ 1)

getCrashedAsteroids :: Player -> [Asteroid] -> [Asteroid]
getCrashedAsteroids p as =
    let
        (p1, p2, p3) = playerPoints p
        tooClose ap as = minimum
            [distFromLine p1 p2 ap, distFromLine p1 p3 ap, distFromLine p2 p3 ap]
            < (fromIntegral as)
    in
        filter (\a -> tooClose (a^.asteroidEntity.entityPosition) (a^.asteroidSize)) as

playerPoints :: Player -> (Point, Point, Point)
playerPoints p =
    let
        pnt = p^.playerEntity.entityPosition
        theta = p^.playerRotation
    in
        ( pnt `addVector` rotateV theta (0, 7)
        , pnt `addVector` rotateV theta (0, negate 7)
        , pnt `addVector` rotateV theta (14, 0)
        )

distFromLine :: Point -> Point -> Point -> Float
distFromLine a b c =
    let
        ab = dist a b
        ac = dist a c
        bc = dist b c
        s = (ab + ac + bc) / 2 -- semiperimiter
    in
        ifxy (ab < ac || ab < bc) (min ac bc)
            ((2*sqrt(s*(s-ab)*(s-ac)*(s-bc)))/ab)
            -- such math
            -- https://en.wikipedia.org/wiki/Altitude_(triangle)#Altitude_in_terms_of_the_sides

smash :: RandomGen g => Asteroid -> State g [Asteroid]
smash a = nextRandom (1, 3) >>= \pieces ->
    case a^.asteroidSize of
        24 -> mapM randomAccelerate . replicate pieces . setSize 16 $ a
        16 -> mapM randomAccelerate . replicate pieces . setSize 8 $ a
        8 -> return [] -- completely destroyed
        x -> error ("Weirdly sized asteroid: " ++ show x)

setSize :: Int -> Asteroid -> Asteroid
setSize s a = a & (asteroidSize .~ s) . (asteroidEntity . entityDelta .~ (0, 0))

randomAccelerate :: RandomGen g => Asteroid -> State g Asteroid
randomAccelerate a = do
    speed <- nextRandom (50, 100)
    angle <- nextRandom (0, 2 * pi)
    return $ Asteroid (accelerate speed angle 10000 (a^.asteroidEntity)) (a^.asteroidSize)

getShotAsteroids :: Game -> [(Shot, Asteroid)]
getShotAsteroids g = do
    shots <- g^.gameShots
    asteroids <- g^.gameAsteroids
    filter collided . return $ (shots, asteroids)
    where
        collided (shot, asteroid) =
            (shot^.shotEntity.entityPosition)
            `dist` (asteroid^.asteroidEntity.entityPosition)
            < (fromIntegral (asteroid^.asteroidSize) + 2)

dist :: Point -> Point -> Float
dist (px1, py1) (px2, py2) = sqrt $ (px1 - px2) ^ 2 + (py1 - py2) ^ 2

updateAsteroids :: Seconds -> [Asteroid] -> [Asteroid]
updateAsteroids time = fmap (moveAsteroid time)

moveAsteroid :: Seconds -> Asteroid -> Asteroid
moveAsteroid time = over asteroidEntity (move time)

updateShots :: Seconds -> Player -> [Shot] -> [Shot]
updateShots time player
    = fmap (moveShot time) -- move shots
    . filter ((< 1) . (^.shotLife)) -- kill old shots
    . fmap (shotLife +~ time) -- age shots
    . ifapp (player^.playerShoot) (newShot player:) -- new shot if shooting

newShot :: Player -> Shot
newShot p = Shot (accelerate 400 (p^.playerRotation) 400 (p^.playerEntity)) 0

moveShot :: Seconds -> Shot -> Shot
moveShot time = over shotEntity (move time)

updatePlayer :: Seconds -> Player -> Player
updatePlayer time
    = stopShoot -- stop shooting (this may later take a "machine gun" flag)
    . movePlayer time -- move player
    . boostPlayer time -- apply boost
    . rotatePlayer time -- apply rotation

stopShoot :: Player -> Player
stopShoot = playerShoot .~ False

movePlayer :: Seconds -> Player -> Player
movePlayer time = over playerEntity (move time)

move :: Seconds -> Entity -> Entity
move time e = e & entityPosition .~
    (wrapPosition
        $ (e^.entityPosition) `addVector` (mulSV time $ e^.entityDelta))

wrapPosition :: Point -> Point
wrapPosition (px, py) =
    ( ifxy (px > 500) (-500) (ifxy (px < -500) 500 px)
    , ifxy (py > 500) (-500) (ifxy (py < -500) 500 py)
    )

boostPlayer :: Seconds -> Player -> Player
boostPlayer time p =
    ifxy (p^.playerBoost)
        (p & playerEntity .~ accelerate
            (time * boost)
            (p^.playerRotation)
            300
            (p^.playerEntity))
        p

boost :: Float
boost = 200

accelerate :: Float -> Float -> Float -> Entity -> Entity
accelerate magnitude radians maxmag = over entityDelta
    $ capVector maxmag
    . (`addVector` (mulSV magnitude
        . unitVectorAtAngle
        . normaliseAngle
        $ radians))

capVector :: Float -> Vector -> Vector
capVector maxmag v =
    if magV v > maxmag then
        mulSV maxmag . normaliseV $ v
    else v

addVector :: Vector -> Vector -> Vector
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

rotatePlayer :: Seconds -> Player -> Player
rotatePlayer time p =
    case p^.playerTurning of
        Just TurningRight -> p & playerRotation -~ (pi * time)
        Just TurningLeft -> p & playerRotation +~ (pi * time)
        Nothing -> p
