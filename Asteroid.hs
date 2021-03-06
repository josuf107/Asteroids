module Asteroid where

import Control.Monad.State
import Data.Monoid
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

data Turning = TurningLeft | TurningRight deriving (Show, Eq)

data Player = Player
    { playerEntity :: Entity
    , playerRotation :: Float
    , playerTurning :: Maybe Turning
    , playerBoost :: Bool
    , playerShoot :: Bool
    , playerLives :: Int
    } deriving (Show)

data Shot = Shot
    { shotEntity :: Entity
    , shotLife :: Seconds
    } deriving (Show, Eq)

data Asteroid = Asteroid
    { asteroidEntity :: Entity
    , asteroidSize :: Int
    } deriving (Show, Eq)

data Entity = Entity
    { entityPosition :: Point
    , entityDelta :: Vector
    } deriving (Show, Eq)

data Game = Game
    { gamePlayer :: Player
    , gameShots :: [Shot]
    , gameAsteroids :: [Asteroid]
    , gameOver :: Bool
    , gameGen :: StdGen
    } deriving (Show)

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
        draw (gamePlayer g)
        : fmap draw (gameShots g)
        ++ fmap draw (gameAsteroids g)

instance Draw Asteroid where
    draw a = color white
        . uncurry translate (entityPosition . asteroidEntity $ a)
        . circleSolid
        . fromIntegral
        . asteroidSize
        $ a

instance Draw Shot where
    draw s = color blue
        . uncurry translate (entityPosition . shotEntity $ s)
        . circleSolid
        $ 2

instance Draw Player where
    draw p =
        let (px, py) = entityPosition . playerEntity $ p
        in translate px py
            . rotate (radToDeg . normaliseAngle . negate . playerRotation $ p)
            . ifapp (playerBoost p)
                (\x -> pictures [x, color red . line $ [(0, 7), (0, negate 7)]])
            . color white
            . line
            $ [(0, 7), (0, negate 7), (14, 0), (0, 7)]

type Seconds = Float

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

updateGame :: Seconds -> Game -> Game
updateGame time g = ifxy (gameOver g)
    (initialGame . gameGen $ g) -- restart
    (checkCollisions $ g
        { gamePlayer = updatePlayer time (gamePlayer g)
        , gameShots = updateShots time (gamePlayer g) (gameShots g)
        , gameAsteroids = updateAsteroids time (gameAsteroids g)
        })

checkCollisions :: Game -> Game
checkCollisions g =
    let
        shotAsteroids = getShotAsteroids g
        crashedAsteroids = getCrashedAsteroids (gamePlayer g) (gameAsteroids g)
        asteroidsToSmash = fmap snd shotAsteroids ++ crashedAsteroids
        (newAsteroids, newGen) = flip runState (gameGen g)
            . mapM (\a -> ifxy (a `elem` asteroidsToSmash)
                (smash a)
                (return [a]))
            . gameAsteroids
            $ g
    in g
        { gameShots = filter (`notElem` (fmap fst shotAsteroids)) . gameShots $ g
        , gameAsteroids = concat newAsteroids
        , gameGen = newGen
        , gamePlayer = ifapp (not . null $ crashedAsteroids) kill . gamePlayer $ g
        }

kill :: Player -> Player
kill p = p
    { playerEntity = Entity (0, 0) (0, 0)
    , playerRotation = pi / 2
    , playerTurning = Nothing
    , playerBoost = False
    , playerShoot = False
    , playerLives = playerLives p - 1
    }

getCrashedAsteroids :: Player -> [Asteroid] -> [Asteroid]
getCrashedAsteroids p as =
    let
        (p1, p2, p3) = playerPoints p
        tooClose ap as = minimum
            [distFromLine p1 p2 ap, distFromLine p1 p3 ap, distFromLine p2 p3 ap]
            < (fromIntegral as)
    in
        filter (\a -> tooClose (entityPosition . asteroidEntity $ a) (asteroidSize a)) as

playerPoints :: Player -> (Point, Point, Point)
playerPoints p =
    let
        pnt = entityPosition . playerEntity $ p
        theta = playerRotation p
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
    case asteroidSize a of
        24 -> mapM randomAccelerate . replicate pieces . setSize 16 $ a
        16 -> mapM randomAccelerate . replicate pieces . setSize 8 $ a
        8 -> return [] -- completely destroyed
        x -> error ("Weirdly sized asteroid: " ++ show x)

setSize :: Int -> Asteroid -> Asteroid
setSize s a = a
    { asteroidSize = s
    , asteroidEntity = (asteroidEntity a) { entityDelta = (0, 0) }
    }

randomAccelerate :: RandomGen g => Asteroid -> State g Asteroid
randomAccelerate a = do
    speed <- nextRandom (50, 100)
    angle <- nextRandom (0, 2 * pi)
    return $ Asteroid (accelerate speed angle 10000 (asteroidEntity a)) (asteroidSize a)

getShotAsteroids :: Game -> [(Shot, Asteroid)]
getShotAsteroids g = do
    shots <- gameShots g
    asteroids <- gameAsteroids g
    filter collided . return $ (shots, asteroids)
    where
        collided (shot, asteroid) =
            (entityPosition . shotEntity $ shot)
            `dist` (entityPosition . asteroidEntity $ asteroid)
            < (fromIntegral (asteroidSize asteroid) + 2)

dist :: Point -> Point -> Float
dist (px1, py1) (px2, py2) = sqrt $ (px1 - px2) ^ 2 + (py1 - py2) ^ 2

updateAsteroids :: Seconds -> [Asteroid] -> [Asteroid]
updateAsteroids time = fmap (moveAsteroid time)

moveAsteroid :: Seconds -> Asteroid -> Asteroid
moveAsteroid time a = a { asteroidEntity = move time (asteroidEntity a) }

updateShots :: Seconds -> Player -> [Shot] -> [Shot]
updateShots time player
    = fmap (moveShot time) -- move shots
    . filter ((< 1) . shotLife) -- kill old shots
    . fmap (\s -> s { shotLife = shotLife s + time} ) -- age shots
    . ifapp (playerShoot player) (newShot player:) -- new shot if shooting

newShot :: Player -> Shot
newShot p = Shot (accelerate 400 (playerRotation p) 400 (playerEntity p)) 0

moveShot :: Seconds -> Shot -> Shot
moveShot time s = s { shotEntity = move time (shotEntity s) }

updatePlayer :: Seconds -> Player -> Player
updatePlayer time
    = stopShoot -- stop shooting (this may later take a "machine gun" flag)
    . movePlayer time -- move player
    . boostPlayer time -- apply boost
    . rotatePlayer time -- apply rotation

stopShoot :: Player -> Player
stopShoot p = p { playerShoot = False }

movePlayer :: Seconds -> Player -> Player
movePlayer time p = p { playerEntity = move time (playerEntity p) }

move :: Seconds -> Entity -> Entity
move time e = e { entityPosition = wrapPosition $
    entityPosition e `addVector` (mulSV time . entityDelta $ e) }

wrapPosition :: Point -> Point
wrapPosition (px, py) =
    ( ifxy (px > 500) (-500) (ifxy (px < -500) 500 px)
    , ifxy (py > 500) (-500) (ifxy (py < -500) 500 py)
    )

boostPlayer :: Seconds -> Player -> Player
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

rotatePlayer :: Seconds -> Player -> Player
rotatePlayer time p =
    case playerTurning p of
        Just TurningRight -> p { playerRotation = playerRotation p - (pi * time) }
        Just TurningLeft -> p { playerRotation = playerRotation p + (pi * time) }
        Nothing -> p
