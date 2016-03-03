import Random exposing (Seed, Generator)
import Time exposing (Time)
import Keyboard
import Text
import Graphics.Collage exposing (outlined, collage, ngon, rect, filled, move,
                                    Form, solid, group, toForm, polygon, rotate,
                                    circle)
import Graphics.Element as Element exposing (Element)
import Color
import Signal
import Window

-- TODO
-- - Refactor Mover to use {heading, velocity} in place of {dx, dy}
-- - Remove a bullet when it hits an asteroid
-- - Random new asteroids
-- - Explosions (with physics!)
-- - Better asteroid fragmentation/change of velocity for new small asteroids

-- MODEL

type State = Start | Play | Pause | Over


(gameWidth, gameHeight) = (600, 600)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)


type alias Mover a =
  { a |
      location : (Float, Float), -- (x, y)
      velocity: (Float, Float) -- (magnitude, direction)
  }


type alias Player =
  Mover { facing : Float, accelerating : Bool, cooldown : Int }


type AsteroidSize =
  Small | Medium | Large


type alias Asteroid =
  Mover { size : AsteroidSize }


type alias Bullet =
  Mover { ttl : Float }

type alias Game =
  { state : State
  , player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  }


rotationSpeed = -0.3
accelerationCoefficient = 40
bulletSpeed = 300


randomCoordinate : Generator (Float, Float)
randomCoordinate =
  Random.pair (Random.float -halfWidth halfWidth) (Random.float -halfHeight halfHeight)


randomVelocity : Generator (Float, Float)
randomVelocity =
  Random.pair (Random.float -200 200) (Random.map degrees <| Random.float 0 360)


randomSize : Generator AsteroidSize
randomSize =
  Random.map (\i -> case i of
        1 -> Small
        2 -> Medium
        _ -> Large) (Random.int 1 3)


asteroid : (Float, Float) -> (Float, Float) -> AsteroidSize -> Asteroid
asteroid randLoc randV randSize =
  {location = randLoc, velocity = randV, size = randSize}


randomAsteroid : Generator Asteroid
randomAsteroid =
  Random.map3 asteroid randomCoordinate randomVelocity randomSize


timeSeed : Time -> Seed
timeSeed t = (Random.initialSeed << round) t


newGame : Game
newGame =
  { state = Start
  , player = { location = (0, 0)
             , facing = 0
             , accelerating = False
             , velocity = (0, 0)
             , cooldown = 0 }
  , asteroids = []
  , bullets = []
  }


type alias Input =
  { delta : Time
  , pauseKey : Bool
  , fireKey : Bool
  , arrows : {x : Int, y : Int}
  }

-- UPDATE

wrapScalar : Float -> Float -> Float
wrapScalar x max =
  if x > max then
    -max
  else if x < -max then
    max
  else
    x


physicsUpdate : Time -> Mover a -> Mover a
physicsUpdate t ({location, velocity} as obj) =
  let dt = t
      (x, y) = location
      (r, theta) = velocity
      (dy, dx) = (r * sin theta, r * cos theta)
      newX = x + dt * dx
      newY = y + dt * dy
  in
    { obj |
        location = (wrapScalar newX halfWidth,
                    wrapScalar newY halfHeight)
    }


randomAsteroids : Seed -> List Asteroid
randomAsteroids seed =
  let (result, _) = Random.generate (Random.list 5 randomAsteroid) seed
  in
    result


addVelocities : (Float, Float) -> (Float, Float) -> (Float, Float)
addVelocities v1 v2 =
  let (x1, y1) = Debug.log "x1, y1" <| fromPolar v1
      (x2, y2) = Debug.log "x2, y2" <| fromPolar v2
  in
    toPolar (x1 + x2, y1 + y2)


updatePlayer : Input -> Game -> Game
updatePlayer {delta, arrows} ({player} as game) =
  let newFacing = player.facing + rotationSpeed * (toFloat arrows.x)
      deltaV = (delta * accelerationCoefficient, newFacing)
      accelerating = if arrows.y == 1 then True else False
      setNewFields = (\player ->
                        { player |
                            facing = newFacing,
                            velocity = if accelerating then
                                         addVelocities player.velocity deltaV
                                       else
                                         player.velocity,
                            accelerating = accelerating
                        })
  in
    { game |
        player =  physicsUpdate delta (setNewFields player)
    }


newBullet : Game -> Bullet
newBullet {player} =
  { location = player.location
  , velocity = (bulletSpeed, player.facing)
  , ttl = 1.5
  }


addBulletIfNeeded : Input -> Game -> Game
addBulletIfNeeded input ({player} as game) =
  let cooldown = game.player.cooldown
      decrementCooldown = (\player ->
                            if player.cooldown > 0 then
                              { player |
                                  cooldown = player.cooldown - 1
                              }
                            else
                              player)
  in
    if input.fireKey == True && cooldown == 0 then
      { game |
          bullets = newBullet game :: game.bullets,
          player = { player | cooldown = 5}
      }
    else
      { game | player = decrementCooldown game.player }


near : Float -> (Float, Float) -> (Float, Float) -> Bool
near radius (x1, y1) (x2, y2) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2) < radius


playerHasCollided : Game -> Bool
playerHasCollided ({asteroids, player} as game) =
  List.any (\a -> near 20 player.location a.location) asteroids

updateState : Game -> Game
updateState game =
  { game | state =  if playerHasCollided game then Over else game.state }


rotateAsteroidCourse : Float -> Asteroid -> Asteroid
rotateAsteroidCourse rotation ({velocity} as asteroid) =
  let (speed, angle) = velocity
      newAngle = angle + rotation
  in
    { asteroid |
        velocity = (speed, newAngle)
    }


fragmentAsteroid : Asteroid -> List Asteroid
fragmentAsteroid a =
  let childAsteroids = (\newSize ->
                        [rotateAsteroidCourse -15 { a | size = newSize},
                         rotateAsteroidCourse 1 {a | size = newSize}])
  in
    case a.size of
      Small -> []
      Medium -> childAsteroids Small
      Large -> childAsteroids Medium


asteroidIsShot : List Bullet -> Asteroid -> Bool
asteroidIsShot bullets asteroid =
  List.any (\bullet ->
              (near 20 asteroid.location bullet.location)) bullets


fragmentShotAsteroids : Game -> Game
fragmentShotAsteroids game =
  let collidedAsteroids = List.filter (asteroidIsShot game.bullets) game.asteroids
      uncollidedAsteroids = List.filter (not << (asteroidIsShot game.bullets)) game.asteroids
  in
    { game |
        asteroids = uncollidedAsteroids ++ List.concatMap fragmentAsteroid collidedAsteroids
    }


updateAsteroids : Time -> Game -> Game
updateAsteroids delta game =
  { game |
    asteroids = game.asteroids
                 |> List.map (physicsUpdate delta)
  } |> fragmentShotAsteroids


updateBullets : Time -> Game -> Game
updateBullets delta game =
  let decrementTtl = (\bullet -> { bullet | ttl = bullet.ttl - delta })
  in
    { game |
        bullets = game.bullets
                    |> (List.map <| (physicsUpdate delta >> decrementTtl))
                    |> List.filter (\bullet -> bullet.ttl > 0)
    }


update : Input -> Game -> Game
update ({delta, pauseKey, arrows} as input) ({player} as game) =
  case game.state of
    Start -> { game |
                 asteroids = randomAsteroids (timeSeed delta),
                 state = Pause
             }
    Over -> if pauseKey then newGame else game
    Pause -> if pauseKey then {game | state = Play} else game
    Play -> updateState game
         |> updateAsteroids delta
         |> updateBullets delta
         |> updatePlayer input
         |> addBulletIfNeeded input

-- VIEW

renderBullets : List Bullet -> List Form
renderBullets bullets =
  List.map (\b -> circle 3 |> filled Color.white |> move b.location) bullets


renderAsteroid : Asteroid -> Form
renderAsteroid asteroid =
  let size = case asteroid.size of
               Small -> 7
               Medium -> 14
               Large -> 20
  in
    outlined (solid Color.white) (ngon 5 size) |> move asteroid.location


renderThrust : Player -> Maybe Form
renderThrust {facing, accelerating} =
  case accelerating of
    True ->
      Just (group [(filled Color.orange (polygon [(-10, 5),
                                                  (-18, 0),
                                                  (-10, -5)])),
                   (filled Color.lightBlue (polygon [(-10, 5),
                                                     (-15, 0),
                                                     (-10, -5)]))])
    False ->
      Nothing


renderShip : Player -> Form
renderShip ({location, facing} as player) =
  let ship = filled Color.white (polygon [(-10, -10), (15, 0), (-10, 10)])
      shipWithThrust = case renderThrust player of
                         Just thrust -> group [ship, thrust]
                         Nothing -> ship
  in
    shipWithThrust
      |> move location
      |> rotate facing


view : (Int, Int) -> Game -> Element
view (w, h) game =
  let state = txt identity (if game.state == Play then
                              ""
                            else if game.state == Pause then
                              "Paused (ENTER to start)"
                            else "Game Over")
  in
    Element.container w h Element.middle <|
    collage gameWidth gameHeight
      -- Background
      [ rect gameWidth gameHeight |> filled Color.black
      -- Paused indicator
      , toForm state |> move (0, 0)
      -- Player's ship
      , renderShip game.player
      -- Asteroids
      , group (List.map renderAsteroid game.asteroids)
      , group (renderBullets game.bullets)]

txt f string =
  Text.fromString string
    |> Text.color Color.white
    |> Text.monospace
    |> f
    |> Element.leftAligned


-- SIGNALS

main = Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState = Signal.foldp update newGame input

delta = Signal.map Time.inSeconds (Time.fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      delta
      Keyboard.enter
      Keyboard.space
      Keyboard.arrows
