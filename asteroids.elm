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
-- - More Collisions
-- - Make bullets not wrap
-- - Rate-limit shots
-- - Filter offscreen bullets
-- - Make bullets destroy asteroids
-- - Explosions (with physics!)
-- - Asteroid fragmentation

-- MODEL

type State = Play | Pause | Over

(gameWidth, gameHeight) = (600, 600)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)


type alias Mover a = { a | x : Float, y : Float, dx : Float, dy : Float }

type alias Player = Mover { heading : Float, accelerating : Bool }

type alias Asteroid = Mover {}

type alias Bullet = Mover { ttl : Float }

type alias Game =
  { state : State
  , player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  }

randomCoordinate : Generator (Float, Float)
randomCoordinate =
  Random.pair (Random.float -halfWidth halfWidth) (Random.float -halfHeight halfHeight)

randomVelocity : Generator (Float, Float)
randomVelocity = Random.pair (Random.float -200 200) (Random.float -200 200)

asteroid : (Float, Float) -> (Float, Float) -> Asteroid
asteroid (rx, ry) (rdx, rdy) = {x = rx, y = ry, dx = rdx, dy = rdy}

randomAsteroid : Generator Asteroid
randomAsteroid =
  Random.map2 asteroid randomCoordinate randomVelocity

timeSeed : Time -> Seed
timeSeed t = (Random.initialSeed << round) t

-- Create an empty game (paused, no asteroids)

newGame : Game
newGame =
  { state = Pause
  , player = { x = 0, y = 0, heading = 0, accelerating = False, dx = 0, dy = 0 }
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
physicsUpdate t obj =
  let dt = t
      newX = obj.x + dt * obj.dx
      newY = obj.y + dt * obj.dy
  in
    { obj |
        x = wrapScalar newX halfWidth,
        y = wrapScalar newY halfHeight
    }


randomAsteroids : Seed -> List Asteroid
randomAsteroids seed =
  let (result, _) = Random.generate (Random.list 5 randomAsteroid) seed
  in
    result

rotationSpeed = -0.3
accelerationCoefficient = 4 -- i.e. how much thrust engine applies

updatePlayer : Input -> Game -> Game
updatePlayer {delta, arrows} ({player} as game) =
  let newHeading = player.heading + rotationSpeed * (toFloat arrows.x)
      ddx = (cos newHeading) * accelerationCoefficient
      ddy = (sin newHeading) * accelerationCoefficient
      accelerating = if arrows.y == 1 then True else False
      setNewFields = (\player ->
                        { player |
                            heading = newHeading,
                            dx = if accelerating then
                                   player.dx + ddx
                                 else
                                   player.dx,
                            dy = if accelerating then
                                   player.dy + ddy
                                 else
                                   player.dy,
                            accelerating = accelerating
                        })
  in
    { game |
        player =  physicsUpdate delta (setNewFields player)
    }


bulletSpeed : Float
bulletSpeed = 300


newBullet : Game -> Bullet
newBullet {player} =
  { x = player.x
  , y = player.y
  , dx = bulletSpeed * cos player.heading
  , dy = bulletSpeed * sin player.heading
  , ttl = 1.5
  }


addBulletIfNeeded : Input -> Game -> Game
addBulletIfNeeded input game =
  if input.fireKey == True then
    { game | bullets = newBullet game :: game.bullets}
  else
    game


playerHasCollided : Game -> Bool
playerHasCollided ({asteroids, player} as game) =
  List.any (\a -> (abs (a.x - player.x) < 20) && (abs (a.y - player.y)) < 20) asteroids

updateState : Game -> Game
updateState game =
  { game | state =  if playerHasCollided game then Over else game.state }


updateAsteroids : Time -> Game -> Game
updateAsteroids delta game =
  { game |
      asteroids = if game.asteroids == [] then
                    randomAsteroids (timeSeed delta)
                  else if game.state == Pause then
                    game.asteroids
                  else
                    List.map (physicsUpdate delta) game.asteroids }


updateBullets : Time -> Game -> Game
updateBullets delta game =
  let decrementTtl = (\bullet -> { bullet | ttl = bullet.ttl - delta })
  in
    { game |
        bullets = List.map ((physicsUpdate delta) >> decrementTtl) game.bullets
                    |> List.filter (\bullet -> bullet.ttl > 0)
    }


update : Input -> Game -> Game
update ({delta, pauseKey, arrows} as input) ({player} as game) =
  case game.state of
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
  List.map (\b -> circle 3 |> filled Color.white |> move (b.x, b.y)) bullets

renderAsteroids : List Asteroid -> List Form
renderAsteroids asteroids =
  List.map (\a -> outlined (solid Color.white) (ngon 5 20) |> move (a.x, a.y)) asteroids


renderThrust : Player -> Maybe Form
renderThrust {x, y, heading, accelerating} =
  case accelerating of
    True -> Just (filled Color.orange (polygon [(-10, 5), (-15, 0), (-10, -5)]))
    False -> Nothing

renderShip : Player -> Form
renderShip ({x, y, heading} as player) =
  let ship = filled Color.white (polygon [(-10, -10), (15, 0), (-10, 10)])
      shipWithThrust = case renderThrust player of
                         Just thrust -> group [ship, thrust]
                         Nothing -> ship
  in
    shipWithThrust
      |> move (x, y)
      |> rotate heading

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
      , group (renderAsteroids game.asteroids)
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
