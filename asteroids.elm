import Random exposing (Seed, Generator, initialSeed)
import Time exposing (..)
import Keyboard exposing (space)
import Text
import Graphics.Collage exposing (outlined, collage, ngon, rect, filled, move,
                                    Form, solid, group, toForm, polygon, rotate)
import Graphics.Element exposing (container, middle, Element, leftAligned)
import Color exposing (..)
import Signal exposing (constant)
import Window

-- TODO
-- - Enable ship rotation
-- - Enable ship movement
-- - Collisions
-- - Firing
-- - Asteroid fragmentation

-- MODEL

type State = Play | Pause

(gameWidth, gameHeight) = (600, 600)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

type alias Player =
  { x : Float
  , y : Float
  , heading : Float
  , v : Float
  }

type alias Asteroid =
-- all the same size for now
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  }

type alias Game =
  { state : State
  , player : Player
  , asteroids : List Asteroid
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
timeSeed t = (initialSeed << round) t

-- Create an empty game (paused, no asteroids)

newGame : Game
newGame =
  { state = Pause
  , player = { x = 0, y = 0, heading = 0, v = 0}
  , asteroids = []
  }

type alias Input =
  { delta : Time
  , pauseKey : Bool
  , arrows : {x : Int, y : Int}
  }

-- UPDATE
-- Currently, just move the asteroids

wrapScalar : Float -> Float -> Float
wrapScalar x max =
  if x > max then
    -max
  else if x < -max then
    max
  else
    x

updateAsteroid : Time -> Asteroid -> Asteroid
updateAsteroid t asteroid =
  let dt = t
      newX = asteroid.x + dt * asteroid.dx
      newY = asteroid.y + dt * asteroid.dy
  in
    { asteroid |
        x = wrapScalar newX halfWidth,
        y = wrapScalar newY halfHeight
    }

randomAsteroids : Seed -> List Asteroid
randomAsteroids seed =
  let (result, _) = Random.generate (Random.list 5 randomAsteroid) seed
  in
    result

flipState : State -> State
flipState s = if s == Play then Pause else Play

update : Input -> Game -> Game
update {delta, pauseKey, arrows} ({player} as game) =
  let newState = if pauseKey then Play else game.state
      newAsteroids =
        if game.asteroids == [] then
            randomAsteroids (timeSeed delta)
        else if game.state == Pause then
               game.asteroids
        else
          List.map (updateAsteroid delta) game.asteroids
      rotationSpeed = 0.3
      newPlayer = { player |
                      heading = player.heading + rotationSpeed * (toFloat arrows.x)}
  in
    { game |
        state = newState
    ,   asteroids = newAsteroids
    ,   player = newPlayer
    }

-- VIEW

renderAsteroids : List Asteroid -> List Form
renderAsteroids asteroids =
  List.map (\a -> outlined (solid white) (ngon 5 20) |> move (a.x, a.y)) asteroids

renderShip : Player -> Form
renderShip {x, y, heading} =
  filled white (polygon [(-10, -10), (0, 15), (10, -10)])
    |> move (x, y)
    |> rotate heading

view : (Int, Int) -> Game -> Element
view (w, h) game =
  let state = txt identity (if game.state == Play then "" else "Paused")
  in
    container w h middle <|
    collage gameWidth gameHeight
      -- Background
      [ rect gameWidth gameHeight |> filled black
      -- Paused indicator
      , toForm state |> move (0, 0)
      -- Player's ship
      , renderShip game.player
      -- Asteroids
      , group (renderAsteroids game.asteroids)]

txt f string =
  Text.fromString string
    |> Text.color white
    |> Text.monospace
    |> f
    |> leftAligned


-- SIGNALS

main = Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState = Signal.foldp update newGame input

delta = Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      delta
      Keyboard.enter
      Keyboard.arrows
