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

type State = Play | Pause | Over

(gameWidth, gameHeight) = (600, 600)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)


type alias Mover a = { a | x : Float, y : Float, dx : Float, dy : Float }

type alias Player = Mover { heading : Float, accelerating : Bool }

type alias Asteroid = Mover {}

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
  , player = { x = 0, y = 0, heading = 0, accelerating = False, dx = 0, dy = 0 }
  , asteroids = []
  }

type alias Input =
  { delta : Time
  , pauseKey : Bool
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

flipState : State -> State
flipState s = if s == Play then Pause else Play

rotationSpeed = -0.3
accelerationCoefficient = 4 -- i.e. how much thrust engine applies

updatePlayer : Input -> Player -> Player
updatePlayer {delta, arrows} player =
  let newHeading = player.heading + rotationSpeed * (toFloat arrows.x)
      ddx = (cos newHeading) * accelerationCoefficient
      ddy = (sin newHeading) * accelerationCoefficient
  in
    physicsUpdate delta { player |
      heading = newHeading,
      dx = if player.accelerating then player.dx + ddx else player.dx,
      dy = if player.accelerating then player.dy + ddy else player.dy,
      accelerating = if arrows.y == 1 then True else False
    }

playerHasCollided : Game -> Bool
playerHasCollided ({asteroids, player} as game) =
  List.any (\a -> (abs (a.x - player.x) < 20) && (abs (a.y - player.y)) < 20) asteroids

update : Input -> Game -> Game
update ({delta, pauseKey, arrows} as input) ({player} as game) =
  case game.state of
    Over -> if pauseKey then {game | state = Pause} else game
    Pause -> if pauseKey then {game | state = Play} else game
    Play ->
      let newState = if playerHasCollided game then
                       Over
                     else
                       game.state
      -- Populate asteroids if empty; otherwise, simulate them
          newAsteroids = if game.asteroids == [] then
                           randomAsteroids (timeSeed delta)
                         else if game.state == Pause then
                                game.asteroids
                              else
                                List.map (physicsUpdate delta) game.asteroids
      -- Update the player
          newPlayer =
            updatePlayer input { player | accelerating = if arrows.y == 1 then True else False}
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


renderThrust : Player -> Maybe Form
renderThrust {x, y, heading, accelerating} =
  case accelerating of
    True -> Just (filled orange (polygon [(-10, 5), (-15, 0), (-10, -5)]))
    False -> Nothing

renderShip : Player -> Form
renderShip ({x, y, heading} as player) =
  let ship = filled white (polygon [(-10, -10), (15, 0), (-10, 10)])
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
                              "Paused"
                            else "Game Over")
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
