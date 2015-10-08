import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (watch)

-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  , stepFrame : Int
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


bob : Model
bob =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  , stepFrame = 0
  }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) bob =
  (Debug.watch "bob" bob)
    |> gravity dt
    |> jump keys
    |> walk keys
    |> physics dt


jump : Keys -> Model -> Model
jump keys bob =
  if keys.y > 0 && bob.vy == 0
    then { bob | vy <- 6.0 }
    else bob


gravity : Float -> Model -> Model
gravity dt bob =
  { bob |
      vy <- if bob.y > 0 then bob.vy - dt/3 else 0
  }


physics : Float -> Model -> Model
physics dt bob =
  { bob |
      x <- bob.x + dt * bob.vx,
      y <- max 0 (bob.y + dt * bob.vy)
  }


walk : Keys -> Model -> Model
walk keys bob =
  { bob |
      vx <- toFloat keys.x,
      stepFrame <- if | bob.stepFrame <= 10 -> bob.stepFrame + 1
                      | otherwise -> 0,
      dir <-
        if  | keys.x < 0 -> Left
            | keys.x > 0 -> Right
            | otherwise  -> bob.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') bob =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if  | bob.y  >  0 -> "jump"
          | bob.vx /= 0 -> "walk"
          | otherwise   -> "stand"

    dir =
      case bob.dir of
        Left -> "left"
        Right -> "right"

    step =
      if | bob.stepFrame >= 5 -> "left"
         | otherwise -> "right"

    src =
      case verb of
        "walk" -> "bob-"++ verb ++ "-" ++ dir ++ "-" ++ step ++ ".png"
        _      -> "bob-"++ verb ++ "-" ++ dir ++ ".png"

    bobImage =
      image 200 300 src

    groundY = 175 - h/2

    position =
      (bob.x, bob.y + groundY)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , bobImage
          |> toForm
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update bob input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
