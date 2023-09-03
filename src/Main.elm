module Main exposing(main)

import Playground exposing (..)
import Random



main =
  game view update initialState

view computer state =
  visualize computer state

visualize : Computer -> State -> List Shape
visualize computer state =
    let
         message : Shape
         message = words red ("t = " ++ String.fromInt state.t)
            |> moveX (computer.screen.width / 2 - 50)
            |> moveY (computer.screen.height/2 - 20)

     in
        rectangle black computer.screen.width computer.screen.height ::  message :: (List.map datumToShape state.data)

datumToShape : Datum -> Shape
datumToShape datum =
  let
    dx = scaleObject*datum.x
    dy = scaleObject*datum.y
    r = (datum.colorPhase * 255)
    b = (255 * (1 - datum.colorPhase))
    c = rgb r 0 b
    scaleObject = 4.5
  in
    (circle c 2.0) |> moveX dx |> moveY dy |> fade datum.alpha

update : Computer -> State ->  State
update computer state =
  case List.head state.data of
      Nothing -> state
      Just firstDatum ->
          let
            (delta, newSeed)  =
              newDelta state.seed
            x = firstDatum.x + delta.dx |> clamp computer.screen.left computer.screen.right
            y = firstDatum.y + delta.dy |> clamp computer.screen.bottom computer.screen.top
            colorPhase = firstDatum.colorPhase + delta.dColor |> clamp 0 1
            alpha = firstDatum.alpha + delta.dAlpha |> clamp 0 1

            newDatum : Datum
            newDatum =
              { x = x, y = y, colorPhase = colorPhase, alpha = alpha }

            _ = Debug.log "@(x_y_t)" (x, y, state.t + 1)
          in
           { state | seed = newSeed , data = newDatum :: state.data , t = state.t + 1 |> Debug.log "@T"}


type alias State = {
     seed : Random.Seed
   , data : List Datum
   , t : Int
   }


type ExtendedDatum = D Datum | S Shape

type alias Datum = {
     x : Float
   , y : Float
   , colorPhase : Float
   , alpha : Float
   }

initialDatum : Datum
initialDatum =
  { x = 0, y = 0, colorPhase = 0.5, alpha = 0.5 }

initialState : State
initialState = { seed = Random.initialSeed 1234, data = initialDatum::[], t = 0  }


-- HELPERS

wrap : Float -> Float -> Float -> Float -> Float
wrap epsilon lower upper x =
    if x > upper - epsilon then lower + epsilon
    else upper - epsilon

type alias Delta = { dx : Float, dy : Float, dColor : Float, dAlpha : Float }

newDelta : Random.Seed -> (Delta, Random.Seed)
newDelta seed0 =
  let
    (dx, seed1) = Random.step (Random.float -1 1) seed0
    (dy, seed2) = Random.step (Random.float -1 1) seed1
    (dColor, seed3) = Random.step (Random.float -0.01 0.01) seed2
    (dAlpha, seed4) = Random.step (Random.float -0.01 0.01) seed3
  in
    ( Delta dx dy dColor dAlpha, seed4 )
