module Main exposing(main)

import Playground exposing (..)
import Random
import Set



main =
  game view update initialState

view computer state =
  visualize computer state

visualize : Computer -> State -> List Shape
visualize computer state =
    let
         blackScreen = rectangle black computer.screen.width computer.screen.height

         message : Shape
         message = words red ("t = " ++ String.fromInt state.t)
            |> moveX (computer.screen.width / 2 - 50)
            |> moveY (computer.screen.height/2 - 20)

         sun : Shape
         sun = circle yellow 30.0

     in
        blackScreen ::  sun :: message :: (List.map datumToShape state.data)

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
                if computer.keyboard.keys == Set.singleton "r" then
                   let
                       _ = Debug.log "(x,y)" (computer.mouse.x  , computer.mouse.y )
                   in
                   --{ x = computer.mouse.x/3 , y = -(computer.mouse.y /3), colorPhase = colorPhase, alpha = alpha }
                   { x = 0 , y = 0, colorPhase = 1, alpha = alpha }

                else if computer.keyboard.keys == Set.singleton "b" then
                    let
                        _ = Debug.log "(x,y)" (computer.mouse.x  , computer.mouse.y )
                    in
                    --{ x = computer.mouse.x/3 , y = -(computer.mouse.y /3), colorPhase = colorPhase, alpha = alpha }
                    { x = 0 , y = 0, colorPhase = 0, alpha = alpha }

                else if computer.keyboard.keys == Set.singleton "0" then
                    let
                        _ = Debug.log "(x,y)" (computer.mouse.x  , computer.mouse.y )
                    in
                    --{ x = computer.mouse.x/3 , y = -(computer.mouse.y /3), colorPhase = colorPhase, alpha = alpha }
                    { x = 0 , y = 0, colorPhase = 0.5, alpha = alpha }
                else
                  { x = x, y = y, colorPhase = colorPhase, alpha = alpha }
          in
           { state | seed = newSeed , data = newDatum :: state.data , t = state.t + 1 }


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
