module Main exposing (..)

import Playground exposing (..)
import Random
import Set


main =
    game view update initialState


view computer state =
    visualize computer state


config =
    { deltaAlpha = 0.05
    , deltaColor = 0.05
    , deltaXY = 2.0
    , radius = 3.0
    }


visualize : Computer -> State -> List Shape
visualize computer state =
    let
        blackScreen =
            rectangle black computer.screen.width computer.screen.height

        message : Shape
        message =
            words red ("t = " ++ String.fromInt state.t)
                |> moveX (computer.screen.width / 2 - 50)
                |> moveY (computer.screen.height / 2 - 20)

        message2 : Shape
        message2 =
            words red ("(" ++ x ++ ", " ++ y ++ ")")
                |> moveX (computer.screen.width / 2 - 50)
                |> moveY (computer.screen.height / 2 - 40)

        x =
            Maybe.map .x (List.head state.data) |> Maybe.withDefault 0 |> round |> String.fromInt

        y =
            Maybe.map .y (List.head state.data) |> Maybe.withDefault 0 |> round |> String.fromInt

        sun : Shape
        sun =
            circle white 3.0
    in
    blackScreen :: sun :: message :: message2 :: List.indexedMap datumToShape state.data


datumToShape : Int -> Datum -> Shape
datumToShape index datum =
    let
        dx =
            scaleObject * datum.x

        dy =
            scaleObject * datum.y

        r =
            datum.colorPhase * 255

        b =
            255 * (1 - datum.colorPhase)

        c =
            if index == 0 then
                white

            else
                rgb r 0 b

        alpha_ =
            if index == 0 then
                1

            else
                datum.alpha

        scaleObject =
            4.5
    in
    circle c config.radius |> moveX dx |> moveY dy |> fade alpha_


update : Computer -> State -> State
update computer state =
    case List.head state.data of
        Nothing ->
            state

        Just firstDatum ->
            let
                ( delta, newSeed ) =
                    newDelta state.seed

                x =
                    --firstDatum.x + delta.dx |> bounce 2 20 computer.screen.left computer.screen.right
                    firstDatum.x + delta.dx |> bounce 2 20 -150 150

                y =
                    --firstDatum.y + delta.dy |> bounce 2 20 computer.screen.bottom computer.screen.top
                    firstDatum.y + delta.dy |> bounce 2 20 -100 100

                colorPhase =
                    firstDatum.colorPhase + delta.dColor |> bounce 0.01 0.05 0 1

                alpha =
                    --firstDatum.alpha + delta.dAlpha |> wrap 0.01 0 0.2
                    firstDatum.alpha + delta.dAlpha |> bounce 0.01 0.05 0.2 0.3

                newDatum : Datum
                newDatum =
                    if computer.keyboard.keys == Set.singleton "r" then
                        { x = 0, y = 0, colorPhase = 1, alpha = 1 }

                    else if computer.keyboard.keys == Set.singleton "b" then
                        { x = 0, y = 0, colorPhase = 0, alpha = 1 }

                    else
                        { x = x, y = y, colorPhase = colorPhase, alpha = alpha }
            in
            { state | seed = newSeed, data = newDatum :: state.data, t = state.t + 1 }


type alias State =
    { seed : Random.Seed
    , data : List Datum
    , t : Int
    }



-- HELPERS


wrap : Float -> Float -> Float -> Float -> Float
wrap epsilon lower upper x =
    if x > upper - epsilon then
        lower + epsilon

    else if x < lower + epsilon then
        upper - epsilon

    else
        x


bounce : Float -> Float -> Float -> Float -> Float -> Float
bounce epsilon dBounce lower upper x =
    if x > upper - epsilon then
        upper - dBounce

    else if x < lower + epsilon then
        lower + dBounce

    else
        x


type alias Delta =
    { dx : Float, dy : Float, dColor : Float, dAlpha : Float }


newDelta : Random.Seed -> ( Delta, Random.Seed )
newDelta seed0 =
    let
        ( dx, seed1 ) =
            Random.step (Random.float -config.deltaXY config.deltaXY) seed0

        ( dy, seed2 ) =
            Random.step (Random.float -config.deltaXY config.deltaXY) seed1

        ( dColor, seed3 ) =
            Random.step (Random.float -config.deltaColor config.deltaColor) seed2

        ( dAlpha, seed4 ) =
            Random.step (Random.float -config.deltaAlpha config.deltaAlpha) seed3
    in
    ( Delta dx dy dColor dAlpha, seed4 )
