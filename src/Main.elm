module Main exposing (..)

import Model exposing (State)
import Playground exposing (..)
import Random
import Set


main =
    game view update (initialState config1)


initialState : Config -> State
initialState config =
    Model.initialState (Random.initialSeed config.seed) config.populationSize config.gridSize config.initialCapital


view : Computer -> State -> List Shape
view computer state =
    visualize computer state



-- CONFIGURATION


type alias Config =
    { seed : Int
    , populationSize : Int
    , gridSize : Float
    , radius : Float
    , initialCapital : Float
    }


config1 =
    { seed = 12345
    , populationSize = 200
    , initialCapital = 20
    , gridSize = 500
    , radius = 1.0
    }


visualize : Computer -> State -> List Shape
visualize computer state =
    let
        blackScreen =
            rectangle black computer.screen.width computer.screen.height

        boundingBox =
            rectangle (rgb 30 30 60) (config1.gridSize + 20) (config1.gridSize + 20)

        message1 : Shape
        message1 =
            words red ("t = " ++ String.fromInt state.t)
                |> moveX (computer.screen.width / 2 - 50)
                |> moveY (computer.screen.height / 2 - 20)

        message2 =
            words red ("initial Capital = " ++ (config1.initialCapital |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 85)
                |> moveY (computer.screen.height / 2 - 50)

        message3 =
            words red ("max Capital = " ++ (Model.maxCapital state |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 80)
                |> moveY (computer.screen.height / 2 - 70)

        message4 =
            words red ("quintile 5 = " ++ (quintiles.quintile5 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71)
                |> moveY (computer.screen.height / 2 - 100)

        message5 =
            words red ("quintile 4 = " ++ (quintiles.quintile4 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71)
                |> moveY (computer.screen.height / 2 - 120)

        message6 =
            words red ("quintile 3 = " ++ (quintiles.quintile3 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71)
                |> moveY (computer.screen.height / 2 - 140)

        message7 =
            words red ("quintile 2 = " ++ (quintiles.quintile2 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71)
                |> moveY (computer.screen.height / 2 - 160)

        message8 =
            words red ("quintile 1 = " ++ (quintiles.quintile1 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71)
                |> moveY (computer.screen.height / 2 - 180)

        message9 =
            words red ("max / quintile 1 = " ++ ((Model.maxCapital state / quintiles.quintile1) |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 85)
                |> moveY (computer.screen.height / 2 - 210)

        quintiles =
            Model.quintiles (state.people |> List.map .capital)
    in
    blackScreen
        :: boundingBox
        :: message1
        :: message2
        :: message3
        :: message4
        :: message5
        :: message6
        :: message7
        :: message8
        :: message9
        :: List.indexedMap (personToShape config1.gridSize) state.people


personToShape : Float -> Int -> Model.Person -> Shape
personToShape gridSize index person =
    let
        dx =
            scaleObject * person.x - gridSize / 2

        dy =
            scaleObject * person.y - gridSize / 2

        c2 =
            Playground.blue

        scaleObject =
            1

        radius =
            -- person.capital ^ 3 / 3000
            person.capital ^ 2.5 / 1000
    in
    circle c2 radius |> moveX dx |> moveY dy


update : Computer -> State -> State
update computer state =
    Model.nextState state
