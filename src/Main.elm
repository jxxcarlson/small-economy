module Main exposing (..)

import Model exposing (State)
import Playground exposing (..)
import Random
import Set


main =
    game view update (initialState Model.config)


initialState : Model.Config -> State
initialState config =
    Model.initialState Model.config


view : Computer -> State -> List Shape
view computer state =
    blackScreen computer :: [ visualize computer state ]



-- CONFIGURATION


blackScreen computer =
    rectangle black computer.screen.width computer.screen.height


visualize : Computer -> State -> Shape
visualize computer state =
    let
        boundingBox =
            rectangle (rgb 30 30 60) (state.gridSize + 20) (state.gridSize + 20)

        dx =
            40

        dy =
            120

        message1 : Shape
        message1 =
            words red ("transactions = " ++ String.fromInt state.t)
                |> moveX (computer.screen.width / 2 - 86 - 73 - dx)
                |> moveY (computer.screen.height / 2 - 20 - dy)

        message2 =
            words red ("populaton = " ++ (state.populationSize |> toFloat |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 80 - 85 - dx)
                |> moveY (computer.screen.height / 2 - 50 - dy)

        message2a =
            words red ("transaction = $" ++ (state.transactionAmount |> Model.roundAt2 2))
                |> moveX (computer.screen.width / 2 - 80 - 85 - dx)
                |> moveY (computer.screen.height / 2 - 80 - dy)

        message2b =
            words red ("initial Capital = $" ++ (state.initialCapital |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 89 - 64 - dx)
                |> moveY (computer.screen.height / 2 - 100 - dy)

        message3 =
            words red ("max Capital = $" ++ (Model.maxCapital state |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 84 - 75 - dx)
                |> moveY (computer.screen.height / 2 - 120 - dy)

        message4 =
            words red ("quintile 5 = " ++ (quintiles.quintile5 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71 - 98 - dx)
                |> moveY (computer.screen.height / 2 - 150 - dy)

        message5 =
            words red ("quintile 4 = " ++ (quintiles.quintile4 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71 - 98 - dx)
                |> moveY (computer.screen.height / 2 - 170 - dy)

        message6 =
            words red ("quintile 3 = " ++ (quintiles.quintile3 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71 - 98 - dx)
                |> moveY (computer.screen.height / 2 - 190 - dy)

        message7 =
            words red ("quintile 2 = " ++ (quintiles.quintile2 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71 - 98 - dx)
                |> moveY (computer.screen.height / 2 - 210 - dy)

        message8 =
            words red ("quintile 1 = " ++ (quintiles.quintile1 |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 71 - 98 - dx)
                |> moveY (computer.screen.height / 2 - 230 - dy)

        message9 =
            words red ("max / quintile 1 = " ++ ((Model.maxCapital state / quintiles.quintile1) |> Model.roundAt2 1))
                |> moveX (computer.screen.width / 2 - 85 - 64 - dx)
                |> moveY (computer.screen.height / 2 - 260 - dy)

        messageC1 =
            words blue "Commands"
                |> moveX (computer.screen.width / 2 - 121 - 62 - dx)
                |> moveY (computer.screen.height / 2 - 320 - dy)

        messageC2 =
            words blue "p: pause"
                |> moveX (computer.screen.width / 2 - 131 - 62 - dx)
                |> moveY (computer.screen.height / 2 - 350 - dy)

        messageC3 =
            words blue "r: run"
                |> moveX (computer.screen.width / 2 - 140 - 62 - dx)
                |> moveY (computer.screen.height / 2 - 370 - dy)

        messageC4 =
            words blue "x: reset"
                |> moveX (computer.screen.width / 2 - 136 - 62 - dx)
                |> moveY (computer.screen.height / 2 - 390 - dy)

        messageC5 =
            words blue "s: new seed"
                |> moveX (computer.screen.width / 2 - 122 - 62 - dx)
                |> moveY (computer.screen.height / 2 - 410 - dy)

        messageC6 =
            let
                c =
                    if state.transactionAmount == 0.5 then
                        red

                    else
                        blue
            in
            words c "a: set transaction amount to 0.5"
                |> moveX (computer.screen.width / 2 - 122 - dx)
                |> moveY (computer.screen.height / 2 - 440 - dy)

        messageC7 =
            let
                c =
                    if state.transactionAmount == 1.0 then
                        red

                    else
                        blue
            in
            words c "b: set transaction amount to 1.0"
                |> moveX (computer.screen.width / 2 - 122 - dx)
                |> moveY (computer.screen.height / 2 - 460 - dy)

        messageC8 =
            let
                c =
                    if state.transactionAmount == 1.5 then
                        red

                    else
                        blue
            in
            words c "c: set transaction amount to 1.5"
                |> moveX (computer.screen.width / 2 - 122 - dx)
                |> moveY (computer.screen.height / 2 - 480 - dy)

        messageC9 =
            let
                c =
                    if state.transactionAmount == 2.0 then
                        red

                    else
                        blue
            in
            words c "d: set transaction amount to 2.0"
                |> moveX (computer.screen.width / 2 - 122 - dx)
                |> moveY (computer.screen.height / 2 - 500 - dy)

        messageC10 =
            let
                c =
                    if state.taxRate == 0 then
                        red

                    else
                        blue
            in
            words c "n: no taxes"
                |> moveX (computer.screen.width / 2 - 188 - dx)
                |> moveY (computer.screen.height / 2 - 530 - dy)

        messageC11 =
            let
                c =
                    if state.taxRate == 0.04 then
                        red

                    else
                        blue
            in
            words c "e: tax rate = 4%"
                |> moveX (computer.screen.width / 2 - 172 - dx)
                |> moveY (computer.screen.height / 2 - 550 - dy)

        messageC12 =
            let
                c =
                    if state.taxRate == 0.08 then
                        red

                    else
                        blue
            in
            words c "f: tax rate = 8%"
                |> moveX (computer.screen.width / 2 - 172 - dx)
                |> moveY (computer.screen.height / 2 - 570 - dy)

        messageC13 =
            let
                c =
                    if state.taxRate == 0.12 then
                        red

                    else
                        blue
            in
            words c "g: tax rate 12%"
                |> moveX (computer.screen.width / 2 - 172 - dx)
                |> moveY (computer.screen.height / 2 - 590 - dy)

        quintiles =
            Model.quintiles (state.people |> List.map .capital)

        message10 =
            words Playground.blue "Random Exchange Model"
                |> moveY (-state.gridSize / 2 - 40)
    in
    boundingBox
        :: message1
        :: message2
        :: message2a
        :: message2b
        :: message3
        :: message4
        :: message5
        :: message6
        :: message7
        :: message8
        :: message9
        :: message10
        :: messageC1
        :: messageC2
        :: messageC3
        :: messageC4
        :: messageC5
        :: messageC6
        :: messageC7
        :: messageC8
        :: messageC9
        :: messageC10
        :: messageC11
        :: messageC12
        :: messageC13
        :: List.indexedMap (personToShape state.gridSize) state.people
        |> group
        |> moveY 10


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
            max 1 (person.capital ^ 2.5 / 500)
    in
    circle c2 radius |> moveX dx |> moveY dy


update : Computer -> State -> State
update computer state =
    let
        newState =
            if computer.keyboard.keys == Set.singleton "p" then
                { state | paused = True }

            else if computer.keyboard.keys == Set.singleton "r" then
                { state | paused = False }

            else if computer.keyboard.keys == Set.singleton "x" then
                let
                    state1 =
                        initialState Model.config
                in
                { state1 | paused = True }

            else if computer.keyboard.keys == Set.singleton "s" then
                let
                    newSeedInteger =
                        state.seedInteger + 1

                    state1 =
                        let
                            originalConfig =
                                Model.config
                        in
                        initialState { originalConfig | seedInteger = newSeedInteger }
                in
                { state1 | paused = True }

            else if computer.keyboard.keys == Set.singleton "a" then
                setTransactionAmount state 0.5

            else if computer.keyboard.keys == Set.singleton "b" then
                setTransactionAmount state 1.0

            else if computer.keyboard.keys == Set.singleton "c" then
                setTransactionAmount state 1.5

            else if computer.keyboard.keys == Set.singleton "d" then
                setTransactionAmount state 2.0

            else if computer.keyboard.keys == Set.singleton "f" then
                setTaxRate state 0.04

            else if computer.keyboard.keys == Set.singleton "f" then
                setTaxRate state 0.08

            else if computer.keyboard.keys == Set.singleton "g" then
                setTaxRate state 0.12

            else if computer.keyboard.keys == Set.singleton "n" then
                unSetTaxRate state

            else
                state
    in
    if newState.paused then
        newState

    else
        Model.nextState newState


setTransactionAmount state newTransactionAmount =
    let
        state1 =
            { state | transactionAmount = newTransactionAmount }
    in
    { state1 | paused = True }


setTaxRate state rate =
    let
        state1 =
            { state | taxRate = rate }
    in
    { state1 | paused = True, ubi = True }


unSetTaxRate state =
    let
        state1 =
            { state | taxRate = 0 }
    in
    { state1 | paused = True, ubi = False }
