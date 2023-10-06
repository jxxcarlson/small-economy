module Main exposing (..)

import Model exposing (State)
import Playground exposing (..)
import Random
import Set


main =
    game view update (initialState Model.config)


initialState : Model.Config -> State
initialState config =
    Model.initialState config


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

        ( dx1, dx2 ) =
            ( 40, 48 )

        ( dy1, dy2 ) =
            ( 200, 90 )

        message1 : Shape
        message1 =
            words Playground.blue ("transactions " ++ String.fromInt state.t)
                |> moveY (-state.gridSize / 2 - 60)

        -- display computer red "transactions " (String.fromInt state.t) (271 - dx1) (270 - dy1) 83
        --words red ("transactions = " ++ String.fromInt state.t)
        --    |> moveX (computer.screen.width / 2 - 86 - 82 - dx)
        --    |> moveY (computer.screen.height / 2 - 20 - dy)
        message2 =
            display2 computer red "population " (state.populationSize |> String.fromInt |> String.padRight 4 ' ') (275 - dx1) (280 - dy1) 76

        message2a =
            display2 computer red "transaction " (state.transactionAmount |> Model.roundAt2 2) (275 - dx1) (300 - dy1) 75

        message2b =
            display2 computer red "init. Capital " (state.initialCapital |> Model.roundAt2 1) (273 - dx1) (320 - dy1) 75

        message3 =
            display2 computer red "max Capital " (Model.maxCapital state |> Model.roundAt2 1) (272 - dx1) (340 - dy1) 75

        message3b =
            display3b computer orange "$$ " "$/c" "%" (185 - dx1) (380 - dy1) 52

        message4 =
            displayData computer
                "quintile 5"
                quintiles.quintile5
                red
                (278 - dx1)
                (400 - dy1)
                83

        message5 =
            displayData computer
                "quintile 4"
                quintiles.quintile4
                red
                (278 - dx1)
                (420 - dy1)
                83

        message6 =
            displayData computer
                "quintile 3"
                quintiles.quintile3
                red
                (278 - dx1)
                (440 - dy1)
                83

        message7 =
            displayData computer
                "quintile 2"
                quintiles.quintile2
                red
                (278 - dx1)
                (460 - dy1)
                83

        message8 =
            displayData computer
                "quintile 1"
                quintiles.quintile1
                red
                (277 - dx1)
                (480 - dy1)
                83

        message9b =
            display2 computer orange "q5/q1" (state.q5toq1 |> Model.roundAt2 1) (288 - dx1) (510 - dy1) 88

        message9c =
            display2 computer orange "q5/q2" (state.q5toq2 |> Model.roundAt2 1) (288 - dx1) (530 - dy1) 88

        message9 =
            display2 computer orange "gini" (state.giniIndex |> Model.roundAt2 2) (296 - dx1) (550 - dy1) 98

        message9d =
            display2 computer orange "entropy" (state.entropy |> Model.roundAt2 2) (284 - dx1) (570 - dy1) 85

        messageC1 =
            words blue "Commands (press one key)"
                |> moveX (computer.screen.width / 2 - 70 - 62 - dx2)
                |> moveY (computer.screen.height / 2 - 320 - dy2)

        messageC2 =
            words blue "p: pause"
                |> moveX (computer.screen.width / 2 - 131 - 62 - dx2)
                |> moveY (computer.screen.height / 2 - 350 - dy2)

        messageC3 =
            words blue "r: run"
                |> moveX (computer.screen.width / 2 - 140 - 62 - dx2)
                |> moveY (computer.screen.height / 2 - 370 - dy2)

        messageC4 =
            words blue "x: reset"
                |> moveX (computer.screen.width / 2 - 136 - 62 - dx2)
                |> moveY (computer.screen.height / 2 - 390 - dy2)

        messageC5 =
            words blue "s: new seed"
                |> moveX (computer.screen.width / 2 - 122 - 62 - dx2)
                |> moveY (computer.screen.height / 2 - 410 - dy2)

        messageC6 =
            let
                c =
                    if state.transactionAmount == 0.5 then
                        red

                    else
                        blue
            in
            words c "a: set transaction amount to 0.5"
                |> moveX (computer.screen.width / 2 - 122 - dx2)
                |> moveY (computer.screen.height / 2 - 440 - dy2)

        messageC7 =
            let
                c =
                    if state.transactionAmount == 1.0 then
                        red

                    else
                        blue
            in
            words c "b: set transaction amount to 1.0"
                |> moveX (computer.screen.width / 2 - 122 - dx2)
                |> moveY (computer.screen.height / 2 - 460 - dy2)

        messageC8 =
            let
                c =
                    if state.transactionAmount == 1.5 then
                        red

                    else
                        blue
            in
            words c "c: set transaction amount to 1.5"
                |> moveX (computer.screen.width / 2 - 122 - dx2)
                |> moveY (computer.screen.height / 2 - 480 - dy2)

        messageC9 =
            let
                c =
                    if state.transactionAmount == 2.0 then
                        red

                    else
                        blue
            in
            words c "d: set transaction amount to 2.0"
                |> moveX (computer.screen.width / 2 - 122 - dx2)
                |> moveY (computer.screen.height / 2 - 500 - dy2)

        messageC10 =
            let
                c =
                    if state.taxRate == 0 then
                        red

                    else
                        blue
            in
            words c "n: no taxes"
                |> moveX (computer.screen.width / 2 - 188 - dx2)
                |> moveY (computer.screen.height / 2 - 530 - dy2)

        messageC11 =
            let
                c =
                    if state.taxRate == 0.04 then
                        red

                    else
                        blue
            in
            words c "e: tax rate  4%"
                |> moveX (computer.screen.width / 2 - 177 - dx2)
                |> moveY (computer.screen.height / 2 - 550 - dy2)

        messageC12 =
            let
                c =
                    if state.taxRate == 0.08 then
                        red

                    else
                        blue
            in
            words c "f: tax rate  8%"
                |> moveX (computer.screen.width / 2 - 177 - dx2)
                |> moveY (computer.screen.height / 2 - 570 - dy2)

        messageC13 =
            let
                c =
                    if state.taxRate == 0.12 then
                        red

                    else
                        blue
            in
            words c "g: tax rate 12%"
                |> moveX (computer.screen.width / 2 - 172 - dx2)
                |> moveY (computer.screen.height / 2 - 590 - dy2)

        messageC14 =
            let
                c =
                    if state.taxRate == 0.16 then
                        red

                    else
                        blue
            in
            words c "h: tax rate 16%"
                |> moveX (computer.screen.width / 2 - 172 - dx2)
                |> moveY (computer.screen.height / 2 - 610 - dy2)

        messageC15 =
            let
                c =
                    if state.taxRate == 0.2 then
                        red

                    else
                        blue
            in
            words c "i: tax rate 20%"
                |> moveX (computer.screen.width / 2 - 172 - dx2)
                |> moveY (computer.screen.height / 2 - 630 - dy2)

        quintiles =
            state.quintiles

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
        :: message3b
        :: message4
        :: message5
        :: message6
        :: message7
        :: message8
        :: message9
        --:: message9d
        :: message9b
        :: message9c
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
        :: messageC14
        :: messageC15
        :: displayLeft computer blue "Rules of the game" 0 0
        :: displayLeft computer blue "----------------------" 0 20
        :: displayLeft computer blue "200 people, each with $10" 23 40
        :: displayLeft computer blue "At each move, two players" 23 70
        :: displayLeft computer blue "are selected at random" 9 90
        :: displayLeft computer blue "Player A transfers x$" 4 120
        :: displayLeft computer blue "to player B unless he is broke." 34 140
        :: displayLeft computer blue "x is the transaction amount." 24 160
        :: displayLeft computer blue "Initial tax rate is 0%" 0 190
        :: displayLeft computer blue "Taxes are collected every" 15 210
        :: displayLeft computer blue "1000 moves. The tax revenue" 26 230
        :: displayLeft computer blue "is split evenly among all players." 40 250
        :: displayLeft computer blue "Gini coefficient is a measure of " 35 300
        :: displayLeft computer blue "inequality.  Gini = 0 is perfect " 28 320
        :: displayLeft computer blue "equality: everyone has the same " 36 340
        :: displayLeft computer blue "amount of money. Gini = 1 is perfect " 52 360
        :: displayLeft computer blue "inequality: one player has it all. " 34 380
        :: displayLeft2 computer blue "Gini coefficients (circa 2019)" 62 422
        :: displayLeft2 computer blue "-----------------------------------" 62 435
        :: displayLeft2 computer blue "U.S. 41.5" 0 510
        :: displayLeft2 computer blue "Germany 31.7" 14 570
        :: displayLeft2 computer blue "South Africa 63" 20 450
        :: displayLeft2 computer blue "Canada 33.3" 9 530
        :: displayLeft2 computer blue "Mexico 45.4" 9 490
        :: displayLeft2 computer blue "Brazil 48.9" 4 470
        :: displayLeft2 computer blue "Denmark 27.7" 14 610
        :: displayLeft2 computer blue "Japan 32.9" 4 550
        :: displayLeft2 computer blue "S. Korea 31.4" 12 590
        :: List.indexedMap (personToShape state.gridSize) state.people
        |> group
        |> moveY 10


displayLeft : Computer -> Color -> String -> Float -> Float -> Shape
displayLeft computer color str dx dy =
    [ words color str
        |> moveX (-computer.screen.width / 2 + 100 + dx)
        |> moveY (computer.screen.height / 2 - 135 - dy + gdy)
    ]
        |> group


gdx =
    -36


gdy =
    45


displayLeft2 : Computer -> Color -> String -> Float -> Float -> Shape
displayLeft2 computer color str dx dy =
    [ words color str
        |> moveX (-computer.screen.width / 2 + 100 + dx + gdx)
        |> moveY (computer.screen.height / 2 - 135 - dy + gdy)
    ]
        |> group


display2 computer color str1 str2 dx dy deltaX =
    [ words color str1
        |> moveX (computer.screen.width / 2 - dx)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str2
        |> moveX (computer.screen.width / 2 - dx + deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    ]
        |> group


display3 computer color str1 str2 str3 dx dy deltaX =
    [ words color str1
        |> moveX (computer.screen.width / 2 - dx)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str2
        |> moveX (computer.screen.width / 2 - dx + deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str3
        |> moveX (computer.screen.width / 2 - dx + 1.7 * deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    ]
        |> group


display3b computer color str1 str2 str3 dx dy deltaX =
    [ words color str1
        |> moveX (computer.screen.width / 2 - dx)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str2
        |> moveX (computer.screen.width / 2 - dx + deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str3
        |> moveX (computer.screen.width / 2 - dx + 1.82 * deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    ]
        |> group


display4 computer color str1 str2 str3 str4 dx dy deltaX =
    [ words color str1
        |> moveX (computer.screen.width / 2 - dx)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str2
        |> moveX (computer.screen.width / 2 - dx + deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str3
        |> moveX (computer.screen.width / 2 - dx + 1.7 * deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    , words color str4
        |> moveX (computer.screen.width / 2 - dx + 2.2 * deltaX)
        |> moveY (computer.screen.height / 2 - dy)
    ]
        |> group


displayData computer name data color dx dy deltaX =
    display4 computer
        color
        name
        (data.capital |> Model.roundAt 1 |> String.fromFloat)
        (data.averageCapital |> Model.roundAt 1 |> String.fromFloat)
        (100.0 * data.capital / 2000.0 |> Model.roundAt 1 |> String.fromFloat)
        dx
        dy
        deltaX


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

                    originalConfig =
                        Model.config

                    newState_ =
                        initialState { originalConfig | seedInteger = newSeedInteger }
                in
                { newState_ | paused = True }

            else if computer.keyboard.keys == Set.singleton "a" then
                setTransactionAmount state 0.5

            else if computer.keyboard.keys == Set.singleton "b" then
                setTransactionAmount state 1.0

            else if computer.keyboard.keys == Set.singleton "c" then
                setTransactionAmount state 1.5

            else if computer.keyboard.keys == Set.singleton "d" then
                setTransactionAmount state 2.0

            else if computer.keyboard.keys == Set.singleton "e" then
                setTaxRate state 0.04

            else if computer.keyboard.keys == Set.singleton "f" then
                setTaxRate state 0.08

            else if computer.keyboard.keys == Set.singleton "g" then
                setTaxRate state 0.12

            else if computer.keyboard.keys == Set.singleton "h" then
                setTaxRate state 0.16

            else if computer.keyboard.keys == Set.singleton "i" then
                setTaxRate state 0.2

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
    { state | transactionAmount = newTransactionAmount }


setTaxRate state rate =
    { state | taxRate = rate, ubi = True }


unSetTaxRate state =
    let
        state1 =
            { state | taxRate = 0 }
    in
    { state1 | ubi = False }
