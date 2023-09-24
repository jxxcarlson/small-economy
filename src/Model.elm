module Model exposing
    ( Person
    , State
    , average
    , config
    , initialState
    , listCapital
    , maxCapital
    , nextState
    , quintiles
    , roundAt
    , roundAt2
    , slice
    )

import List.Extra
import Maybe.Extra
import Random
import Random.Seeded


runN3 : Int -> State -> ( Maybe Float, Maybe Float, Maybe (List ( Int, Float )) )
runN3 n state =
    let
        capitalList =
            runN n state |> List.map (.people >> listCapital)

        totalCapital =
            capitalList |> List.map (List.map Tuple.second) |> List.map List.sum

        maxCapital3 =
            capitalList
                |> List.map (List.map Tuple.second)
                |> List.map List.maximum
                |> Maybe.Extra.values
    in
    ( List.head totalCapital
    , List.head maxCapital3
    , List.head capitalList
        |> Maybe.map (List.sortBy Tuple.second)
        |> Maybe.map List.reverse
    )


maxCapital : State -> Float
maxCapital state =
    let
        capitalList : List ( Int, Float )
        capitalList =
            state |> (.people >> listCapital)
    in
    capitalList
        |> List.map Tuple.second
        |> List.maximum
        |> Maybe.withDefault -1



-- runN2 : Int -> State -> ( List Float, List (List ( Int, Float )) )


runN : Int -> State -> List State
runN n state =
    let
        -- foldl : (a -> b -> b) -> b -> List a -> b
        update : ( State, List State ) -> ( State, List State )
        update ( state1, acc ) =
            ( nextState state1, state1 :: acc )

        folder : Int -> ( State, List State ) -> ( State, List State )
        folder k ( state1, acc ) =
            update ( state1, acc )

        ( last, rest ) =
            List.foldl folder ( state, [] ) (List.range 0 n)
    in
    last :: rest


type alias State =
    { seed : Random.Seed
    , people : List Person
    , transactionAmount : Float
    , t : Int
    }


nextState : State -> State
nextState state =
    let
        n =
            List.length state.people

        ( seed, ( i, j ) ) =
            Random.Seeded.intPair state.seed 0 (n - 1)

        updatePeople : List Person -> Int -> Int -> List Person
        updatePeople people ii jj =
            case ( List.Extra.getAt ii people, List.Extra.getAt jj people ) of
                ( Just personI, Just personJ ) ->
                    if ii /= jj && personI.capital - state.transactionAmount >= 0 then
                        let
                            newPersonI =
                                { personI | capital = personI.capital - state.transactionAmount }

                            newPersonJ =
                                { personJ | capital = personJ.capital + state.transactionAmount }
                        in
                        people |> List.Extra.setAt ii newPersonI |> List.Extra.setAt jj newPersonJ

                    else
                        people

                _ ->
                    people
    in
    { state
        | seed = seed
        , people = updatePeople state.people i j
        , t = state.t + 1
    }


initialState : Random.Seed -> Int -> Float -> Float -> State
initialState seed populationSize gridSize initialCapital =
    let
        ( newSeed, people ) =
            initPeople seed populationSize gridSize initialCapital
    in
    { seed = newSeed
    , people = people
    , transactionAmount = 0.5
    , t = 0
    }


type alias Config =
    { initialCapital : Float
    , initialPopulation : Int
    , initialSeed : Random.Seed
    }


config : Config
config =
    { initialCapital = 100.0
    , initialPopulation = 20
    , initialSeed = Random.initialSeed 1234
    }


type alias Person =
    { id : Int
    , x : Float
    , y : Float
    , capital : Float
    }


initPerson : Int -> ( Float, Float ) -> Float -> Person
initPerson id ( x, y ) capital =
    { id = id
    , x = x
    , y = y
    , capital = capital
    }


listCapital : List Person -> List ( Int, Float )
listCapital people =
    List.map (\p -> ( p.id, p.capital )) people


roundAt : Int -> Float -> Float
roundAt n f =
    let
        factor =
            10 ^ n |> toFloat
    in
    toFloat (round (f * factor)) / factor


roundAt2 : Int -> Float -> String
roundAt2 n f =
    let
        factor =
            10 ^ n |> toFloat

        intPart =
            truncate f |> toFloat

        fracPart =
            (f - intPart) * factor |> roundAt n |> truncate
    in
    String.fromFloat intPart ++ "." ++ String.fromInt fracPart


initPeople : Random.Seed -> Int -> Float -> Float -> ( Random.Seed, List Person )
initPeople seed populationSize gridSize capital =
    let
        result =
            Random.Seeded.floatPairs seed 0 gridSize populationSize
    in
    ( result.seed, initPeople_ capital result.pairs )


initPeople_ : Float -> List ( Float, Float ) -> List Person
initPeople_ capital positions =
    List.indexedMap (\k p -> initPerson k p capital) positions


slice : Float -> Float -> List Float -> List Float
slice fraction1 fraction2 xs =
    let
        sorted =
            List.sort xs

        n =
            List.length sorted

        m1 =
            fraction1 * toFloat n |> truncate

        m2 =
            fraction2 * toFloat n |> round
    in
    sorted |> List.take m2 |> List.drop m1


quintiles : List Float -> { quintile1 : Float, quintile2 : Float, quintile3 : Float, quintile4 : Float, quintile5 : Float }
quintiles xs =
    let
        sorted =
            List.sort xs

        n =
            List.length sorted

        m1 =
            0.2 * toFloat n |> truncate

        m2 =
            0.4 * toFloat n |> round

        m3 =
            0.6 * toFloat n |> round

        m4 =
            0.8 * toFloat n |> round
    in
    { quintile1 = sorted |> List.take m1 |> average
    , quintile2 = sorted |> List.drop m1 |> List.take (m2 - m1) |> average
    , quintile3 = sorted |> List.drop m2 |> List.take (m3 - m2) |> average
    , quintile4 = sorted |> List.drop m3 |> List.take (m4 - m3) |> average
    , quintile5 = sorted |> List.drop m4 |> List.take (n - m4) |> average
    }


average : List Float -> Float
average xs =
    List.sum xs / toFloat (List.length xs)
