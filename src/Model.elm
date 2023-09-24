module Model exposing
    ( Person
    , State
    , config
    , initialState
    , listCapital
    , nextState
    , runN
    , runN2
    , runN3
    )

import List.Extra
import Maybe.Extra
import Random
import Random.Seeded


runN3 : Int -> State -> ( Maybe Int, Maybe Int, Maybe (List ( Int, Int )) )
runN3 n state =
    let
        capitalList =
            runN n state |> List.map (.people >> listCapital)

        totalCapital =
            capitalList |> List.map (List.map Tuple.second) |> List.map List.sum

        -- maxCapital : List (Maybe Int)
        maxCapital =
            capitalList
                |> List.map (List.map Tuple.second)
                |> List.map List.maximum
                |> Maybe.Extra.values
    in
    ( List.head totalCapital, List.head maxCapital, List.head capitalList )



-- runN2 : Int -> State -> ( List Float, List (List ( Int, Float )) )


runN2 n state =
    let
        capitalList =
            runN n state |> List.map (.people >> listCapital)

        totalCapital =
            capitalList |> List.map (List.map Tuple.second) |> List.map List.sum

        data =
            List.map2 (\x y -> ( x, y )) totalCapital capitalList
    in
    List.indexedMap (\k v -> Debug.log (String.fromInt k) v) (List.reverse data)



-- |> List.head


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
    , transactionAmount : Int
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
    { state | seed = seed, people = updatePeople state.people i j }


initialState : Random.Seed -> Int -> Float -> Int -> State
initialState seed populationSize gridSize initialCapital =
    let
        ( newSeed, people ) =
            initPeople seed populationSize gridSize initialCapital
    in
    { seed = newSeed
    , people = people
    , transactionAmount = 1
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
    , capital : Int
    }


initPerson : Int -> ( Float, Float ) -> Int -> Person
initPerson id ( x, y ) capital =
    { id = id
    , x = x
    , y = y
    , capital = capital
    }


listCapital : List Person -> List ( Int, Int )
listCapital people =
    List.map (\p -> ( p.id, p.capital )) people


roundAt : Int -> Float -> Float
roundAt n f =
    let
        factor =
            10 ^ n |> toFloat
    in
    toFloat (round (f * factor)) / factor


initPeople : Random.Seed -> Int -> Float -> Int -> ( Random.Seed, List Person )
initPeople seed populationSize gridSize capital =
    let
        result =
            Random.Seeded.floatPairs seed 0 gridSize populationSize
    in
    ( result.seed, initPeople_ capital result.pairs )


initPeople_ : Int -> List ( Float, Float ) -> List Person
initPeople_ capital positions =
    List.indexedMap (\k p -> initPerson k p capital) positions
