module Model exposing
    ( Config
    , Person
    , State
    , average
    , cdf
    , computeGini
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
    { seedInteger : Int
    , seed : Random.Seed
    , people : List Person
    , populationSize : Int
    , transactionAmount : Float
    , initialCapital : Float
    , gridSize : Float
    , quintiles : Quintiles
    , giniIndex : Float
    , q5toq2 : Float
    , q5toq1 : Float

    --
    , ubi : Bool
    , taxRate : Float
    , socialPayment : Float
    , taxationInterval : Int

    --
    , t : Int
    , paused : Bool
    }


type alias Quintiles =
    { quintile1 : Float, quintile2 : Float, quintile3 : Float, quintile4 : Float, quintile5 : Float }


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

        newPeople =
            if state.t > 1 && state.ubi && modBy state.taxationInterval state.t == 0 then
                runUBI state

            else
                updatePeople state.people i j

        newGiniIndex =
            computeGini (newPeople |> List.map .capital)

        nQ =
            quintiles (listCapital newPeople |> List.map Tuple.second)

        mu =
            0.01

        averageQuintiles =
            { quintile1 = mu * nQ.quintile1 + (1 - mu) * state.quintiles.quintile1
            , quintile2 = mu * nQ.quintile2 + (1 - mu) * state.quintiles.quintile2
            , quintile3 = mu * nQ.quintile3 + (1 - mu) * state.quintiles.quintile4
            , quintile4 = mu * nQ.quintile4 + (1 - mu) * state.quintiles.quintile4
            , quintile5 = mu * nQ.quintile5 + (1 - mu) * state.quintiles.quintile5
            }

        q5toq2 =
            mu * (averageQuintiles.quintile5 / averageQuintiles.quintile2) + (1 - mu) * state.q5toq2

        q5toq1 =
            mu * (averageQuintiles.quintile5 / averageQuintiles.quintile1) + (1 - mu) * state.q5toq1
    in
    { state
        | seed = seed
        , people = newPeople
        , quintiles = averageQuintiles
        , giniIndex = mu * newGiniIndex + (1 - mu) * state.giniIndex
        , q5toq2 = q5toq2
        , q5toq1 = q5toq1
        , t = state.t + 1
    }


runUBI : State -> List Person
runUBI state =
    let
        peopleAndTaxes : List ( Person, Float )
        peopleAndTaxes =
            List.map (taxPerson state.taxRate) state.people

        peopleAfterTaxes =
            List.map Tuple.first peopleAndTaxes

        taxRevenue =
            List.map Tuple.second peopleAndTaxes |> List.sum

        ubiPayment =
            taxRevenue / toFloat (List.length state.people)

        peopleAfterUBIPaid =
            List.map (\p -> { p | capital = p.capital + ubiPayment }) peopleAfterTaxes
    in
    peopleAfterUBIPaid


taxPerson : Float -> Person -> ( Person, Float )
taxPerson taxRate person =
    let
        taxAmount =
            person.capital * taxRate

        newPerson =
            { person | capital = person.capital - taxAmount }
    in
    ( newPerson, taxAmount )


initialState : Config -> State
initialState config_ =
    let
        ( newSeed, people ) =
            initPeople (Random.initialSeed config_.seedInteger) config_.populationSize config_.gridSize config_.initialCapital
    in
    { seedInteger = config_.seedInteger
    , seed = newSeed
    , people = people
    , populationSize = config_.populationSize
    , initialCapital = config_.initialCapital
    , transactionAmount = config_.transactionAmount
    , gridSize = config_.gridSize
    , quintiles = quintiles (listCapital people |> List.map Tuple.second)
    , giniIndex = computeGini (people |> List.map .capital)
    , q5toq2 = 1
    , q5toq1 = 1
    , t = 0
    , paused = False
    , ubi = config_.ubi
    , taxRate = config_.taxRate
    , socialPayment = config_.socialPayment
    , taxationInterval = config_.taxationInterval
    }


type alias Config =
    { seedInteger : Int
    , initialCapital : Float
    , populationSize : Int
    , initialSeed : Random.Seed
    , transactionAmount : Float
    , ubi : Bool
    , taxationInterval : Int
    , taxRate : Float
    , socialPayment : Float
    , gridSize : Float
    }


config : Config
config =
    { seedInteger = 1234
    , initialCapital = 10.0
    , populationSize = 200
    , initialSeed = Random.initialSeed 1234
    , transactionAmount = 2.0
    , ubi = False

    --
    , taxationInterval = 1000
    , taxRate = 0.0
    , socialPayment = 1.0

    --
    , gridSize = 500.0
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



-- ROUNDING


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


quintiles : List Float -> Quintiles
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


cdf : List Float -> Float -> Float
cdf ps x =
    let
        n =
            List.length ps |> toFloat

        k =
            List.filter (\p -> p <= x) ps |> List.length |> toFloat
    in
    k / n


computeGini : List Float -> Float
computeGini xs =
    let
        n =
            List.length xs

        sorted =
            List.sort xs

        numerator =
            2.0 * List.sum (List.indexedMap (\k x -> (toFloat k + 1.0) * x) sorted)

        denominator =
            toFloat n * List.sum sorted

        correction =
            (toFloat n + 1) / toFloat n
    in
    numerator / denominator - correction


average : List Float -> Float
average xs =
    List.sum xs / toFloat (List.length xs)
