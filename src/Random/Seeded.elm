module Random.Seeded exposing (floatPairs, intPair, intPairs, ints, probabilities)

import Random


probabilities : Random.Seed -> Int -> { seed : Random.Seed, probs : List Float }
probabilities seed n =
    let
        result =
            probabilitiesHelp { n = n, seed = seed, probs = [] }
    in
    { seed = result.seed, probs = result.probs }


probabilitiesHelp : { n : Int, seed : Random.Seed, probs : List Float } -> { n : Int, seed : Random.Seed, probs : List Float }
probabilitiesHelp data =
    if data.n <= 0 then
        data

    else
        let
            ( p, seed ) =
                Random.step (Random.float 0 1) data.seed
        in
        probabilitiesHelp { n = data.n - 1, seed = seed, probs = p :: data.probs }


ints : Random.Seed -> Int -> Int -> Int -> { seed : Random.Seed, ints : List Int }
ints seed low high n =
    let
        result =
            intsHelp { low = low, high = high, n = n, seed = seed, ints = [] }
    in
    { seed = result.seed, ints = result.ints }


intsHelp : { low : Int, high : Int, n : Int, seed : Random.Seed, ints : List Int } -> { low : Int, high : Int, n : Int, seed : Random.Seed, ints : List Int }
intsHelp data =
    if data.n <= 0 then
        data

    else
        let
            ( k, seed ) =
                Random.step (Random.int data.low data.high) data.seed
        in
        intsHelp
            { n = data.n - 1
            , low = data.low
            , high = data.high
            , seed = seed
            , ints = k :: data.ints
            }


intPair : Random.Seed -> Int -> Int -> ( Random.Seed, ( Int, Int ) )
intPair seed low high =
    let
        ( k1, seed1 ) =
            Random.step (Random.int low high) seed

        ( k2, seed2 ) =
            Random.step (Random.int low high) seed1
    in
    ( seed2, ( k1, k2 ) )


intPairs : Random.Seed -> Int -> Int -> Int -> { seed : Random.Seed, pairs : List ( Int, Int ) }
intPairs seed low high n =
    let
        result =
            randomIntPairsHelp { low = low, high = high, n = n, seed = seed, pairs = [] }
    in
    { seed = result.seed, pairs = result.pairs }


randomIntPairsHelp : { low : Int, high : Int, n : Int, seed : Random.Seed, pairs : List ( Int, Int ) } -> { low : Int, high : Int, n : Int, seed : Random.Seed, pairs : List ( Int, Int ) }
randomIntPairsHelp data =
    if data.n <= 0 then
        data

    else
        let
            ( k1, seed1 ) =
                Random.step (Random.int data.low data.high) data.seed

            ( k2, seed2 ) =
                Random.step (Random.int data.low data.high) seed1
        in
        randomIntPairsHelp
            { n = data.n - 1
            , low = data.low
            , high = data.high
            , seed = seed2
            , pairs = ( k1, k2 ) :: data.pairs
            }


floatPairs : Random.Seed -> Float -> Float -> Int -> { seed : Random.Seed, pairs : List ( Float, Float ) }
floatPairs seed low high n =
    let
        result =
            randomFloatPairsHelp { low = low, high = high, n = n, seed = seed, pairs = [] }
    in
    { seed = result.seed, pairs = result.pairs }


randomFloatPairsHelp : { low : Float, high : Float, n : Int, seed : Random.Seed, pairs : List ( Float, Float ) } -> { low : Float, high : Float, n : Int, seed : Random.Seed, pairs : List ( Float, Float ) }
randomFloatPairsHelp data =
    if data.n <= 0 then
        data

    else
        let
            ( k1, seed1 ) =
                Random.step (Random.float data.low data.high) data.seed

            ( k2, seed2 ) =
                Random.step (Random.float data.low data.high) seed1
        in
        randomFloatPairsHelp
            { n = data.n - 1
            , low = data.low
            , high = data.high
            , seed = seed2
            , pairs = ( k1, k2 ) :: data.pairs
            }
