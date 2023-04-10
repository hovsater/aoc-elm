module Year2022.Day04 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Parser exposing ((|.), (|=), Parser, Step(..), int, loop, map, oneOf, run, spaces, succeed, symbol)


type alias Range =
    ( Int, Int )


type alias RangePair =
    ( Range, Range )


rangePairContainment : RangePair -> Bool
rangePairContainment ( ( a1, b1 ), ( a2, b2 ) ) =
    a1 >= a2 && b1 <= b2 || a2 >= a1 && b2 <= b1


rangePairOverlap : RangePair -> Bool
rangePairOverlap ( ( a1, b1 ), ( a2, b2 ) ) =
    a1 >= a2 && a1 <= b2 || a2 >= a1 && a2 <= b1


parseInput : String -> List RangePair
parseInput input =
    let
        parseRange : Parser Range
        parseRange =
            succeed Tuple.pair
                |= int
                |. symbol "-"
                |= int

        parseRangePair : Parser RangePair
        parseRangePair =
            succeed Tuple.pair
                |= parseRange
                |. symbol ","
                |= parseRange
                |. spaces

        parseRangePairs : Parser (List RangePair)
        parseRangePairs =
            loop []
                (\rangePairs ->
                    oneOf
                        [ parseRangePair |> map (\rangePair -> Loop (rangePair :: rangePairs))
                        , succeed <| Done (List.reverse rangePairs)
                        ]
                )
    in
    run parseRangePairs input |> Result.withDefault []


partOne : Input -> Answer
partOne input =
    input
        |> parseInput
        |> List.filter rangePairContainment
        |> List.length
        |> IntAnswer


partTwo : Input -> Answer
partTwo input =
    input
        |> parseInput
        |> List.filter rangePairOverlap
        |> List.length
        |> IntAnswer


solve : Input -> Solution
solve input =
    ( partOne input, partTwo input )
