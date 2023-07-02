module Year2022.Day02 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Dict exposing (Dict)


type Shape
    = Rock
    | Paper
    | Scissors


flipShape : Shape -> Shape
flipShape shape =
    case shape of
        Rock ->
            Paper

        Paper ->
            Scissors

        Scissors ->
            Rock


type alias Round =
    ( Shape, Shape )


parseRounds : Input -> List Round
parseRounds input =
    let
        letterToShape : Dict String Shape
        letterToShape =
            Dict.fromList
                [ ( "A", Rock )
                , ( "B", Paper )
                , ( "C", Scissors )
                , ( "X", Rock )
                , ( "Y", Paper )
                , ( "Z", Scissors )
                ]

        lineToRound : String -> Maybe Round
        lineToRound line =
            case String.words line of
                [ a, b ] ->
                    Maybe.map2 Tuple.pair (Dict.get a letterToShape) (Dict.get b letterToShape)

                _ ->
                    Nothing
    in
    input |> String.lines |> List.filterMap lineToRound


scoreRound : Round -> Int
scoreRound ( a, b ) =
    let
        shapeScore : Shape -> Int
        shapeScore shape =
            case shape of
                Rock ->
                    1

                Paper ->
                    2

                Scissors ->
                    3
    in
    if a == b then
        3 + shapeScore b

    else if a == flipShape b then
        shapeScore b

    else
        6 + shapeScore b


partOne : Input -> Int
partOne input =
    input
        |> parseRounds
        |> List.foldl (\round sum -> sum + scoreRound round) 0


partTwo : Input -> Int
partTwo input =
    let
        transformRound : Round -> Round
        transformRound ( a, b ) =
            case b of
                Rock ->
                    ( a, flipShape <| flipShape a )

                Paper ->
                    ( a, a )

                Scissors ->
                    ( a, flipShape a )
    in
    input
        |> parseRounds
        |> List.foldl (\round sum -> sum + (scoreRound <| transformRound round)) 0


solve : Input -> Solution
solve input =
    ( partOne input |> IntAnswer
    , partTwo input |> IntAnswer
    )
