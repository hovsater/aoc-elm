module Year2022.Day02 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Dict exposing (Dict)


type Shape
    = Rock
    | Paper
    | Scissors


partOne : Input -> Answer
partOne input =
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

        shapeScore : Shape -> Int
        shapeScore shape =
            case shape of
                Rock ->
                    1

                Paper ->
                    2

                Scissors ->
                    3

        flipShape : Shape -> Shape
        flipShape shape =
            case shape of
                Rock ->
                    Paper

                Paper ->
                    Scissors

                Scissors ->
                    Rock

        scoreRound : List Shape -> Int
        scoreRound shapes =
            case shapes of
                [ a, a ] ->
                    3 + shapeScore a

                [ a, b ] ->
                    if a == flipShape b then
                        shapeScore b

                    else
                        6 + shapeScore b
    in
    input
        |> String.split "\n"
        |> List.map (List.filterMap (\v -> Dict.get v dict) << String.split " ")
        |> List.fold scoreRound
        |> List.sum
        |> IntAnswer


partTwo : Input -> Answer
partTwo input =
    IntAnswer 0


solve : Input -> Solution
solve input =
    ( partOne input, partTwo input )
