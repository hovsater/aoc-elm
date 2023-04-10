module Year2022.Day01 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)


reversedComparsion : comparable -> comparable -> Order
reversedComparsion a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


elfCalories : Input -> List Int
elfCalories input =
    String.split "\n\n" input
        |> List.map (List.sum << List.filterMap String.toInt << String.split "\n")


partOne : Input -> Answer
partOne input =
    IntAnswer (List.maximum (elfCalories input) |> Maybe.withDefault 0)


partTwo : Input -> Answer
partTwo input =
    IntAnswer (List.sum <| List.take 3 <| List.sortWith reversedComparsion (elfCalories input))


solve : Input -> Solution
solve input =
    ( partOne input, partTwo input )
