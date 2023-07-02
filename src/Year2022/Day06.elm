module Year2022.Day06 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Set


findStartOfPacketMarker : Int -> Int -> String -> Maybe Int
findStartOfPacketMarker offset gap signal =
    if offset >= String.length signal then
        Nothing

    else if (String.slice offset (offset + gap) signal |> String.toList |> Set.fromList |> Set.size) == gap then
        Just (offset + gap)

    else
        findStartOfPacketMarker (offset + 1) gap signal


partOne : Input -> Int
partOne input =
    input
        |> findStartOfPacketMarker 0 4
        |> Maybe.withDefault 0


partTwo : Input -> Int
partTwo input =
    input
        |> findStartOfPacketMarker 0 14
        |> Maybe.withDefault 0


solve : Input -> Solution
solve input =
    ( partOne input |> IntAnswer
    , partTwo input |> IntAnswer
    )
