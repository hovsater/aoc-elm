module Year2018.Day01 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Set exposing (Set)


parseInput : Input -> List Int
parseInput input =
    input |> String.lines |> List.filterMap String.toInt


partOne : Input -> Int
partOne input =
    input |> parseInput |> List.sum


partTwo : Input -> Int
partTwo input =
    let
        initialFrequencies : List Int
        initialFrequencies =
            parseInput input

        findRepeatingfrequency : Int -> Set Int -> List Int -> Int
        findRepeatingfrequency frequency seen frequencies =
            case frequencies of
                [] ->
                    findRepeatingfrequency frequency seen initialFrequencies

                h :: t ->
                    let
                        newFrequency : Int
                        newFrequency =
                            frequency + h
                    in
                    if Set.member newFrequency seen then
                        newFrequency

                    else
                        findRepeatingfrequency newFrequency (Set.insert newFrequency seen) t
    in
    findRepeatingfrequency 0 (Set.singleton 0) initialFrequencies


solve : Input -> Solution
solve input =
    ( partOne input |> IntAnswer
    , partTwo input |> IntAnswer
    )
