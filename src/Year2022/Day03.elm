module Year2022.Day03 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Set exposing (Set)


type alias Item =
    Char


itemPriority : Item -> Int
itemPriority itemType =
    if Char.isLower itemType then
        Char.toCode itemType - Char.toCode 'a' + 1

    else
        Char.toCode itemType - Char.toCode 'A' + itemPriority 'z' + 1


type alias Compartment =
    Set Item


type alias Rucksack =
    List Item


toCompartments : Rucksack -> ( Compartment, Compartment )
toCompartments rucksack =
    let
        middle : Int
        middle =
            List.length rucksack // 2
    in
    ( List.take middle rucksack, List.drop middle rucksack )
        |> Tuple.mapBoth Set.fromList Set.fromList


groupsOf : Int -> List a -> List (List a)
groupsOf n list =
    case list of
        [] ->
            []

        _ ->
            List.take n list :: groupsOf n (List.drop n list)


parseInput : String -> List Rucksack
parseInput input =
    input
        |> String.lines
        |> List.map String.toList


partOne : Input -> Int
partOne input =
    let
        findCommonItem : ( Compartment, Compartment ) -> Maybe Item
        findCommonItem ( c1, c2 ) =
            Set.intersect c1 c2 |> Set.toList |> List.head
    in
    input
        |> parseInput
        |> List.filterMap (toCompartments >> findCommonItem)
        |> List.map itemPriority
        |> List.sum


partTwo : Input -> Int
partTwo input =
    let
        findCommonItem : List Rucksack -> Maybe Item
        findCommonItem rucksacks =
            case List.map Set.fromList rucksacks of
                [ c1, c2, c3 ] ->
                    Set.intersect c1 c2 |> Set.intersect c3 |> Set.toList |> List.head

                _ ->
                    Nothing
    in
    input
        |> parseInput
        |> groupsOf 3
        |> List.filterMap findCommonItem
        |> List.map itemPriority
        |> List.sum


solve : Input -> Solution
solve input =
    ( partOne input |> IntAnswer
    , partTwo input |> IntAnswer
    )
