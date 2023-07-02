module Year2022.Day05 exposing (partOne, partTwo, solve)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Array exposing (Array)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompIf, chompWhile, getChompedString, int, keyword, loop, map, oneOf, succeed, symbol)


type alias Crate =
    String


type alias Stack =
    List Crate


type alias Move =
    { amount : Int, from : Int, to : Int }


parseStacks : Parser (List Stack)
parseStacks =
    let
        parseCrate : Parser (Maybe Crate)
        parseCrate =
            oneOf
                [ succeed Just |. symbol "[" |= (getChompedString <| chompWhile Char.isUpper) |. symbol "]"
                , succeed Nothing |. symbol "    "
                ]

        parseCrates : Parser (List (Maybe Crate))
        parseCrates =
            succeed (::)
                |= parseCrate
                |= loop []
                    (\crates ->
                        oneOf
                            [ parseCrate |> map (\crate -> Loop (crate :: crates))
                            , succeed (Loop crates) |. chompIf (\c -> c == ' ')
                            , succeed (Done <| List.reverse crates)
                            ]
                    )
    in
    loop []
        (\stacks ->
            oneOf
                [ parseCrates |> map (\crates -> Loop (crates :: stacks))
                , succeed (Loop stacks) |. chompIf (\c -> c == '\n')
                , succeed <| Done (List.reverse stacks |> List.transpose |> List.map (List.filterMap identity))
                ]
        )


parseMoves : Parser (List Move)
parseMoves =
    let
        parseMove : Parser Move
        parseMove =
            succeed (\amount from to -> Move amount (from - 1) (to - 1))
                |. symbol "move "
                |= int
                |. symbol " from "
                |= int
                |. symbol " to "
                |= int
    in
    loop []
        (\moves ->
            oneOf
                [ parseMove |> map (\move -> Loop (move :: moves))
                , succeed (Loop moves) |. chompIf (\c -> c == '\n')
                , succeed (Done <| List.reverse moves)
                ]
        )


parseInput : Input -> ( Array Stack, List Move )
parseInput input =
    case String.split "\n\n" input of
        [ stacks, moves ] ->
            ( Parser.run parseStacks stacks |> Result.withDefault [] |> Array.fromList
            , Parser.run parseMoves moves |> Result.withDefault []
            )

        _ ->
            ( Array.empty, [] )


moveOne : Move -> Array Stack -> Array Stack
moveOne { amount, from, to } stacks =
    let
        fromCrates : List Crate
        fromCrates =
            Array.get from stacks |> Maybe.withDefault []

        toCrates : List Crate
        toCrates =
            Array.get to stacks |> Maybe.withDefault []
    in
    stacks
        |> Array.set from (List.drop amount fromCrates)
        |> Array.set to (List.concat [ List.take amount fromCrates |> List.reverse, toCrates ])


moveMany : Move -> Array Stack -> Array Stack
moveMany { amount, from, to } stacks =
    let
        fromCrates : List Crate
        fromCrates =
            Array.get from stacks |> Maybe.withDefault []

        toCrates : List Crate
        toCrates =
            Array.get to stacks |> Maybe.withDefault []
    in
    stacks
        |> Array.set from (List.drop amount fromCrates)
        |> Array.set to (List.concat [ List.take amount fromCrates, toCrates ])


partOne : Input -> String
partOne input =
    let
        ( stacks, moves ) =
            parseInput input
    in
    List.foldl moveOne stacks moves
        |> Array.toList
        |> List.filterMap List.head
        |> String.join ""


partTwo : Input -> String
partTwo input =
    let
        ( stacks, moves ) =
            parseInput input
    in
    List.foldl moveMany stacks moves
        |> Array.toList
        |> List.filterMap List.head
        |> String.join ""


solve : Input -> Solution
solve input =
    ( partOne input |> StringAnswer
    , partTwo input |> StringAnswer
    )
