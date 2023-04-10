module Aoc exposing (Day, Year, dayToInt, intToDay, intToYear, solve, yearToInt)

import Aoc.Problem exposing (Answer(..), Input, Solution)
import Year2022.Day01
import Year2022.Day02


type Year
    = Year2015
    | Year2016
    | Year2017
    | Year2018
    | Year2019
    | Year2020
    | Year2021
    | Year2022


yearToInt : Year -> Int
yearToInt year =
    case year of
        Year2015 ->
            2015

        Year2016 ->
            2016

        Year2017 ->
            2017

        Year2018 ->
            2018

        Year2019 ->
            2019

        Year2020 ->
            2020

        Year2021 ->
            2021

        Year2022 ->
            2022


intToYear : Int -> Maybe Year
intToYear int =
    case int of
        2015 ->
            Just Year2015

        2016 ->
            Just Year2016

        2017 ->
            Just Year2017

        2018 ->
            Just Year2018

        2019 ->
            Just Year2019

        2020 ->
            Just Year2020

        2021 ->
            Just Year2021

        2022 ->
            Just Year2022

        _ ->
            Nothing


type Day
    = Day01
    | Day02
    | Day03
    | Day04
    | Day05
    | Day06
    | Day07
    | Day08
    | Day09
    | Day10
    | Day11
    | Day12
    | Day13
    | Day14
    | Day15
    | Day16
    | Day17
    | Day18
    | Day19
    | Day20
    | Day21
    | Day22
    | Day23
    | Day24


dayToInt : Day -> Int
dayToInt day =
    case day of
        Day01 ->
            1

        Day02 ->
            2

        Day03 ->
            3

        Day04 ->
            4

        Day05 ->
            5

        Day06 ->
            6

        Day07 ->
            7

        Day08 ->
            8

        Day09 ->
            9

        Day10 ->
            10

        Day11 ->
            11

        Day12 ->
            12

        Day13 ->
            13

        Day14 ->
            14

        Day15 ->
            15

        Day16 ->
            16

        Day17 ->
            17

        Day18 ->
            18

        Day19 ->
            19

        Day20 ->
            20

        Day21 ->
            21

        Day22 ->
            22

        Day23 ->
            23

        Day24 ->
            24


intToDay : Int -> Maybe Day
intToDay int =
    case int of
        1 ->
            Just Day01

        2 ->
            Just Day02

        3 ->
            Just Day03

        4 ->
            Just Day04

        5 ->
            Just Day05

        6 ->
            Just Day06

        7 ->
            Just Day07

        8 ->
            Just Day08

        9 ->
            Just Day09

        10 ->
            Just Day10

        11 ->
            Just Day11

        12 ->
            Just Day12

        13 ->
            Just Day13

        14 ->
            Just Day14

        15 ->
            Just Day15

        16 ->
            Just Day16

        17 ->
            Just Day17

        18 ->
            Just Day18

        19 ->
            Just Day19

        20 ->
            Just Day20

        21 ->
            Just Day21

        22 ->
            Just Day22

        23 ->
            Just Day23

        24 ->
            Just Day24

        _ ->
            Nothing


solve : Year -> Day -> Input -> Solution
solve year day input =
    case ( year, day ) of
        ( Year2022, Day01 ) ->
            Year2022.Day01.solve input

        ( Year2022, Day02 ) ->
            Year2022.Day02.solve input

        _ ->
            ( StringAnswer "Not implemented", StringAnswer "Not implemented" )
