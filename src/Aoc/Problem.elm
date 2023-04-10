module Aoc.Problem exposing (Answer(..), Input, Solution)


type Answer
    = IntAnswer Int
    | StringAnswer String


type alias Input =
    String


type alias Solution =
    ( Answer, Answer )
