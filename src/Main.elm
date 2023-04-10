port module Main exposing (main)

import Aoc exposing (Day, Year, dayToInt, intToDay, intToYear, solve, yearToInt)
import Aoc.Problem exposing (Answer(..), Input, Solution)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Platform exposing (Program)


type alias Model =
    ()


type Msg
    = Solve (Maybe Year) (Maybe Day) Input
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Solve (Just year) (Just day) input ->
            ( (), sendSolution <| solutionEncoder year day <| solve year day input )

        _ ->
            ( model, Cmd.none )


port requestSolve : (E.Value -> msg) -> Sub msg


solveRequestDecoder : Decoder Msg
solveRequestDecoder =
    let
        maybeDecodeYear : Decoder (Maybe Year)
        maybeDecodeYear =
            D.field "year" D.int |> D.map intToYear

        maybeDecodeDay : Decoder (Maybe Day)
        maybeDecodeDay =
            D.field "day" D.int |> D.map intToDay
    in
    D.map3 Solve maybeDecodeYear maybeDecodeDay (D.field "input" D.string)


port sendSolution : E.Value -> Cmd msg


solutionEncoder : Year -> Day -> Solution -> E.Value
solutionEncoder year day ( partOne, partTwo ) =
    let
        encodeAnswer : Answer -> E.Value
        encodeAnswer answer =
            case answer of
                IntAnswer int ->
                    E.int int

                StringAnswer string ->
                    E.string string
    in
    E.object
        [ ( "year", E.int <| yearToInt year )
        , ( "day", E.int <| dayToInt day )
        , ( "part_one", encodeAnswer partOne )
        , ( "part_two", encodeAnswer partTwo )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    requestSolve <| Result.withDefault NoOp << D.decodeValue solveRequestDecoder


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
