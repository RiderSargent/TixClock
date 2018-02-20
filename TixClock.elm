module TixClock exposing (..)

import Html exposing (Html, div, pre, span)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


clockHeight : Int
clockHeight =
    180


clockWidth : Int
clockWidth =
    570


squareWidth : Int
squareWidth =
    40


col1 : Int
col1 =
    20


col2 : Int
col2 =
    100


col3 : Int
col3 =
    150


col4 : Int
col4 =
    200


col5 : Int
col5 =
    280


col6 : Int
col6 =
    330


col7 : Int
col7 =
    410


col8 : Int
col8 =
    460


col9 : Int
col9 =
    510


row1 : Int
row1 =
    20


row2 : Int
row2 =
    70


row3 : Int
row3 =
    120


hours : Model -> Int
hours model =
    let
        totalHours =
            model.time
                |> Time.inHours
                |> truncate
    in
    rem totalHours 24


minutes : Model -> Int
minutes model =
    let
        totalMinutes =
            model.time
                |> Time.inMinutes
                |> truncate
    in
    rem totalMinutes 60


seconds : Model -> Int
seconds model =
    let
        totalSeconds =
            model.time
                |> Time.inSeconds
                |> truncate
    in
    rem totalSeconds 60



-- INIT


initialModel : Model
initialModel =
    { time = 0.0
    , count = 0
    , hoursTens = List.repeat 3 False
    , hoursOnes = List.repeat 9 False
    , minutesTens = List.repeat 6 False
    , minutesOnes = List.repeat 9 False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- MODEL


type alias Model =
    { time : Time
    , count : Int
    , hoursTens : List Bool
    , hoursOnes : List Bool
    , minutesTens : List Bool
    , minutesOnes : List Bool
    }



-- VIEW


view : Model -> Html msg
view model =
    div
        []
        [ svg
            [ version "1.1"
            , baseProfile "full"
            , width (toString clockWidth)
            , height (toString clockHeight)
            ]
            [ rect
                [ width "100%"
                , height "100%"
                , fill "black"
                ]
                []
            , square col1 row1 "grey"
            , square col1 row2 "grey"
            , square col1 row3 "grey"
            , square col2 row1 "grey"
            , square col2 row2 "grey"
            , square col2 row3 "grey"
            , square col3 row1 "grey"
            , square col3 row2 "grey"
            , square col3 row3 "grey"
            , square col4 row1 "grey"
            , square col4 row2 "grey"
            , square col4 row3 "grey"
            , square col5 row1 "grey"
            , square col5 row2 "grey"
            , square col5 row3 "grey"
            , square col6 row1 "grey"
            , square col6 row2 "grey"
            , square col6 row3 "grey"
            , square col7 row1 "grey"
            , square col7 row2 "grey"
            , square col7 row3 "grey"
            , square col8 row1 "grey"
            , square col8 row2 "grey"
            , square col8 row3 "grey"
            , square col9 row1 "grey"
            , square col9 row2 "grey"
            , square col9 row3 "grey"
            ]
        , div
            []
            [ span
                []
                [ hours model
                    |> toString
                    |> text
                ]
            , span
                []
                [ text " : " ]
            , span
                []
                [ minutes model
                    |> toZeroPaddedString
                    |> text
                ]
            , span
                []
                [ text " : " ]
            , span
                []
                [ seconds model
                    |> toZeroPaddedString
                    |> text
                ]
            ]
        , div
            []
            [ pre
                []
                [ model
                    |> toString
                    |> text
                ]
            ]
        ]


square : Int -> Int -> String -> Svg msg
square xCoord yCoord color =
    rect
        [ x (toString xCoord)
        , y (toString yCoord)
        , width (toString squareWidth)
        , height (toString squareWidth)
        , fill color
        ]
        []


toZeroPaddedString : Int -> String
toZeroPaddedString minutes =
    if minutes < 10 then
        "0" ++ toString minutes
    else
        toString minutes



-- UPDATE


type Msg
    = Tick Time
    | Increment Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        Increment newTime ->
            ( { model
                | time = newTime
                , count = model.count + 1
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        secondsPast =
            model.time
                |> Time.inSeconds
                |> truncate

        doIncrement =
            rem secondsPast 6 == 0
    in
    if doIncrement then
        Time.every second Increment
    else
        Time.every second Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
