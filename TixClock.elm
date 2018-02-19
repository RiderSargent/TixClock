module TixClock exposing (..)

import Html exposing (Html, div)
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



-- INIT


initialModel =
    { time = 0.0
    , count = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- MODEL


type alias Model =
    { time : Time
    , count : Int
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
            [ model
                |> toString
                |> text
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



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }