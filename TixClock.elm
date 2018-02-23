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
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type alias Square =
    { x : Int
    , y : Int
    , active : Bool
    }



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
        [ viewClock <| elements <| model
        , viewTime model
        , viewDebug model
        ]


hoursTensColumn : Model -> List Square
hoursTensColumn model =
    let
        cols =
            [ 20, 20, 20 ]

        rows =
            [ 20, 70, 120 ]

        active =
            [ False, True, False ]
    in
    List.map tupleToSquare <| zip cols rows active


hoursOnesColumn : Model -> List Square
hoursOnesColumn model =
    let
        cols =
            [ 100, 100, 100, 150, 150, 150, 200, 200, 200 ]

        rows =
            [ 20, 70, 120, 20, 70, 120, 20, 70, 120 ]

        active =
            [ False, True, True, False, True, False, False, True, False ]
    in
    List.map tupleToSquare <| zip cols rows active


minutesTensColumn : Model -> List Square
minutesTensColumn model =
    let
        cols =
            [ 280, 280, 280, 330, 330, 330 ]

        rows =
            [ 20, 70, 120, 20, 70, 120 ]

        active =
            [ False, False, False, True, False, False ]
    in
    List.map tupleToSquare <| zip cols rows active


minutesOnesColumn : Model -> List Square
minutesOnesColumn model =
    let
        cols =
            [ 410, 410, 410, 460, 460, 460, 510, 510, 510 ]

        rows =
            [ 20, 70, 120, 20, 70, 120, 20, 70, 120 ]

        active =
            [ False, True, False, True, False, True, False, True, False ]
    in
    List.map tupleToSquare <| zip cols rows active


zip : List Int -> List Int -> List Bool -> List ( Int, Int, Bool )
zip cols rows active =
    case ( cols, rows, active ) of
        ( c :: cTail, r :: rTail, a :: aTail ) ->
            ( c, r, a ) :: zip cTail rTail aTail

        ( _, _, _ ) ->
            []


tupleToSquare : ( Int, Int, Bool ) -> Square
tupleToSquare ( col, row, active ) =
    Square col row active


elements : Model -> List Square
elements model =
    List.concat
        [ hoursTensColumn model
        , hoursOnesColumn model
        , minutesTensColumn model
        , minutesOnesColumn model
        ]


viewClock : List Square -> Svg msg
viewClock squares =
    svg
        [ version "1.1"
        , baseProfile "full"
        , width (toString clockWidth)
        , height (toString clockHeight)
        ]
        (rect [ width "100%", height "100%", fill "#444" ] []
            :: List.map viewSquare squares
        )


viewSquare : Square -> Svg msg
viewSquare square =
    rect
        [ x (toString square.x)
        , y (toString square.y)
        , width (toString squareWidth)
        , height (toString squareWidth)
        , fill <| fillColor <| square.active
        ]
        []


fillColor : Bool -> String
fillColor isOn =
    if isOn then
        "#ccc"
    else
        "#888"


viewTime : Model -> Html msg
viewTime model =
    div
        []
        [ span [] [ hours model |> toZeroPaddedString |> text ]
        , span [] [ text " : " ]
        , span [] [ minutes model |> toZeroPaddedString |> text ]
        , span [] [ text " : " ]
        , span [] [ seconds model |> toZeroPaddedString |> text ]
        ]


toZeroPaddedString : Int -> String
toZeroPaddedString num =
    if num < 10 then
        "0" ++ toString num
    else
        toString num


viewDebug : Model -> Html msg
viewDebug model =
    div
        []
        [ pre
            []
            [ model |> toString |> text ]
        ]



-- UPDATE


type Msg
    = Tick Time
    | Increment Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

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
