module TixClock exposing (..)

import Html exposing (Html, div, pre, span)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)


hours : Time -> Int
hours time =
    let
        totalHours =
            time
                |> Time.inHours
                |> truncate
    in
        rem totalHours 24


hoursTens : Time -> Int
hoursTens time =
    hours time // 10


hoursOnes : Time -> Int
hoursOnes time =
    rem (hours time) 10


minutes : Time -> Int
minutes time =
    let
        totalMinutes =
            time
                |> Time.inMinutes
                |> truncate
    in
        rem totalMinutes 60


minutesTens : Time -> Int
minutesTens time =
    minutes time // 10


minutesOnes : Time -> Int
minutesOnes time =
    rem (minutes time) 10


seconds : Time -> Int
seconds time =
    let
        totalSeconds =
            time
                |> Time.inSeconds
                |> truncate
    in
        rem totalSeconds 60



-- MODEL


initialModel : Model
initialModel =
    { time = 0.0
    , hoursTensList = []
    , hoursOnesList = []
    , minutesTensList = []
    , minutesOnesList = []
    }


type alias Model =
    { time : Time
    , hoursTensList : List Bool
    , hoursOnesList : List Bool
    , minutesTensList : List Bool
    , minutesOnesList : List Bool
    }


type alias Square =
    { x : Int
    , y : Int
    , active : Bool
    }



-- VIEW


view : Model -> Html msg
view model =
    div
        []
        [ viewClock model
        , viewTime model
        , viewDebug model
        ]


viewClock : Model -> Svg msg
viewClock model =
    let
        squareList =
            List.concat
                [ viewHoursTensSquares model.hoursTensList
                , viewHoursOnesSquares model.hoursOnesList
                , viewMinutesTensSquares model.minutesTensList
                , viewMinutesOnesSquares model.minutesOnesList
                ]
    in
        svg
            [ version "1.1"
            , baseProfile "full"
            , width "570"
            , height "180"
            ]
            (rect [ width "100%", height "100%", fill "#444" ] []
                :: List.map viewSquare squareList
            )


viewSquare : Square -> Svg msg
viewSquare square =
    rect
        [ x (toString square.x)
        , y (toString square.y)
        , width "40"
        , height "40"
        , square.active
            |> fillColor
            |> fill
        ]
        []


viewHoursTensSquares : List Bool -> List Square
viewHoursTensSquares hoursTensList =
    let
        cols =
            [ 20, 20, 20 ]

        rows =
            [ 20, 70, 120 ]
    in
        hoursTensList
            |> zip cols rows
            |> List.map tupleToSquare


viewHoursOnesSquares : List Bool -> List Square
viewHoursOnesSquares hoursOnesList =
    let
        cols =
            [ 100, 100, 100, 150, 150, 150, 200, 200, 200 ]

        rows =
            [ 20, 70, 120, 20, 70, 120, 20, 70, 120 ]
    in
        hoursOnesList
            |> zip cols rows
            |> List.map tupleToSquare


viewMinutesTensSquares : List Bool -> List Square
viewMinutesTensSquares minutesTensList =
    let
        cols =
            [ 280, 280, 280, 330, 330, 330 ]

        rows =
            [ 20, 70, 120, 20, 70, 120, 20, 70, 120 ]
    in
        minutesTensList
            |> zip cols rows
            |> List.map tupleToSquare


viewMinutesOnesSquares : List Bool -> List Square
viewMinutesOnesSquares minutesOnesList =
    let
        cols =
            [ 410, 410, 410, 460, 460, 460, 510, 510, 510 ]

        rows =
            [ 20, 70, 120, 20, 70, 120, 20, 70, 120 ]
    in
        minutesOnesList
            |> zip cols rows
            |> List.map tupleToSquare


asListOf : Int -> Int -> List Bool
asListOf length num =
    let
        on =
            List.repeat num True

        off =
            List.repeat (length - num) False
    in
        on ++ off


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
        [ span [] [ hours model.time |> toZeroPaddedString |> text ]
        , span [] [ text " : " ]
        , span [] [ minutes model.time |> toZeroPaddedString |> text ]
        , span [] [ text " : " ]
        , span [] [ seconds model.time |> toZeroPaddedString |> text ]
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
        [ pre [] [ model |> toString |> text ]
        , pre [] [ model.time |> hoursTens |> toString |> text ]
        , pre [] [ model.time |> hoursOnes |> toString |> text ]
        , pre [] [ model.time |> minutesTens |> toString |> text ]
        , pre [] [ model.time |> minutesOnes |> toString |> text ]
        ]



-- UPDATE


type Msg
    = Tick Time
    | ShuffleLists Time
    | ShuffleMinutesOnes (List Bool)
    | ShuffleMinutesTens (List Bool)
    | ShuffleHoursOnes (List Bool)
    | ShuffleHoursTens (List Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        ShuffleLists newTime ->
            ( { model | time = newTime }
            , Cmd.batch
                [ generate ShuffleMinutesOnes (shuffle (model.time |> minutesOnes |> asListOf 9))
                , generate ShuffleMinutesTens (shuffle (model.time |> minutesTens |> asListOf 6))
                , generate ShuffleHoursOnes (shuffle (model.time |> hoursOnes |> asListOf 9))
                , generate ShuffleHoursTens (shuffle (model.time |> hoursTens |> asListOf 3))
                ]
            )

        ShuffleMinutesOnes newList ->
            ( { model | minutesOnesList = newList }, Cmd.none )

        ShuffleMinutesTens newList ->
            ( { model | minutesTensList = newList }, Cmd.none )

        ShuffleHoursOnes newList ->
            ( { model | hoursOnesList = newList }, Cmd.none )

        ShuffleHoursTens newList ->
            ( { model | hoursTensList = newList }, Cmd.none )



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
            Time.every second ShuffleLists
        else
            Time.every second Tick


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
