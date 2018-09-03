port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { timerRunning : Bool
    , timer : Int
    , timingDict : Dict Int String
    , newTiming : Int
    , newTimingPhrase : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False 0 Dict.empty 0 ""
    , Cmd.none
    )


port textToSpeechQueue : Json.Encode.Value -> Cmd msg



-- UPDATE


type Msg
    = Tick Time.Posix
    | ResetTimer
    | StopTimer
    | StartTimer
    | NewTiming String
    | NewTimingPhrase String
    | AddNewTiming


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.timerRunning then
                if Dict.member model.timer model.timingDict then
                    ( { model | timer = model.timer + 1 }
                    , textToSpeechQueue (Json.Encode.string (Maybe.withDefault "" (Dict.get model.timer model.timingDict)))
                    )

                else
                    ( { model | timer = model.timer + 1 }
                    , Cmd.none
                    )

            else
                ( model
                , Cmd.none
                )

        ResetTimer ->
            ( { model | timer = 0 }
            , Cmd.none
            )

        StopTimer ->
            ( { model | timerRunning = False }
            , Cmd.none
            )

        StartTimer ->
            ( { model | timerRunning = True }
            , Cmd.none
            )

        NewTiming timing ->
            ( { model | newTiming = Maybe.withDefault 0 (String.toInt timing) }
            , Cmd.none
            )

        NewTimingPhrase phrase ->
            ( { model | newTimingPhrase = phrase }
            , Cmd.none
            )

        AddNewTiming ->
            ( { model | timingDict = Dict.insert model.newTiming model.newTimingPhrase model.timingDict }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (secondsToClockString model.timer) ]
        , div []
            [ button [ onClick ResetTimer ] [ text "Reset" ]
            , viewTimerControl model
            ]
        , div [] (viewTimings model)
        , div []
            [ input [ type_ "number", placeholder "Enter timing", value (String.fromInt model.newTiming), onInput NewTiming ] []
            , input [ type_ "text", placeholder "Enter phrase", value model.newTimingPhrase, onInput NewTimingPhrase ] []
            , button [ onClick AddNewTiming ] [ text "Add" ]
            ]
        ]


viewTimerControl : Model -> Html Msg
viewTimerControl model =
    if model.timerRunning then
        button [ onClick StopTimer ] [ text "Pause" ]

    else
        button [ onClick StartTimer ] [ text "Play" ]


viewTimings : Model -> List (Html msg)
viewTimings model =
    let
        timingsList =
            Dict.toList model.timingDict

        timingToText t =
            secondsToClockString (Tuple.first t) ++ " " ++ Tuple.second t

        viewTiming t =
            div []
                [ text (timingToText t) ]
    in
    List.map viewTiming timingsList


secondsToClockString : Int -> String
secondsToClockString s =
    let
        format i =
            if String.length (String.fromInt i) == 1 then
                "0" ++ String.fromInt i

            else
                String.fromInt i

        displayMinutes =
            format (s // 60)

        displaySeconds =
            format (modBy 60 s)
    in
    displayMinutes ++ " : " ++ displaySeconds
