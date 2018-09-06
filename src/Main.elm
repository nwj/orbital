port module Main exposing (main)

import Browser
import Build exposing (Build)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Random
import Stopwatch exposing (Stopwatch)
import Time
import Timing exposing (Timing)



-- TODO (nwj) Add ability to store multiple builds in memory
-- TODO (nwj) Add ability to export or import a build
-- TODO (nwj) Add caching of builds backed by localStorage


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { stopwatch : Stopwatch
    , currentBuild : Build
    , newTiming : Int
    , newTimingPhrase : String
    , idSeed : Random.Seed
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( Model Stopwatch.init Build.init 0 "" (Random.initialSeed flags)
    , Cmd.none
    )


port textToSpeechQueue : Json.Encode.Value -> Cmd msg


anyPositiveInt : Random.Generator Int
anyPositiveInt =
    Random.int 0 Random.maxInt



-- UPDATE


type Msg
    = Tick Time.Posix
    | ResetStopwatch
    | ToggleStopwatch
    | NewTime String
    | NewPhrase String
    | AddTiming
    | RemoveTiming Timing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if Stopwatch.isPlaying model.stopwatch then
                if Build.anyTimingsByTime model.stopwatch.time model.currentBuild then
                    ( { model | stopwatch = Stopwatch.tick model.stopwatch }
                    , textToSpeechQueue (Json.Encode.list (\t -> Json.Encode.string t) (List.map .phrase (Build.timingsByTime model.stopwatch.time model.currentBuild)))
                    )

                else
                    ( { model | stopwatch = Stopwatch.tick model.stopwatch }
                    , Cmd.none
                    )

            else
                ( model
                , Cmd.none
                )

        ResetStopwatch ->
            ( { model | stopwatch = Stopwatch.init }
            , Cmd.none
            )

        ToggleStopwatch ->
            ( { model | stopwatch = Stopwatch.toggle model.stopwatch }
            , Cmd.none
            )

        NewTime timing ->
            ( { model | newTiming = Maybe.withDefault 0 (String.toInt timing) }
            , Cmd.none
            )

        NewPhrase phrase ->
            ( { model | newTimingPhrase = phrase }
            , Cmd.none
            )

        AddTiming ->
            let
                ( newId, newSeed ) =
                    Random.step anyPositiveInt model.idSeed

                newTiming =
                    Timing newId model.newTiming model.newTimingPhrase
            in
            ( { model
                | currentBuild = Build.addTiming newTiming model.currentBuild
                , idSeed = newSeed
              }
            , Cmd.none
            )

        RemoveTiming timing ->
            ( { model | currentBuild = Build.removeTiming timing model.currentBuild }
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
        [ div [] [ text (secondsToClockString model.stopwatch.time) ]
        , div []
            [ button [ onClick ResetStopwatch ] [ text "Reset" ]
            , viewTimerControl model
            ]
        , div [] (viewTimings model)
        , div []
            [ input [ type_ "number", placeholder "Enter timing", value (String.fromInt model.newTiming), onInput NewTime ] []
            , input [ type_ "text", placeholder "Enter phrase", value model.newTimingPhrase, onInput NewPhrase ] []
            , button [ onClick AddTiming ] [ text "Add" ]
            ]
        ]


viewTimerControl : Model -> Html Msg
viewTimerControl model =
    if Stopwatch.isPaused model.stopwatch then
        button [ onClick ToggleStopwatch ] [ text "Pause" ]

    else
        button [ onClick ToggleStopwatch ] [ text "Play" ]


viewTimings : Model -> List (Html Msg)
viewTimings model =
    let
        timingToText : Timing -> String
        timingToText t =
            secondsToClockString t.time ++ " " ++ t.phrase

        viewTiming : Timing -> Html Msg
        viewTiming t =
            div []
                [ div [] [ text (timingToText t) ]
                , button [ onClick (RemoveTiming t) ] [ text "X" ]
                ]
    in
    List.map viewTiming (List.sortBy .time model.currentBuild.timings)


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
