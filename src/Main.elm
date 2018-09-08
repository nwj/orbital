port module Main exposing (main)

import Browser
import Build exposing (Build)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Random
import Stopwatch exposing (Stopwatch)
import Time
import Timing exposing (Timing)



-- TODO (nwj) Add ability to export or import a build
-- TODO (nwj) Add caching of builds backed by localStorage
-- TODO (nwj) Add variable clock speeds
-- TODO (nwj) JSON encode/decode flags


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
    , builds : Dict Int Build
    , newTiming : Int
    , newTimingPhrase : String
    , idSeed : Random.Seed
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags

        ( id, nextSeed ) =
            Random.step anyPositiveInt initialSeed
    in
    ( Model Stopwatch.init (Build.init id) Dict.empty 0 "" nextSeed
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
    | SaveBuild
    | NewBuild
    | SelectBuild Build
    | NameBuild String


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

        SaveBuild ->
            ( { model | builds = Dict.insert model.currentBuild.id model.currentBuild model.builds }
            , Cmd.none
            )

        NewBuild ->
            let
                ( newId, newSeed ) =
                    Random.step anyPositiveInt model.idSeed
            in
            ( { model | currentBuild = Build.init newId, idSeed = newSeed }
            , Cmd.none
            )

        SelectBuild build ->
            ( { model | currentBuild = build }
            , Cmd.none
            )

        NameBuild newName ->
            let
                currentBuild =
                    model.currentBuild

                newBuild =
                    { currentBuild | name = newName }
            in
            ( { model | currentBuild = newBuild }
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
        , div []
            [ div []
                (List.map
                    (\b -> a [ onClick (SelectBuild b) ] [ text b.name ])
                    (Dict.values model.builds)
                )
            ]
        , div [] (viewTimings model)
        , div []
            [ input [ type_ "text", placeholder "Name your build", value model.currentBuild.name, onInput NameBuild ] []
            , button [ onClick SaveBuild ] [ text "Save Build" ]
            , button [ onClick NewBuild ] [ text "New Build" ]
            ]
        , div []
            [ input [ type_ "number", placeholder "Enter timing", value (String.fromInt model.newTiming), onInput NewTime ] []
            , input [ type_ "text", placeholder "Enter phrase", value model.newTimingPhrase, onInput NewPhrase ] []
            , button [ onClick AddTiming ] [ text "Add Timing" ]
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
