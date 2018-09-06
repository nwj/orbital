port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Random
import Time



-- TODO (nwj) Break out build, timing, clock modules
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
    { timerRunning : Bool
    , timer : Int
    , currentBuild : Build
    , newTiming : Int
    , newTimingPhrase : String
    , idSeed : Random.Seed
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( Model False 0 (Build -1 "" []) 0 "" (Random.initialSeed flags)
    , Cmd.none
    )


port textToSpeechQueue : Json.Encode.Value -> Cmd msg


type alias Timing =
    { id : Int
    , timing : Int
    , timingPhrase : String
    }


type alias Build =
    { id : Int
    , name : String
    , timings : List Timing
    }


anyPositiveInt : Random.Generator Int
anyPositiveInt =
    Random.int 0 Random.maxInt



-- UPDATE


type Msg
    = Tick Time.Posix
    | ResetTimer
    | StopTimer
    | StartTimer
    | NewTiming String
    | NewTimingPhrase String
    | AddNewTiming
    | RemoveTiming Timing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.timerRunning then
                if List.any (\t -> t.timing == model.timer) model.currentBuild.timings then
                    ( { model | timer = model.timer + 1 }
                    , textToSpeechQueue (Json.Encode.list (\t -> Json.Encode.string t) (List.map .timingPhrase (List.filter (\t -> t.timing == model.timer) model.currentBuild.timings)))
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
            let
                ( newId, newSeed ) =
                    Random.step anyPositiveInt model.idSeed
            in
            ( { model
                | currentBuild = Build model.currentBuild.id model.currentBuild.name (Timing newId model.newTiming model.newTimingPhrase :: model.currentBuild.timings)
                , idSeed = newSeed
              }
            , Cmd.none
            )

        RemoveTiming timing ->
            ( { model | currentBuild = Build model.currentBuild.id model.currentBuild.name (List.filter (\t -> not (t.id == timing.id)) model.currentBuild.timings) }
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


viewTimings : Model -> List (Html Msg)
viewTimings model =
    let
        timingToText : Timing -> String
        timingToText t =
            secondsToClockString t.timing ++ " " ++ t.timingPhrase

        viewTiming : Timing -> Html Msg
        viewTiming t =
            div []
                [ div [] [ text (timingToText t) ]
                , button [ onClick (RemoveTiming t) ] [ text "X" ]
                ]
    in
    List.map viewTiming (List.sortBy .timing model.currentBuild.timings)


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
