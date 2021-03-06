port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Build exposing (Build)
import Dict exposing (Dict)
import FeatherIcons
import Html exposing (Attribute, Html, a, button, div, input, span, text)
import Html.Attributes exposing (class, classList, href, placeholder, target, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Html.Lazy exposing (lazy)
import Json.Decode exposing (field)
import Json.Encode
import Random
import Stopwatch exposing (Stopwatch)
import Time
import Timing exposing (Timing)
import Url exposing (Url)
import Url.Builder
import Url.Parser



-- TODO (nwj) Add ability to export or import a build
-- TODO (nwj) Add some preset builds
-- TODO (nwj) Look into giving stopwatch milliseconds precision


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \m ->
                { title = "Orbital"
                , body = [ view m ]
                }
        , onUrlChange = ChangeUrl
        , onUrlRequest = ClickLink
        }



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , route : Route
    , stopwatch : Stopwatch
    , currentBuild : Build
    , builds : Dict String Build
    , idSeed : Random.Seed
    }


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        decodedFlags =
            decodeFlags flags

        ( newBuildIdInt, newSeed ) =
            Random.step anyPositiveInt <| Random.initialSeed decodedFlags.seedInt

        newBuildId =
            String.fromInt newBuildIdInt

        initialBuild =
            Build.init newBuildId

        ( route, cmd ) =
            case Url.Parser.parse routeParser url of
                Just r ->
                    ( r, Cmd.none )

                -- Note that we're rewriting the url back to root in this case
                Nothing ->
                    ( Root, Nav.replaceUrl key <| Url.Builder.absolute [] [] )
    in
    ( Model key route Stopwatch.init initialBuild decodedFlags.storedBuilds newSeed
    , cmd
    )


anyPositiveInt : Random.Generator Int
anyPositiveInt =
    Random.int 0 Random.maxInt



-- ROUTES


type Route
    = Root
    | Builds


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Root Url.Parser.top
        , Url.Parser.map Builds (Url.Parser.s "builds")
        ]



-- PORTS


port textToSpeechQueue : Json.Encode.Value -> Cmd msg


port buildsToStore : Json.Encode.Value -> Cmd msg



-- FLAGS


type alias Flags =
    { seedInt : Int
    , storedBuilds : Dict String Build
    }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map2 Flags
        (field "seedInt" Json.Decode.int)
        (field "storedBuilds" <| Json.Decode.dict Build.jsonDecodeBuild)


decodeFlags : Json.Decode.Value -> Flags
decodeFlags encodedFlags =
    case Json.Decode.decodeValue flagsDecoder encodedFlags of
        Ok flags ->
            flags

        Err e ->
            Flags -1 Dict.empty



-- UPDATE


type Msg
    = ClickLink Browser.UrlRequest
    | ChangeUrl Url
    | Tick Time.Posix
    | ResetStopwatch
    | ToggleStopwatch
    | CurrentBuildNameChange String
    | StoreCurrentBuild
    | TimingSecondsChange Timing String
    | TimingMinutesChange Timing String
    | TimingHoursChange Timing String
    | TimingPhraseChange Timing String
    | RemoveTiming Timing
    | AddTiming
    | CopyBuild Build
    | RemoveBuild Build
    | NewBuild
    | SelectBuild Build


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                Browser.External url ->
                    ( model, Nav.load url )

        ChangeUrl url ->
            let
                route =
                    case Url.Parser.parse routeParser url of
                        Just r ->
                            r

                        Nothing ->
                            Root
            in
            ( { model | route = route }
            , Cmd.none
            )

        Tick _ ->
            {-
               We add 1 to stopwatch.time throughout here so that timings trigger text to speech at the
               beginning of each second rather than at the end of each second. One consequence of this
               design choice is that timings with time == 0 will never trigger. As a result, there is code
               elsewhere to prevent the creation of timings with time == 0.
            -}
            if Build.anyTimingsByTime (model.stopwatch.time + 1) model.currentBuild then
                ( { model | stopwatch = Stopwatch.tick model.stopwatch }
                , textToSpeechQueue <|
                    Json.Encode.list Json.Encode.string <|
                        List.map .phrase <|
                            Build.timingsByTime (model.stopwatch.time + 1) model.currentBuild
                )

            else
                ( { model | stopwatch = Stopwatch.tick model.stopwatch }
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

        CurrentBuildNameChange newName ->
            let
                currentBuild =
                    model.currentBuild

                newBuild =
                    { currentBuild | name = newName }
            in
            ( { model | currentBuild = newBuild }
            , Cmd.none
            )

        StoreCurrentBuild ->
            if currentBuildCanSave model then
                let
                    updatedBuilds =
                        Dict.insert model.currentBuild.id model.currentBuild model.builds
                in
                ( { model | builds = updatedBuilds }
                , buildsToStore <| Json.Encode.dict identity Build.jsonEncodeBuild updatedBuilds
                )

            else
                ( model, Cmd.none )

        TimingSecondsChange timing stringSeconds ->
            let
                oldSeconds =
                    Timing.toClockSeconds timing

                newSeconds =
                    Maybe.withDefault oldSeconds <| String.toInt stringSeconds

                secondsDiff =
                    newSeconds - oldSeconds

                newTime =
                    max 1 (timing.time + secondsDiff)

                newTiming =
                    { timing | time = newTime }

                newTimings =
                    Dict.insert newTiming.id newTiming model.currentBuild.timings

                oldCurrentBuild =
                    model.currentBuild

                newCurrentBuild =
                    { oldCurrentBuild | timings = newTimings }
            in
            ( { model | currentBuild = newCurrentBuild }
            , Cmd.none
            )

        TimingMinutesChange timing stringMinutes ->
            let
                oldMinutes =
                    Timing.toClockMinutes timing

                newMinutes =
                    Maybe.withDefault oldMinutes <| String.toInt stringMinutes

                minutesDiff =
                    newMinutes - oldMinutes

                newTime =
                    max 1 (timing.time + (minutesDiff * 60))

                newTiming =
                    { timing | time = newTime }

                newTimings =
                    Dict.insert newTiming.id newTiming model.currentBuild.timings

                oldCurrentBuild =
                    model.currentBuild

                newCurrentBuild =
                    { oldCurrentBuild | timings = newTimings }
            in
            ( { model | currentBuild = newCurrentBuild }
            , Cmd.none
            )

        TimingHoursChange timing stringHours ->
            let
                oldHours =
                    Timing.toClockHours timing

                newHours =
                    Maybe.withDefault oldHours <| String.toInt stringHours

                hoursDiff =
                    newHours - oldHours

                newTime =
                    max 1 (timing.time + (hoursDiff * 3600))

                newTiming =
                    { timing | time = newTime }

                newTimings =
                    Dict.insert newTiming.id newTiming model.currentBuild.timings

                oldCurrentBuild =
                    model.currentBuild

                newCurrentBuild =
                    { oldCurrentBuild | timings = newTimings }
            in
            ( { model | currentBuild = newCurrentBuild }
            , Cmd.none
            )

        TimingPhraseChange timing newPhrase ->
            let
                newTiming =
                    { timing | phrase = newPhrase }

                newTimings =
                    Dict.insert newTiming.id newTiming model.currentBuild.timings

                oldCurrentBuild =
                    model.currentBuild

                newCurrentBuild =
                    { oldCurrentBuild | timings = newTimings }
            in
            ( { model | currentBuild = newCurrentBuild }
            , Cmd.none
            )

        AddTiming ->
            let
                ( newIdInt, newSeed ) =
                    Random.step anyPositiveInt model.idSeed

                newId =
                    String.fromInt newIdInt

                latestTiming =
                    Build.latestTiming model.currentBuild

                newTime =
                    case latestTiming of
                        Just t ->
                            t.time + 1

                        Nothing ->
                            1

                newTiming =
                    Timing newId newTime ""
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

        CopyBuild build ->
            let
                ( newIdInt, newSeed ) =
                    Random.step anyPositiveInt model.idSeed

                newId =
                    String.fromInt newIdInt

                newBuildName =
                    "Copy of " ++ build.name

                newBuild =
                    { build | id = newId, name = newBuildName }
            in
            ( { model | currentBuild = newBuild, idSeed = newSeed }
            , Nav.pushUrl model.navKey <| Url.Builder.absolute [] []
            )

        RemoveBuild build ->
            let
                updatedBuilds =
                    Dict.remove build.id model.builds
            in
            ( { model | builds = updatedBuilds }
            , buildsToStore <| Json.Encode.dict identity Build.jsonEncodeBuild updatedBuilds
            )

        NewBuild ->
            let
                ( newIdInt, newSeed ) =
                    Random.step anyPositiveInt model.idSeed

                newId =
                    String.fromInt newIdInt
            in
            ( { model | currentBuild = Build.init newId, idSeed = newSeed }
            , Cmd.none
            )

        SelectBuild build ->
            ( { model | currentBuild = build }
            , Nav.pushUrl model.navKey <| Url.Builder.absolute [] []
            )


currentBuildCanSave : Model -> Bool
currentBuildCanSave model =
    if (String.length <| String.trim model.currentBuild.name) == 0 then
        False

    else
        case Dict.get model.currentBuild.id model.builds of
            Just build ->
                not <| Build.equal build model.currentBuild

            Nothing ->
                True



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if Stopwatch.isPlaying model.stopwatch then
        Time.every 1000 Tick

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "main-content" ]
            [ viewHeader
            , viewStopwatch model.stopwatch
            , div [ class "divider" ] []
            , if model.route == Root then
                viewCurrentBuild model

              else
                viewBuildManagement model
            ]
        , viewFooter
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "header" ] [ div [ class "logo" ] [ text "ORBITAL" ] ]


viewStopwatch : Stopwatch -> Html Msg
viewStopwatch stopwatch =
    div [ class "stopwatch" ]
        [ button
            [ classList
                [ ( "stopwatch__reset", True )
                , ( "stopwatch__reset--hidden", stopwatch.time == 0 && Stopwatch.isPaused stopwatch )
                ]
            , onClick ResetStopwatch
            ]
            [ FeatherIcons.rotateCcw ]
        , button
            [ classList
                [ ( "stopwatch__toggle", True )
                , ( "stopwatch__toggle--play", Stopwatch.isPaused stopwatch )
                , ( "stopwatch__toggle--pause", Stopwatch.isPlaying stopwatch )
                ]
            , onClick ToggleStopwatch
            ]
            [ viewStopwatchToggleButtonIcon stopwatch ]
        , div
            [ classList
                [ ( "stopwatch__clock", True )
                , ( "stopwatch__clock--hidden", stopwatch.time == 0 && Stopwatch.isPaused stopwatch )
                ]
            ]
            [ text <| Stopwatch.toClockString stopwatch ]
        ]


viewStopwatchToggleButtonIcon : Stopwatch -> Html msg
viewStopwatchToggleButtonIcon stopwatch =
    if Stopwatch.isPaused stopwatch then
        FeatherIcons.play

    else
        FeatherIcons.pause


viewCurrentBuild : Model -> Html Msg
viewCurrentBuild model =
    div
        [ class "current-build-pane" ]
        [ viewCurrentBuildControls model
        , viewTimings model
        ]


viewCurrentBuildControls : Model -> Html Msg
viewCurrentBuildControls model =
    div [ class "current-build" ]
        [ div [ class "current-build__header" ]
            [ div [ class "current-build__title" ] [ text "ACTIVE BUILD" ]
            , div [ class "current-build__header-buttons" ]
                [ button
                    [ classList
                        [ ( "current-build__button", True )
                        , ( "current-build__button--hidden", Dict.size model.builds > 0 )
                        ]
                    , onClick NewBuild
                    ]
                    [ FeatherIcons.plus, text "New Build" ]
                , a
                    [ classList
                        [ ( "current-build__button", True )
                        , ( "current-build__button--hidden", Dict.size model.builds < 1 )
                        ]
                    , href <| Url.Builder.absolute [ "builds" ] []
                    ]
                    [ FeatherIcons.repeat, text "Swap Build" ]
                , button
                    [ classList
                        [ ( "current-build__button", True )
                        , ( "current-build__button--disabled", not <| currentBuildCanSave model )
                        ]
                    , onClick StoreCurrentBuild
                    ]
                    [ FeatherIcons.save, text "Save Build" ]
                ]
            ]
        , div [ class "current-build__name-container" ]
            [ input
                [ class "current-build__name"
                , type_ "text"
                , value model.currentBuild.name
                , placeholder "Name for this build"
                , onInput CurrentBuildNameChange
                ]
                []
            ]
        ]


viewTimings : Model -> Html Msg
viewTimings model =
    div [ class "timings" ]
        [ div
            [ class "timings__list" ]
            (List.map (\t -> viewTiming t model) <|
                List.sortBy .time <|
                    Dict.values model.currentBuild.timings
            )
        , button
            [ class "timings__add"
            , onClick AddTiming
            ]
            [ FeatherIcons.plus
            , text "Add Timing"
            ]
        ]


viewTiming : Timing -> Model -> Html Msg
viewTiming timing model =
    div
        [ classList
            [ ( "timing", True )
            , ( "timing--flash", viewTimingShouldFlash timing model )
            ]
        ]
        [ lazy viewTimingTimes timing
        , input
            [ class "timing__phrase"
            , value timing.phrase
            , onInput <| TimingPhraseChange timing
            ]
            []
        , button
            [ class "timing__remove"
            , onClick <| RemoveTiming timing
            ]
            [ FeatherIcons.x, text "Remove Timing" ]
        ]


viewTimingTimes : Timing -> Html Msg
viewTimingTimes timing =
    let
        hours =
            Timing.toClockHours timing

        minutes =
            Timing.toClockMinutes timing

        seconds =
            Timing.toClockSeconds timing

        zeroPad i =
            if String.length (String.fromInt i) == 1 then
                "0" ++ String.fromInt i

            else
                String.fromInt i
    in
    if hours < 1 then
        div [ class "timing__times" ]
            [ input
                [ class "timing__time"
                , value <| zeroPad minutes
                , onBlurWithTargetValue <| TimingMinutesChange timing
                ]
                []
            , span [ class "timing__time-separator" ] [ text ":" ]
            , input
                [ class "timing__time"
                , value <| zeroPad seconds
                , onBlurWithTargetValue <| TimingSecondsChange timing
                ]
                []
            ]

    else
        div [ class "timing__times" ]
            [ input
                [ class "timing__time"
                , value <| zeroPad hours
                , onBlurWithTargetValue <| TimingHoursChange timing
                ]
                []
            , span [ class "timing__time-separator" ] [ text ":" ]
            , input
                [ class "timing__time"
                , value <| zeroPad minutes
                , onBlurWithTargetValue <| TimingMinutesChange timing
                ]
                []
            , span [ class "timing__time-separator" ] [ text ":" ]
            , input
                [ class "timing__time"
                , value <| zeroPad seconds
                , onBlurWithTargetValue <| TimingSecondsChange timing
                ]
                []
            ]


viewTimingShouldFlash : Timing -> Model -> Bool
viewTimingShouldFlash timing model =
    Stopwatch.isPlaying model.stopwatch
        && (timing.time >= model.stopwatch.time - 1)
        && (timing.time <= model.stopwatch.time + 1)


viewBuildManagement : Model -> Html Msg
viewBuildManagement model =
    div
        [ class "builds-pane" ]
        [ div [ class "builds__header" ]
            [ div [ class "builds__title" ] [ text "BUILDS" ]
            , div [ class "builds__header-buttons" ]
                [ a
                    [ class "builds__button"
                    , href <| Url.Builder.absolute [] []
                    ]
                    [ FeatherIcons.arrowLeft
                    , text "To Active Build"
                    ]
                , button
                    [ class "builds__button"
                    , onClick NewBuild
                    ]
                    [ FeatherIcons.plus
                    , text "New Build"
                    ]
                ]
            ]
        , div [ class "builds__list" ] (List.map viewBuild <| Dict.values model.builds)
        ]


viewBuild : Build -> Html Msg
viewBuild build =
    div [ class "build" ]
        [ div [ class "build__name", onClick <| SelectBuild build ] [ text build.name ]
        , div [ class "build__controls" ]
            [ button
                [ class "build__button", onClick <| CopyBuild build ]
                [ FeatherIcons.copy, text "Copy Build" ]
            , button
                [ class "build__button", onClick <| RemoveBuild build ]
                [ FeatherIcons.x, text "Remove Build" ]
            ]
        ]


viewFooter : Html Msg
viewFooter =
    div [ class "footer" ]
        [ div [ class "footer__item" ] [ text "© 2018" ]
        , a
            [ class "footer__link"
            , href "https://nwj.cc"
            , target "_blank"
            ]
            [ text "Made with"
            , FeatherIcons.heart
            , text "by NWJ"
            ]
        , a
            [ class "footer__link"
            , href "https://github.com/nwj/orbital"
            , target "_blank"
            ]
            [ FeatherIcons.github
            , text "Github"
            ]
        ]


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Json.Decode.map tagger targetValue)
