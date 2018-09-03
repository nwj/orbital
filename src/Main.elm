module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | ResetTimer
    | StopTimer
    | StartTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.timerRunning then
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (String.fromInt model.timer) ]
        , div []
            [ button [ onClick ResetTimer ] [ text "Reset" ]
            , viewTimerControl model
            ]
        ]


viewTimerControl : Model -> Html Msg
viewTimerControl model =
    if model.timerRunning then
        button [ onClick StopTimer ] [ text "Pause" ]

    else
        button [ onClick StartTimer ] [ text "Play" ]
