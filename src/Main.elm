module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
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
    { timerPaused : Bool
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if not model.timerPaused then
                ( { model | timer = model.timer + 1 }
                , Cmd.none
                )

            else
                ( model
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
        [ div [] [ text (String.fromInt model.timer) ] ]
