module Stopwatch exposing
    ( Stopwatch
    , init
    , isPaused
    , isPlaying
    , pause
    , play
    , tick
    , toClockString
    , toggle
    )

-- STOPWATCH


type alias Stopwatch =
    { status : StopwatchStatus
    , time : Int
    }


init : Stopwatch
init =
    Stopwatch Paused 0



-- STATUS CONTROLS


type StopwatchStatus
    = Playing
    | Paused


play : Stopwatch -> Stopwatch
play stopwatch =
    { stopwatch | status = Playing }


pause : Stopwatch -> Stopwatch
pause stopwatch =
    { stopwatch | status = Paused }


toggle : Stopwatch -> Stopwatch
toggle stopwatch =
    if isPlaying stopwatch then
        pause stopwatch

    else
        play stopwatch


isPlaying : Stopwatch -> Bool
isPlaying stopwatch =
    stopwatch.status == Playing


isPaused : Stopwatch -> Bool
isPaused stopwatch =
    stopwatch.status == Paused



-- TIME MANIPULATION


tick : Stopwatch -> Stopwatch
tick stopwatch =
    { stopwatch | time = stopwatch.time + 1 }



-- VIEW


toClockString : Stopwatch -> String
toClockString stopwatch =
    let
        hours =
            stopwatch.time // 3600

        minutes =
            modBy 3600 stopwatch.time // 60

        seconds =
            modBy 60 (modBy 3600 stopwatch.time)

        zeroPad i =
            if String.length (String.fromInt i) == 1 then
                "0" ++ String.fromInt i

            else
                String.fromInt i
    in
    case ( minutes > 0, hours > 0 ) of
        ( False, False ) ->
            String.fromInt seconds

        ( True, False ) ->
            String.fromInt minutes ++ ":" ++ zeroPad seconds

        ( False, True ) ->
            String.fromInt hours ++ ":" ++ zeroPad minutes ++ ":" ++ zeroPad seconds

        ( True, True ) ->
            String.fromInt hours ++ ":" ++ zeroPad minutes ++ ":" ++ zeroPad seconds
