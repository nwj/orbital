module Stopwatch exposing (Stopwatch, init, isPaused, isPlaying, pause, play, tick, toggle)

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
