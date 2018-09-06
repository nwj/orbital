module Build exposing (Build, addTiming, anyTimingsByTime, init, removeTiming, timingsByTime)

import Timing exposing (Timing)



-- BUILD


type alias Build =
    { id : Int
    , name : String
    , timings : List Timing
    }


init : Build
init =
    Build -1 "" []



-- MANIPULATING BUILD TIMINGS


addTiming : Timing -> Build -> Build
addTiming newTiming build =
    { build | timings = newTiming :: build.timings }


removeTiming : Timing -> Build -> Build
removeTiming timingToRemove build =
    { build | timings = List.filter (\timing -> not (timing.id == timingToRemove.id)) build.timings }



-- QUERYING BUILD TIMINGS


anyTimingsByTime : Int -> Build -> Bool
anyTimingsByTime time build =
    Timing.anyTimingsByTime time build.timings


timingsByTime : Int -> Build -> List Timing
timingsByTime time build =
    Timing.timingsByTime time build.timings
