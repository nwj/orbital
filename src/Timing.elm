module Timing exposing
    ( Timing
    , anyTimingsByTime
    , timingsByTime
    )

-- TIMING


type alias Timing =
    { id : Int
    , time : Int
    , phrase : String
    }



-- QUERYING LISTS OF TIMINGS


anyTimingsByTime : Int -> List Timing -> Bool
anyTimingsByTime time timings =
    List.any (\timing -> timing.time == time) timings


timingsByTime : Int -> List Timing -> List Timing
timingsByTime time timings =
    List.filter (\timing -> timing.time == time) timings
