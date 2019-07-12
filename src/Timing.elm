module Timing exposing
    ( Timing
    , anyTimingsByTime
    , equal
    , jsonDecodeTiming
    , jsonEncodeTiming
    , timingsByTime
    , toClockHours
    , toClockMinutes
    , toClockSeconds
    )

import Json.Decode exposing (field)
import Json.Encode



-- TIMING


type alias Timing =
    { id : String
    , time : Int
    , phrase : String
    }


equal : Timing -> Timing -> Bool
equal timing1 timing2 =
    timing1.id == timing2.id && timing1.time == timing2.time && timing1.phrase == timing2.phrase



-- QUERYING LISTS OF TIMINGS


anyTimingsByTime : Int -> List Timing -> Bool
anyTimingsByTime time timings =
    List.any (\timing -> timing.time == time) timings


timingsByTime : Int -> List Timing -> List Timing
timingsByTime time timings =
    List.filter (\timing -> timing.time == time) timings



-- FORMATING TIME


toClockHours : Timing -> Int
toClockHours timing =
    timing.time // 3600


toClockMinutes : Timing -> Int
toClockMinutes timing =
    modBy 3600 timing.time // 60


toClockSeconds : Timing -> Int
toClockSeconds timing =
    modBy 60 (modBy 3600 timing.time)



-- JSON


jsonDecodeTiming : Json.Decode.Decoder Timing
jsonDecodeTiming =
    Json.Decode.map3 Timing
        (field "id" Json.Decode.string)
        (field "time" Json.Decode.int)
        (field "phrase" Json.Decode.string)


jsonEncodeTiming : Timing -> Json.Encode.Value
jsonEncodeTiming timing =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| timing.id )
        , ( "time", Json.Encode.int <| timing.time )
        , ( "phrase", Json.Encode.string <| timing.phrase )
        ]
