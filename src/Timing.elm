module Timing exposing
    ( Timing
    , anyTimingsByTime
    , decodeTiming
    , encodeTiming
    , timingsByTime
    )

import Json.Decode exposing (field)
import Json.Encode



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



-- JSON


decodeTiming : Json.Decode.Decoder Timing
decodeTiming =
    Json.Decode.map3 Timing
        (field "id" Json.Decode.int)
        (field "time" Json.Decode.int)
        (field "phrase" Json.Decode.string)


encodeTiming : Timing -> Json.Encode.Value
encodeTiming timing =
    Json.Encode.object
        [ ( "id", Json.Encode.int <| timing.id )
        , ( "time", Json.Encode.int <| timing.time )
        , ( "phrase", Json.Encode.string <| timing.phrase )
        ]
