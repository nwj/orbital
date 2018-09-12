module Timing exposing
    ( Timing
    , anyTimingsByTime
    , decodeTiming
    , encodeTiming
    , equal
    , timingsByTime
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



-- JSON


decodeTiming : Json.Decode.Decoder Timing
decodeTiming =
    Json.Decode.map3 Timing
        (field "id" Json.Decode.string)
        (field "time" Json.Decode.int)
        (field "phrase" Json.Decode.string)


encodeTiming : Timing -> Json.Encode.Value
encodeTiming timing =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| timing.id )
        , ( "time", Json.Encode.int <| timing.time )
        , ( "phrase", Json.Encode.string <| timing.phrase )
        ]
