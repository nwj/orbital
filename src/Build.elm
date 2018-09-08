module Build exposing
    ( Build
    , addTiming
    , anyTimingsByTime
    , decodeBuild
    , encodeBuild
    , init
    , removeTiming
    , timingsByTime
    )

import Json.Decode exposing (field)
import Json.Encode
import Timing exposing (Timing)



-- BUILD


type alias Build =
    { id : Int
    , name : String
    , timings : List Timing
    }


init : Int -> Build
init id =
    Build id "" []



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



-- JSON


decodeBuild : Json.Decode.Decoder Build
decodeBuild =
    Json.Decode.map3 Build
        (field "id" Json.Decode.int)
        (field "name" Json.Decode.string)
        (field "timings" <| Json.Decode.list Timing.decodeTiming)


encodeBuild : Build -> Json.Encode.Value
encodeBuild build =
    Json.Encode.object
        [ ( "id", Json.Encode.int <| build.id )
        , ( "name", Json.Encode.string <| build.name )
        , ( "timings", Json.Encode.list Timing.encodeTiming build.timings )
        ]
