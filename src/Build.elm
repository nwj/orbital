module Build exposing
    ( Build
    , addTiming
    , anyTimingsByTime
    , equal
    , init
    , jsonDecodeBuild
    , jsonEncodeBuild
    , latestTiming
    , removeTiming
    , timingsByTime
    )

import Dict exposing (Dict)
import Json.Decode exposing (field)
import Json.Encode
import Timing exposing (Timing)



-- BUILD


type alias Build =
    { id : String
    , name : String
    , timings : Dict String Timing
    }


init : String -> Build
init id =
    Build id "My Build" Dict.empty


equal : Build -> Build -> Bool
equal build1 build2 =
    build1.id == build2.id && build1.name == build2.name && timingsEqual build1 build2


timingsEqual : Build -> Build -> Bool
timingsEqual build1 build2 =
    let
        timings1 =
            List.sortBy .id <| Dict.values build1.timings

        timings2 =
            List.sortBy .id <| Dict.values build2.timings

        listsAreEqualLength =
            List.length timings1 == List.length timings2

        listElementsAreEqual =
            List.all (\t -> Timing.equal (Tuple.first t) (Tuple.second t)) <| List.map2 Tuple.pair timings1 timings2
    in
    listsAreEqualLength && listElementsAreEqual



-- MANIPULATING BUILD TIMINGS


addTiming : Timing -> Build -> Build
addTiming newTiming build =
    { build | timings = Dict.insert newTiming.id newTiming build.timings }


removeTiming : Timing -> Build -> Build
removeTiming timingToRemove build =
    { build | timings = Dict.remove timingToRemove.id build.timings }



-- QUERYING BUILD TIMINGS


anyTimingsByTime : Int -> Build -> Bool
anyTimingsByTime time build =
    Timing.anyTimingsByTime time <| Dict.values build.timings


timingsByTime : Int -> Build -> List Timing
timingsByTime time build =
    Timing.timingsByTime time <| Dict.values build.timings


latestTiming : Build -> Maybe Timing
latestTiming build =
    List.head <| List.reverse <| List.sortBy .time <| Dict.values build.timings



-- JSON


jsonDecodeBuild : Json.Decode.Decoder Build
jsonDecodeBuild =
    Json.Decode.map3 Build
        (field "id" Json.Decode.string)
        (field "name" Json.Decode.string)
        (field "timings" <| Json.Decode.dict Timing.jsonDecodeTiming)


jsonEncodeBuild : Build -> Json.Encode.Value
jsonEncodeBuild build =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| build.id )
        , ( "name", Json.Encode.string <| build.name )
        , ( "timings", Json.Encode.dict identity Timing.jsonEncodeTiming build.timings )
        ]
