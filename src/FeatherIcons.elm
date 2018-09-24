module FeatherIcons exposing (arrowLeft, copy, github, heart, pause, play, plus, repeat, rotateCcw, save, x)

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


pause : Html msg
pause =
    svgFeatherIcon "pause"
        [ Svg.rect [ Svg.Attributes.x "6", y "4", width "4", height "16" ] []
        , Svg.rect [ Svg.Attributes.x "14", y "4", width "4", height "16" ] []
        ]


play : Html msg
play =
    svgFeatherIcon "play"
        [ Svg.polygon [ points "5 3 19 12 5 21 5 3" ] [] ]


rotateCcw : Html msg
rotateCcw =
    svgFeatherIcon "rotate-ccw"
        [ Svg.polyline [ points "1 4 1 10 7 10" ] []
        , Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] []
        ]


save : Html msg
save =
    svgFeatherIcon "save"
        [ Svg.path [ d "M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z" ] []
        , Svg.polyline [ points "17 21 17 13 7 13 7 21" ] []
        , Svg.polyline [ points "7 3 7 8 15 8" ] []
        ]


repeat : Html msg
repeat =
    svgFeatherIcon "repeat"
        [ Svg.polyline [ points "17 1 21 5 17 9" ] []
        , Svg.path [ d "M3 11V9a4 4 0 0 1 4-4h14" ] []
        , Svg.polyline [ points "7 23 3 19 7 15" ] []
        , Svg.path [ d "M21 13v2a4 4 0 0 1-4 4H3" ] []
        ]


x : Html msg
x =
    svgFeatherIcon "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


plus : Html msg
plus =
    svgFeatherIcon "plus"
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


arrowLeft : Html msg
arrowLeft =
    svgFeatherIcon "arrow-left"
        [ Svg.line [ x1 "19", y1 "12", x2 "5", y2 "12" ] []
        , Svg.polyline [ points "12 19 5 12 12 5" ] []
        ]


copy : Html msg
copy =
    svgFeatherIcon "copy"
        [ Svg.rect [ Svg.Attributes.x "9", y "9", width "13", height "13", rx "2", ry "2" ] []
        , Svg.path [ d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" ] []
        ]


github : Html msg
github =
    svgFeatherIcon "github" [ Svg.path [ d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" ] [] ]


heart : Html msg
heart =
    svgFeatherIcon "heart" [ Svg.path [ d "M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z" ] [] ]
