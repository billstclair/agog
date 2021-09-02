---------------------------------------------------------------------
--
-- Board.elm
-- Zephyrnot board, storage and rendering.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Agog.Board exposing
    ( SizerKind(..)
    , colToString
    , count
    , empty
    , get
    , getSizer
    , render
    , rowToString
    , score
    , set
    , simulateGame
    , winner
    )

import Agog.Types as Types
    exposing
        ( Board
        , Color
        , Decoration(..)
        , GameState
        , Player(..)
        , SavedModel
        , Style
        , Winner(..)
        )
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as LE
import Random exposing (Seed)
import Set exposing (Set)
import Svg
    exposing
        ( Attribute
        , Svg
        , defs
        , foreignObject
        , g
        , line
        , marker
        , path
        , rect
        , svg
        )
import Svg.Attributes as Attributes
    exposing
        ( cx
        , cy
        , d
        , fill
        , fillOpacity
        , fontSize
        , height
        , markerEnd
        , markerHeight
        , markerStart
        , markerWidth
        , orient
        , points
        , r
        , refX
        , refY
        , rotate
        , rx
        , ry
        , stroke
        , strokeDasharray
        , strokeOpacity
        , strokeWidth
        , textAnchor
        , transform
        , viewBox
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Svg.Events as Events


empty : Board
empty =
    Array.repeat 6 (Array.repeat 6 False)


count : Board -> Int
count board =
    Array.toList board
        |> List.map Array.toList
        |> List.concat
        |> List.filter ((==) True)
        |> List.length


score : Board -> Int
score board =
    31 - count board


get : Int -> Int -> Board -> Bool
get row col board =
    case Array.get row board of
        Nothing ->
            False

        Just r ->
            case Array.get col r of
                Nothing ->
                    False

                Just res ->
                    res


set : Int -> Int -> Board -> Board
set row col board =
    case Array.get row board of
        Nothing ->
            board

        Just r ->
            Array.set row
                (Array.set col True r)
                board


{-| It might be worthwhile to have an option to increment row or col first.
-}
winner : Player -> Board -> ( Winner, List ( Int, Int ) )
winner player board =
    let
        findPath : Int -> Int -> (( Int, Int ) -> Bool) -> Set ( Int, Int ) -> List ( Int, Int ) -> ( Bool, Set ( Int, Int ), List ( Int, Int ) )
        findPath row col done seen path =
            if done ( row, col ) then
                ( True, seen, path )

            else if row < 0 || row > 5 || col < 0 || col > 5 then
                ( False, seen, path )

            else if Set.member ( row, col ) seen then
                ( False, seen, path )

            else
                let
                    seen2 =
                        Set.insert ( row, col ) seen

                    path2 =
                        ( row, col ) :: path
                in
                if not <| get row col board then
                    ( False, seen2, path )

                else
                    let
                        ( res3, seen3, path3 ) =
                            findPath row (col + 1) done seen2 path2
                    in
                    if res3 then
                        ( True, seen3, path3 )

                    else
                        let
                            ( res4, seen4, path4 ) =
                                findPath (row + 1) col done seen3 path2
                        in
                        if res4 then
                            ( True, seen4, path4 )

                        else
                            let
                                ( res5, seen5, path5 ) =
                                    findPath row (col - 1) done seen4 path2
                            in
                            if res5 then
                                ( True, seen5, path5 )

                            else
                                findPath (row - 1) col done seen5 path2

        hloop row seen =
            let
                ( res, seen2, path2 ) =
                    findPath row 0 (\( _, col ) -> col > 5) seen []
            in
            if res then
                ( True, List.reverse path2 )

            else if row >= 5 then
                ( False, [] )

            else
                hloop (row + 1) seen2

        vloop col seen =
            let
                ( res, seen2, path2 ) =
                    findPath 0 col (\( row, _ ) -> row > 5) seen []
            in
            if res then
                ( True, List.reverse path2 )

            else if col >= 5 then
                ( False, [] )

            else
                vloop (col + 1) seen2
    in
    let
        ( hwin, hpath ) =
            hloop 0 Set.empty

        ( vwin, vpath ) =
            vloop 0 Set.empty
    in
    if hwin && vwin then
        if player == WhitePlayer then
            ( WhiteWinner, hpath )

        else
            ( BlackWinner, vpath )

    else if hwin then
        ( WhiteWinner, hpath )

    else if vwin then
        ( BlackWinner, vpath )

    else
        ( NoWinner, [] )



---
--- Simulation
---


simulateGame : Seed -> ( Winner, Int, Seed )
simulateGame seed =
    let
        loop s b isH =
            let
                ( s2, b2 ) =
                    simulateMove s b isH

                player =
                    if isH then
                        WhitePlayer

                    else
                        BlackPlayer

                ( win, _ ) =
                    winner player b2
            in
            if win == NoWinner then
                loop s2 b2 (not isH)

            else
                ( win, score b2, s2 )
    in
    loop seed empty True


simulateMove : Seed -> Board -> Bool -> ( Seed, Board )
simulateMove seed board isH =
    let
        gen =
            Random.int 0 5

        ( row, seed2 ) =
            Random.step gen seed

        ( col, seed3 ) =
            Random.step gen seed2

        ( board2, seed5 ) =
            if get row col board then
                if isH then
                    let
                        cnt =
                            emptyCols row board

                        ( c, seed4 ) =
                            Random.step (Random.int 0 (cnt - 1)) seed3
                    in
                    ( setEmptyCol row c board, seed4 )

                else
                    let
                        cnt =
                            emptyRows col board

                        ( r, seed4 ) =
                            Random.step (Random.int 0 (cnt - 1)) seed3
                    in
                    ( setEmptyRow r col board, seed4 )

            else
                ( set row col board, seed3 )
    in
    ( seed5, board2 )


emptyCols : Int -> Board -> Int
emptyCols row board =
    List.foldl
        (\col sum ->
            if get row col board then
                sum

            else
                sum + 1
        )
        0
        (List.range 0 5)


setEmptyCol : Int -> Int -> Board -> Board
setEmptyCol row col board =
    let
        loop cnt c =
            if not <| get row c board then
                if cnt == 0 then
                    set row c board

                else
                    loop (cnt - 1) (c + 1)

            else if c >= 5 then
                board

            else
                loop cnt (c + 1)
    in
    loop row 0


emptyRows : Int -> Board -> Int
emptyRows col board =
    List.foldl
        (\row sum ->
            if get row col board then
                sum

            else
                sum + 1
        )
        0
        (List.range 0 5)


setEmptyRow : Int -> Int -> Board -> Board
setEmptyRow row col board =
    let
        loop cnt r =
            if not <| get r col board then
                if cnt == 0 then
                    set r col board

                else
                    loop (cnt - 1) (r + 1)

            else if r >= 5 then
                board

            else
                loop cnt (r + 1)
    in
    loop row 0



---
--- Rendering
---


tos : Int -> String
tos x =
    String.fromInt x


lineWidthO2 : Int
lineWidthO2 =
    3


lineWidth : Int
lineWidth =
    lineWidthO2 * 2


connectSizer : Style -> Int -> ( Int, String )
connectSizer style delta =
    ( connectWidth delta, connectColor style )


pathSizer : Style -> Int -> ( Int, String )
pathSizer style delta =
    ( pathWidth delta, pathColor style )


type alias Sizer =
    { connect : Style -> Int -> ( Int, String )
    , path : Style -> Int -> ( Int, String )
    }


wideSizer : Sizer
wideSizer =
    Sizer connectSizer pathSizer


narrowSizer : Sizer
narrowSizer =
    { connect = \style _ -> ( 2, connectColor style )
    , path = \style _ -> ( 2, pathColor style )
    }


type SizerKind
    = DefaultSizer
    | WideSizer


sizerKinds : List ( SizerKind, Sizer )
sizerKinds =
    [ ( DefaultSizer, narrowSizer )
    , ( WideSizer, wideSizer )
    ]


getSizer : SizerKind -> Sizer
getSizer kind =
    case LE.find (\( k, s ) -> k == kind) sizerKinds of
        Nothing ->
            narrowSizer

        Just ( _, s ) ->
            s


getConnectSizer : Maybe Sizer -> (Style -> Int -> ( Int, String ))
getConnectSizer sizer =
    (case sizer of
        Nothing ->
            getSizer DefaultSizer

        Just s ->
            s
    )
        |> .connect


getPathSizer : Maybe Sizer -> (Style -> Int -> ( Int, String ))
getPathSizer sizer =
    (case sizer of
        Nothing ->
            getSizer DefaultSizer

        Just s ->
            s
    )
        |> .path


render : Style -> Int -> (( Int, Int ) -> msg) -> Maybe Sizer -> Decoration -> Maybe Player -> Bool -> List ( Int, Int ) -> Board -> Html msg
render style size tagger sizer decoration player rotated path board =
    let
        sizeS =
            tos size

        delta =
            (size - lineWidth) // 6

        translate =
            tos <| delta * 3
    in
    svg
        [ width sizeS
        , height sizeS
        ]
        [ g
            (if rotated then
                [ transform
                    ("rotate(-90,"
                        ++ translate
                        ++ ","
                        ++ translate
                        ++ ")"
                    )
                ]

             else
                []
            )
          <|
            List.concat
                [ [ defs []
                        [ arrowMarker style ]
                  , drawCompass style delta rotated
                  ]
                , drawDirectionArrow style delta rotated player
                , drawRows style delta rotated
                , drawCols style delta rotated sizer board
                , drawPath style delta rotated sizer path
                , drawDecoration style delta rotated decoration
                , drawClickRects style delta rotated tagger
                ]
        ]


arrowMarker : Style -> Svg msg
arrowMarker style =
    marker
        [ Attributes.id "arrow"
        , viewBox "0 0 10 10"
        , refX "5"
        , refY "5"
        , markerWidth "4"
        , markerHeight "4"
        , orient "auto-start-reverse"
        , stroke style.arrowColor
        , fill style.arrowColor
        ]
        [ path [ d "M 0 0 L 10 5 L 0 10 z" ]
            []
        ]


drawDirectionArrow : Style -> Int -> Bool -> Maybe Player -> List (Svg msg)
drawDirectionArrow style delta rotated mplayer =
    case mplayer of
        Nothing ->
            []

        Just player ->
            let
                ( ( xx1, yy1 ), ( xx2, yy2 ) ) =
                    case player of
                        WhitePlayer ->
                            ( ( delta, delta )
                            , ( delta * 5, delta )
                            )

                        BlackPlayer ->
                            ( ( delta, delta )
                            , ( delta, delta * 5 )
                            )
            in
            [ Svg.line
                [ x1 <| tos xx1
                , y1 <| tosy delta rotated yy1
                , x2 <| tos xx2
                , y2 <| tosy delta rotated yy2
                , strokeWidth <| tos lineWidth
                , stroke style.arrowColor
                , markerStart "url(#arrow)"
                , markerEnd "url(#arrow)"
                ]
                []
            ]


drawRows : Style -> Int -> Bool -> List (Svg msg)
drawRows style delta rotated =
    List.map (drawRow style delta rotated) [ 0, 1, 2, 3, 4, 5 ]
        |> List.concat


fontSize : Int -> Int
fontSize delta =
    delta // 4


fontStyle : Int -> String
fontStyle fsize =
    "font-weight: bold; font-size: "
        ++ tos fsize
        ++ ";"


drawRow : Style -> Int -> Bool -> Int -> List (Svg msg)
drawRow style delta rotated idx =
    let
        yc =
            delta * idx + delta // 2

        ycs =
            tosy delta rotated yc

        fsize =
            fontSize delta
    in
    [ Svg.line
        [ x1 <| tos (delta // 2)
        , y1 ycs
        , x2 <| tos (delta * 5 + delta // 2)
        , y2 ycs
        , strokeWidth <| tos lineWidth
        , stroke style.lineColor
        ]
        []
    , let
        xs =
            "0"

        ys =
            if not rotated then
                tos (yc - 3 + fsize // 2)

            else
                tosy delta rotated (yc - fsize // 4)
      in
      Svg.text_
        (List.concat
            [ if rotated then
                [ rotate "90" ]

              else
                []
            , [ x "0"
              , y ys
              , Attributes.style <| fontStyle fsize
              , stroke style.lineColor
              , fill style.lineColor
              ]
            ]
        )
        [ Svg.text <| rowToString idx ]
    ]


drawClickRects : Style -> Int -> Bool -> (( Int, Int ) -> msg) -> List (Svg msg)
drawClickRects style delta rotated tagger =
    let
        indices =
            [ 0, 1, 2, 3, 4, 5 ]

        docol rowidx colidx res =
            drawClickRect delta tagger rowidx colidx
                :: res

        dorow rowidx res =
            List.foldl (docol rowidx) res indices
    in
    List.foldl dorow [] indices


drawClickRect : Int -> (( Int, Int ) -> msg) -> Int -> Int -> Svg msg
drawClickRect delta tagger rowidx colidx =
    let
        xc =
            delta * colidx + delta // 2

        yc =
            delta * rowidx + delta // 2
    in
    Svg.rect
        [ x <| tos (xc - delta // 2)
        , y <| tos (yc - delta // 2)
        , width <| tos delta
        , height <| tos delta
        , strokeWidth "0"
        , fillOpacity "0"
        , Events.onClick (tagger ( rowidx, colidx ))
        ]
        []


drawCols : Style -> Int -> Bool -> Maybe Sizer -> Board -> List (Svg msg)
drawCols style delta rotated sizer board =
    List.map (drawCol style delta rotated sizer board) [ 0, 1, 2, 3, 4, 5 ]
        |> List.concat


tosy : Int -> Bool -> Int -> String
tosy delta rotated y =
    tos <|
        if rotated then
            y
            --+ delta // 4

        else
            y


drawCol : Style -> Int -> Bool -> Maybe Sizer -> Board -> Int -> List (Svg msg)
drawCol style delta rotated sizer board idx =
    let
        xc =
            delta * idx + delta // 2

        xcs =
            tos xc

        fsize =
            fontSize delta

        ( xi, yi ) =
            if rotated then
                ( xc, fsize * 1 // 8 )

            else
                ( xc, delta * 6 )
    in
    List.concat
        [ [ Svg.line
                [ x1 xcs
                , y1 <| tosy delta rotated (delta // 2)
                , x2 xcs
                , y2 <| tosy delta rotated (delta * 5 + delta // 2)
                , strokeWidth <| tos lineWidth
                , stroke style.lineColor
                ]
                []
          , let
                xis =
                    tos xi

                yis =
                    tosy delta rotated yi
            in
            Svg.text_
                (List.concat
                    [ if rotated then
                        [ rotate "90" ]

                      else
                        []
                    , [ x xis
                      , y yis
                      , Attributes.style <| fontStyle fsize
                      , textAnchor "middle"
                      , stroke style.lineColor
                      , fill style.lineColor
                      ]
                    ]
                )
                [ Svg.text <| colToString idx ]
          ]
        , List.map (drawVertex style delta rotated sizer idx board) [ 0, 1, 2, 3, 4, 5 ]
            |> List.concat
        ]



---
--- Parameters for sizing and coloring connections and the winning path.
---


connectWidth : Int -> Int
connectWidth delta =
    -- Ensures that the connector is at least as wide as the grid
    -- at all screen sizes.
    --2
    (delta + 48) // 12


pathWidth : Int -> Int
pathWidth delta =
    connectWidth delta - 4


connectColor : Style -> String
connectColor style =
    style.lineColor


pathColor : Style -> String
pathColor style =
    style.pathColor


centers : Int -> Int -> Int -> ( Int, Int )
centers delta rowidx colidx =
    ( delta * colidx + delta // 2, delta * rowidx + delta // 2 )


drawVertex : Style -> Int -> Bool -> Maybe Sizer -> Int -> Board -> Int -> List (Svg msg)
drawVertex style delta rotated sizer colidx board rowidx =
    let
        ( xc, yc ) =
            centers delta rowidx colidx

        setp =
            get rowidx colidx board
    in
    [ [ Svg.circle
            [ cx <| tos xc
            , cy <| tosy delta rotated yc
            , r <| tos (delta // 6)
            , strokeWidth <| tos lineWidthO2
            , stroke style.lineColor
            , fill
                (if setp then
                    style.lineColor

                 else
                    style.backgroundColor
                )
            ]
            []
      ]
    , if not setp then
        []

      else
        drawConnections style delta rotated rowidx colidx (getConnectSizer sizer) board
    ]
        |> List.concat


drawConnections : Style -> Int -> Bool -> Int -> Int -> (Style -> Int -> ( Int, String )) -> Board -> List (Svg msg)
drawConnections style delta rotated rowidx colidx sizer board =
    let
        ( xc, yc ) =
            centers delta rowidx colidx

        ( lw, color ) =
            sizer style delta

        leftp =
            get rowidx (colidx - 1) board

        rightp =
            get rowidx (colidx + 1) board

        upp =
            get (rowidx - 1) colidx board

        downp =
            get (rowidx + 1) colidx board

        lrOverlap =
            if upp || downp then
                lw // 2

            else
                0

        udOverlap =
            if leftp || rightp then
                lw // 2

            else
                0

        xyr =
            lw // 2
    in
    [ if not <| leftp || rightp then
        []

      else
        let
            left =
                if leftp then
                    xc - delta // 2 - lw // 2

                else
                    xc - lrOverlap

            right =
                if rightp then
                    xc + delta // 2 + lw // 2

                else
                    xc + lrOverlap
        in
        [ Svg.rect
            [ x <| tos left
            , y <| tosy delta rotated (yc - lw // 2)
            , width <| tos (right - left)
            , height <| tos lw
            , strokeWidth "1"
            , stroke color
            , fill color
            , rx <| tos xyr
            ]
            []
        ]
    , if not <| upp || downp then
        []

      else
        let
            top =
                if upp then
                    yc - delta // 2 - lw // 2

                else
                    yc - udOverlap

            bottom =
                if downp then
                    yc + delta // 2 + lw // 2

                else
                    yc + udOverlap
        in
        [ Svg.rect
            [ x <| tos (xc - lw // 2)
            , y <| tosy delta rotated top
            , width <| tos lw
            , height <| tos (bottom - top)
            , strokeWidth "1"
            , stroke color
            , fill color
            , ry <| tos xyr
            ]
            []
        ]
    ]
        |> List.concat


drawDecoration : Style -> Int -> Bool -> Decoration -> List (Svg msg)
drawDecoration style delta rotated decoration =
    []


drawPath : Style -> Int -> Bool -> Maybe Sizer -> List ( Int, Int ) -> List (Svg msg)
drawPath style delta rotated sizer path =
    let
        board =
            List.foldl (\( r, c ) b -> set r c b) empty path

        psizer =
            getPathSizer sizer
    in
    List.map (\( r, c ) -> drawConnections style delta rotated r c psizer board) path
        |> List.concat


rowNameDict : Dict Int String
rowNameDict =
    Dict.fromList
        [ ( 0, "a" )
        , ( 1, "b" )
        , ( 2, "c" )
        , ( 3, "d" )
        , ( 4, "e" )
        , ( 5, "f" )
        ]


colToString : Int -> String
colToString y =
    case Dict.get y rowNameDict of
        Nothing ->
            String.fromInt y

        Just s ->
            s


rowToString : Int -> String
rowToString x =
    tos <| 6 - x


drawCompass : Style -> Int -> Bool -> Svg msg
drawCompass style delta rotated =
    let
        c =
            3 * delta

        csx =
            tos c

        csy =
            tosy delta rotated c

        thickness =
            lineWidth + 4

        outerR =
            round (toFloat delta * sqrt (1 / 2))

        innerR =
            outerR - delta // 6 - lineWidthO2 // 2

        innerX =
            round (toFloat innerR / sqrt 2)

        shortR =
            innerR // 3

        shortX =
            round (toFloat shortR / sqrt 2)

        shorterX =
            2 * shortX // 3

        r =
            outerR

        longR =
            r + delta // 6

        fsize =
            fontSize delta

        fr =
            longR + fsize // 4

        connection =
            tos shortX ++ "," ++ tos shortX

        arcStart =
            "0," ++ tos r

        arcAngle =
            70

        arcRotate =
            (arcAngle - 90) // 2

        arcRadians =
            degrees arcAngle

        arcEnd =
            (tos <| round (toFloat r * sin arcRadians))
                ++ ","
                ++ (tos <| round (toFloat r * cos arcRadians))

        ellipseRadii =
            tos r ++ "," ++ tos r

        quarterImage =
            g
                []
                [ Svg.polygon
                    [ points <|
                        "0,0 "
                            ++ ("0," ++ tos longR ++ " ")
                            ++ (connection ++ " ")
                            ++ "0,0"
                    , fill style.lineColor
                    , stroke style.lineColor
                    ]
                    []
                , Svg.polygon
                    [ points <|
                        (connection ++ " ")
                            ++ (tos innerX ++ "," ++ tos innerX ++ " ")
                            -- Should really compute "1" here
                            ++ (tos (shortX + shorterX) ++ "," ++ tos shortX ++ " ")
                            ++ connection
                    , fill style.compassColor
                    , stroke style.compassColor
                    ]
                    []
                , g
                    [ transform <|
                        "rotate("
                            ++ tos arcRotate
                            ++ ")"
                    ]
                    [ Svg.path
                        [ d <|
                            ("M " ++ arcStart ++ " ")
                                ++ ("A"
                                        ++ ellipseRadii
                                        ++ " 0 0,0 "
                                        ++ arcEnd
                                   )
                        , strokeWidth <| tos thickness
                        , stroke style.compassColor
                        , fill style.backgroundColor
                        ]
                        []
                    ]
                ]

        compassText cx cy txt =
            Svg.text_
                [ x <| tos cx
                , y <| tos cy
                , Attributes.style <| fontStyle fsize
                , textAnchor "middle"
                , stroke style.compassColor
                , fill style.compassColor
                ]
                [ Svg.text txt ]
    in
    g
        [ transform <|
            "translate("
                ++ csx
                ++ " "
                ++ csy
                ++ ")"
        , fillOpacity style.compassOpacity
        , strokeOpacity style.compassOpacity
        ]
        [ quarterImage
        , g [ transform "rotate(90)" ] [ quarterImage ]
        , g [ transform "rotate(180)" ] [ quarterImage ]
        , g [ transform "rotate(270)" ] [ quarterImage ]
        , compassText 0 (negate fr) "N"
        , compassText 0 (fr + 3 * fsize // 4) "S"
        , compassText (negate <| fr + fsize // 3) (fsize // 3) "W"
        , compassText (fr + fsize // 3) (fsize // 3) "E"
        ]
