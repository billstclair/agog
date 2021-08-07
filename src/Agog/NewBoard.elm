---------------------------------------------------------------------
--
-- NewBoard.elm
-- Zephyrnot board, storage and rendering.
-- Copyright (c) 2019-2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Agog.NewBoard exposing
    ( SizerKind(..)
    , colToString
    , count
    , empty
    , get
    , getSizer
    , initial
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
        , Color(..)
        , Decoration(..)
        , NewBoard
        , Piece
        , PieceType(..)
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


empty : NewBoard
empty =
    Array.repeat 8 (Array.repeat 8 Types.emptyPiece)


initial : NewBoard
initial =
    let
        whiteGolem =
            { color = WhiteColor
            , pieceType = Golem
            }

        whiteJourneyman =
            { whiteGolem | pieceType = Journeyman }

        blackGolem =
            { whiteGolem | color = BlackColor }

        blackJourneyman =
            { blackGolem | pieceType = Journeyman }

        fillGolems : NewBoard -> Piece -> Int -> Int -> Int -> NewBoard
        fillGolems b p col startRow endRow =
            if startRow > endRow then
                b

            else
                fillGolems
                    (set startRow col p b)
                    p
                    col
                    (startRow + 1)
                    endRow

        b0 =
            set 0 7 blackJourneyman (set 7 0 whiteJourneyman empty)

        b1 =
            fillGolems b0 whiteGolem 0 2 6

        b2 =
            fillGolems b1 whiteGolem 1 3 7

        b3 =
            fillGolems b2 whiteGolem 2 4 7

        b4 =
            fillGolems b3 whiteGolem 3 5 7

        b5 =
            fillGolems b4 whiteGolem 4 6 7

        b6 =
            fillGolems b5 whiteGolem 5 7 7

        b7 =
            fillGolems b6 blackGolem 2 0 0

        b8 =
            fillGolems b7 blackGolem 3 0 1

        b9 =
            fillGolems b8 blackGolem 4 0 2

        b10 =
            fillGolems b9 blackGolem 5 0 3

        b11 =
            fillGolems b10 blackGolem 6 0 4

        b12 =
            fillGolems b11 blackGolem 7 1 5
    in
    b12


count : NewBoard -> Int
count board =
    Array.toList board
        |> List.map Array.toList
        |> List.concat
        |> List.filter (\p -> p.pieceType /= NoPiece)
        |> List.length


score : NewBoard -> Int
score board =
    31 - count board


get : Int -> Int -> NewBoard -> Piece
get row col board =
    case Array.get row board of
        Nothing ->
            Types.emptyPiece

        Just r ->
            case Array.get col r of
                Nothing ->
                    Types.emptyPiece

                Just res ->
                    res


set : Int -> Int -> Piece -> NewBoard -> NewBoard
set row col piece board =
    case Array.get row board of
        Nothing ->
            board

        Just r ->
            Array.set row
                (Array.set col piece r)
                board


{-| It might be worthwhile to have an option to increment row or col first.
-}
winner : Player -> NewBoard -> ( Winner, List ( Int, Int ) )
winner player board =
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


simulateMove : Seed -> NewBoard -> Bool -> ( Seed, NewBoard )
simulateMove seed board isH =
    let
        gen =
            Random.int 0 5

        ( row, seed2 ) =
            Random.step gen seed

        ( col, seed3 ) =
            Random.step gen seed2

        ( board2, seed5 ) =
            let
                p =
                    get row col board
            in
            if p.pieceType /= NoPiece then
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
                ( set row col Types.emptyPiece board, seed3 )
    in
    ( seed5, board2 )


emptyCols : Int -> NewBoard -> Int
emptyCols row board =
    List.foldl
        (\col sum ->
            let
                p =
                    get row col board
            in
            if p.pieceType /= NoPiece then
                sum

            else
                sum + 1
        )
        0
        (List.range 0 5)


setEmptyCol : Int -> Int -> NewBoard -> NewBoard
setEmptyCol row col board =
    let
        loop cnt c =
            let
                p =
                    get row c board
            in
            if not <| p.pieceType == NoPiece then
                if cnt == 0 then
                    set row c Types.emptyPiece board

                else
                    loop (cnt - 1) (c + 1)

            else if c >= 5 then
                board

            else
                loop cnt (c + 1)
    in
    loop row 0


emptyRows : Int -> NewBoard -> Int
emptyRows col board =
    List.foldl
        (\row sum ->
            let
                p =
                    get row col board
            in
            if p.pieceType /= NoPiece then
                sum

            else
                sum + 1
        )
        0
        (List.range 0 5)


setEmptyRow : Int -> Int -> NewBoard -> NewBoard
setEmptyRow row col board =
    let
        loop cnt r =
            let
                p =
                    get r col board
            in
            if p.pieceType /= NoPiece then
                if cnt == 0 then
                    set r col Types.emptyPiece board

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
    1


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


render : Style -> Int -> (( Int, Int ) -> msg) -> Maybe Sizer -> Decoration -> Maybe Player -> Bool -> List ( Int, Int ) -> NewBoard -> Html msg
render style size tagger sizer decoration player notRotated path board =
    let
        rotated =
            False

        whiteSpace =
            5

        innerSize =
            size - (2 * whiteSpace)

        sizeS =
            tos size

        delta =
            round (toFloat (innerSize - lineWidth) / 8 / sqrt 2)

        translate =
            (innerSize - 8 * delta) // 2

        center =
            innerSize // 2
    in
    svg
        [ width sizeS
        , height sizeS
        ]
        [ g
            [ transform
                ("translate("
                    ++ tos whiteSpace
                    ++ " "
                    ++ tos whiteSpace
                    ++ ")"
                )
            ]
            [ g
                [ transform
                    (" translate("
                        ++ tos translate
                        ++ " "
                        ++ tos 0
                        ++ ")"
                        ++ " rotate(-45,"
                        ++ tos center
                        ++ " "
                        ++ tos center
                        ++ ")"
                    )
                ]
              <|
                List.concat
                    [ drawRows style delta rotated
                    , drawCols style delta rotated sizer board
                    , drawRects style board delta
                    , drawClickRects style delta rotated tagger
                    ]
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


drawRows : Style -> Int -> Bool -> List (Svg msg)
drawRows style delta rotated =
    List.map (drawRow style delta rotated) [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        |> List.concat


fontSize : Int -> Int
fontSize delta =
    delta // 2


fontStyle : Int -> String
fontStyle fsize =
    "font-size: "
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
    [ if idx == 0 || idx == 8 then
        Svg.line
            [ x1 <| tos (delta // 2 - lineWidth // 2)
            , y1 ycs
            , x2 <| tos (delta * 8 + delta // 2 + lineWidth // 2)
            , y2 ycs
            , strokeWidth <| tos lineWidth
            , stroke style.lineColor
            ]
            []

      else
        g [] []
    , if idx >= 8 then
        g [] []

      else
        let
            xs =
                "0"

            ys =
                if not rotated then
                    tos (yc - 3 + (delta // 2) + (fsize // 2))

                else
                    -- Needs to be fixed
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


drawRects : Style -> NewBoard -> Int -> List (Svg msg)
drawRects style board delta =
    let
        indices =
            [ 0, 1, 2, 3, 4, 5, 6, 7 ]

        docol rowidx colidx res =
            drawRect style board delta rowidx colidx
                :: res

        dorow rowidx res =
            List.foldl (docol rowidx) res indices
    in
    List.foldl dorow [] indices


drawRect : Style -> NewBoard -> Int -> Int -> Int -> Svg msg
drawRect style board delta rowidx colidx =
    g []
        [ drawShadeRect style delta rowidx colidx
        , drawPiece style board delta rowidx colidx
        ]


drawCircle : Style -> Color -> Float -> Float -> Float -> Svg msg
drawCircle style color delta rowidx colidx =
    let
        xc =
            delta * colidx + delta

        yc =
            delta * rowidx + delta

        diameter =
            delta * 9 / 16

        colorString =
            case color of
                BlackColor ->
                    "Sienna"

                WhiteColor ->
                    "WhiteSmoke"
    in
    Svg.circle
        [ cx <| String.fromFloat xc
        , cy <| String.fromFloat yc
        , r <| String.fromFloat (diameter / 2)
        , stroke "Black"
        , strokeWidth "1"
        , fill colorString
        ]
        []


drawPiece : Style -> NewBoard -> Int -> Int -> Int -> Svg msg
drawPiece style board delta rowidx colidx =
    let
        piece =
            get rowidx colidx board

        deltaF =
            toFloat delta

        rowidxF =
            toFloat rowidx

        colidxF =
            toFloat colidx

        offset =
            1.0 / 8.0

        draw color pieceType =
            case pieceType of
                NoPiece ->
                    g [] []

                Golem ->
                    drawCircle style color deltaF rowidxF colidxF

                Hulk ->
                    g []
                        [ drawCircle style color deltaF rowidxF colidxF
                        , drawCircle style color deltaF (rowidxF - offset) (colidxF + offset)
                        ]

                CorruptedHulk ->
                    g []
                        [ drawCircle style (Types.otherColor color) deltaF rowidxF colidxF
                        , drawCircle style color deltaF (rowidxF - offset) (colidxF + offset)
                        ]

                Journeyman ->
                    g []
                        [ drawCircle style color deltaF rowidxF colidxF
                        , drawCircle style (Types.otherColor color) deltaF (rowidxF - offset) (colidxF + offset)
                        , drawCircle style color deltaF (rowidxF - 2 * offset) (colidxF + 2 * offset)
                        ]
    in
    draw piece.color piece.pieceType


drawShadeRect : Style -> Int -> Int -> Int -> Svg msg
drawShadeRect style delta rowidx colidx =
    let
        idxsum =
            rowidx + colidx
    in
    if idxsum == idxsum // 2 * 2 then
        g [] []

    else
        let
            xoff =
                if colidx == 0 then
                    lineWidth // 2

                else
                    0

            yoff =
                if rowidx == 0 then
                    lineWidth // 2

                else
                    0

            xreduce =
                if colidx == 7 then
                    lineWidth // 2

                else
                    0

            yreduce =
                if rowidx == 7 then
                    lineWidth // 2

                else
                    0

            xc =
                delta * colidx + delta // 2 + xoff

            yc =
                delta * rowidx + delta // 2 + yoff
        in
        Svg.rect
            [ x <| tos xc
            , y <| tos yc
            , width <| tos (delta - xoff - xreduce)
            , height <| tos (delta - yoff - yreduce)
            , fill style.shadeColor
            ]
            []


drawClickRects : Style -> Int -> Bool -> (( Int, Int ) -> msg) -> List (Svg msg)
drawClickRects style delta rotated tagger =
    let
        indices =
            [ 0, 1, 2, 3, 4, 5, 6, 7 ]

        docol rowidx colidx res =
            drawClickRect style delta tagger rowidx colidx
                :: res

        dorow rowidx res =
            List.foldl (docol rowidx) res indices
    in
    List.foldl dorow [] indices


drawClickRect : Style -> Int -> (( Int, Int ) -> msg) -> Int -> Int -> Svg msg
drawClickRect style delta tagger rowidx colidx =
    let
        xc =
            delta * colidx + delta // 2

        yc =
            delta * rowidx + delta // 2
    in
    Svg.rect
        [ x <| tos xc
        , y <| tos yc
        , width <| tos delta
        , height <| tos delta
        , strokeWidth "0"
        , fillOpacity "0"
        , Events.onClick (tagger ( rowidx, colidx ))
        ]
        []


drawCols : Style -> Int -> Bool -> Maybe Sizer -> NewBoard -> List (Svg msg)
drawCols style delta rotated sizer board =
    List.map (drawCol style delta rotated sizer board) [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        |> List.concat


tosy : Int -> Bool -> Int -> String
tosy delta rotated y =
    tos <|
        if rotated then
            y
            --+ delta // 4

        else
            y


drawCol : Style -> Int -> Bool -> Maybe Sizer -> NewBoard -> Int -> List (Svg msg)
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
                ( xc + delta // 2, delta * 9 )
    in
    List.concat
        [ [ if idx == 0 || idx == 8 then
                Svg.line
                    [ x1 xcs
                    , y1 <| tosy delta rotated (delta // 2)
                    , x2 xcs
                    , y2 <| tosy delta rotated (delta * 8 + delta // 2)
                    , strokeWidth <| tos lineWidth
                    , stroke style.lineColor
                    ]
                    []

            else
                g [] []
          , if idx >= 8 then
                g [] []

            else
                let
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


rowNameDict : Dict Int String
rowNameDict =
    Dict.fromList
        [ ( 0, "a" )
        , ( 1, "b" )
        , ( 2, "c" )
        , ( 3, "d" )
        , ( 4, "e" )
        , ( 5, "f" )
        , ( 6, "g" )
        , ( 7, "h" )
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
    tos <| 8 - x
