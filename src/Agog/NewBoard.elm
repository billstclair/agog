---------------------------------------------------------------------
--
-- NewBoard.elm
-- AGOG board, storage and rendering.
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
    , populateLegalMoves
    , rc
    , render
    , rowToString
    , score
    , set
    , winner
    )

import Agog.Types as Types
    exposing
        ( Board
        , Color(..)
        , GameState
        , JumpSequence
        , MovesOrJumps(..)
        , NewBoard
        , OneJump
        , Piece
        , PieceType(..)
        , Player(..)
        , RowCol
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


rc : Int -> Int -> RowCol
rc row col =
    { row = row, col = col }


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
                    (set (rc startRow col) p b)
                    p
                    col
                    (startRow + 1)
                    endRow

        b0 =
            set (rc 0 7)
                blackJourneyman
                (set (rc 7 0) whiteJourneyman empty)

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


get : RowCol -> NewBoard -> Piece
get { row, col } board =
    case Array.get row board of
        Nothing ->
            Types.emptyPiece

        Just r ->
            case Array.get col r of
                Nothing ->
                    Types.emptyPiece

                Just res ->
                    res


set : RowCol -> Piece -> NewBoard -> NewBoard
set { row, col } piece board =
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


render : Style -> Int -> (( Int, Int ) -> msg) -> Maybe Sizer -> Maybe Player -> Bool -> GameState -> Html msg
render style size tagger sizer player notRotated gameState =
    let
        board =
            gameState.newBoard

        legalMoves =
            gameState.legalMoves

        jumps =
            gameState.jumps

        selected =
            gameState.selected

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
                    , drawRects style selected legalMoves jumps board delta
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


removeBlankGs : List (Svg msg) -> List (Svg msg)
removeBlankGs svgs =
    List.filter ((/=) (g [] [])) svgs


drawRects : Style -> Maybe RowCol -> MovesOrJumps -> List OneJump -> NewBoard -> Int -> List (Svg msg)
drawRects style selected legalMoves jumps board delta =
    let
        indices =
            [ 0, 1, 2, 3, 4, 5, 6, 7 ]

        docol f rowidx colidx res =
            f delta rowidx colidx
                :: res

        dorow f rowidx res =
            List.foldl (docol f rowidx) res indices
    in
    (List.foldr (dorow (drawShadeRect style)) [] indices
        |> removeBlankGs
    )
        ++ drawHighlights style selected legalMoves delta
        ++ (List.foldr (dorow (drawPiece style board)) [] indices
                |> removeBlankGs
           )
        ++ drawJumps style board jumps delta


drawHighlight : String -> Int -> RowCol -> Svg msg
drawHighlight color delta { row, col } =
    let
        p =
            shadeRectParams delta row col

        w =
            4

        xoff =
            if col == 0 then
                (w - lineWidth) // 2

            else
                0

        xreduce =
            if col == 7 then
                (w - lineWidth) // 2

            else
                0

        yoff =
            if row == 0 then
                (w - lineWidth) // 2

            else
                0

        yreduce =
            if row == 7 then
                (w - lineWidth) // 2

            else
                0
    in
    Svg.rect
        [ x (tos <| p.x + xoff)
        , y (tos <| p.y + yoff)
        , width (tos <| p.width - xreduce)
        , height (tos <| p.height - yreduce)
        , strokeWidth <| tos w
        , stroke color
        , fillOpacity "0"
        ]
        []


drawHighlights : Style -> Maybe RowCol -> MovesOrJumps -> Int -> List (Svg msg)
drawHighlights style selected legalMoves delta =
    (case legalMoves of
        Moves rowcols ->
            List.map (drawHighlight style.moveColor delta) rowcols

        Jumps sequences ->
            let
                addSequence sequence res =
                    case List.head sequence of
                        Nothing ->
                            res

                        Just { to } ->
                            drawHighlight style.moveColor delta to :: res
            in
            List.foldr addSequence [] sequences
    )
        ++ (case selected of
                Nothing ->
                    []

                Just rowcol ->
                    [ drawHighlight style.selectedColor delta rowcol ]
           )


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
        rowcol =
            rc rowidx colidx

        piece =
            get rowcol board

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


type alias ShadeRectParams =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


shadeRectParams : Int -> Int -> Int -> ShadeRectParams
shadeRectParams delta rowidx colidx =
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
    in
    { x = delta * colidx + delta // 2 + xoff
    , y = delta * rowidx + delta // 2 + yoff
    , width = delta - xoff - xreduce
    , height = delta - yoff - yreduce
    }


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
            p =
                shadeRectParams delta rowidx colidx
        in
        Svg.rect
            [ x <| tos p.x
            , y <| tos p.y
            , width <| tos p.width
            , height <| tos p.height
            , fill style.shadeColor
            ]
            []


drawJumps : Style -> NewBoard -> List OneJump -> Int -> List (Svg msg)
drawJumps style board jumps delta =
    let
        color =
            style.selectedColor

        deltaF =
            toFloat delta

        offset =
            1.0 / 8.0

        drawJump oneJump =
            let
                { row, col } =
                    oneJump.over

                rowidxF =
                    toFloat row

                colidxF =
                    toFloat col

                piece =
                    get oneJump.over board

                ( rowF, colF ) =
                    case piece.pieceType of
                        Hulk ->
                            ( rowidxF - offset, colidxF + offset )

                        CorruptedHulk ->
                            ( rowidxF - offset, colidxF + offset )

                        Journeyman ->
                            ( rowidxF - 2 * offset, colidxF + 2 * offset )

                        _ ->
                            ( rowidxF, colidxF )

                xc =
                    deltaF * colidxF + deltaF

                yc =
                    deltaF * rowidxF + deltaF

                len =
                    deltaF * (9.0 / 16.0) * (2.0 / 3.0)

                leno2 =
                    len / 2

                w =
                    "3"
            in
            g []
                [ Svg.line
                    [ x1 <| tos (round <| xc - leno2)
                    , x2 <| tos (round <| xc + leno2)
                    , y1 <| tos (round yc)
                    , y2 <| tos (round yc)
                    , strokeWidth w
                    , stroke color
                    ]
                    []
                , Svg.line
                    [ y1 <| tos (round <| yc - leno2)
                    , y2 <| tos (round <| yc + leno2)
                    , x1 <| tos (round xc)
                    , x2 <| tos (round xc)
                    , strokeWidth w
                    , stroke color
                    ]
                    []
                ]
    in
    List.map drawJump jumps


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


mapAllNeighbors : (RowCol -> Piece -> a -> a) -> NewBoard -> RowCol -> a -> a
mapAllNeighbors =
    mapNeighbors True WhiteColor


mapForwardNeighbors : Color -> (RowCol -> Piece -> a -> a) -> NewBoard -> RowCol -> a -> a
mapForwardNeighbors color =
    mapNeighbors False color


mapNeighbors : Bool -> Color -> (RowCol -> Piece -> a -> a) -> NewBoard -> RowCol -> a -> a
mapNeighbors all color mapper board startPos init =
    let
        map r c res =
            let
                neighborPos =
                    rc r c
            in
            if not <| isValidRowCol neighborPos then
                res

            else
                mapper neighborPos
                    (get neighborPos board)
                    res

        ( plusRow, plusCol ) =
            case color of
                WhiteColor ->
                    ( -1, 1 )

                BlackColor ->
                    ( 1, -1 )

        { row, col } =
            startPos
    in
    (if all then
        map (row - plusRow) col init
            |> map row (col - plusCol)

     else
        init
    )
        |> map row (col + plusCol)
        |> map (row + plusRow) col


isValidRowCol : RowCol -> Bool
isValidRowCol { row, col } =
    row >= 0 && row < 8 && col >= 0 && col < 8


stepAgain : RowCol -> RowCol -> RowCol
stepAgain from to =
    if from.row < to.row then
        rc (to.row + 1) to.col

    else if from.row > to.row then
        rc (to.row - 1) to.col

    else if from.col < to.col then
        rc to.row (to.col + 1)

    else if from.col > to.col then
        rc to.row (to.col - 1)

    else
        { row = -1, col = -1 }


populateLegalMoves : GameState -> GameState
populateLegalMoves gameState =
    let
        { newBoard, selected } =
            gameState
    in
    { gameState
        | legalMoves = computeLegalMoves newBoard selected
    }


computeLegalMoves : NewBoard -> Maybe RowCol -> MovesOrJumps
computeLegalMoves newBoard selected =
    case selected of
        Nothing ->
            Moves []

        Just rowCol ->
            case legalJumps newBoard rowCol of
                [] ->
                    Moves <| legalSlides newBoard rowCol

                jumpSequences ->
                    Jumps jumpSequences


isHulkType : PieceType -> Bool
isHulkType pieceType =
    pieceType == Hulk || pieceType == CorruptedHulk


legalJumps : NewBoard -> RowCol -> List JumpSequence
legalJumps board startPos =
    let
        { color, pieceType } =
            get startPos board

        res =
            if pieceType == NoPiece then
                []

            else
                let
                    board2 =
                        board |> set startPos Types.emptyPiece
                in
                if isHulkType pieceType then
                    computeLongJumpSequences color board2 startPos

                else
                    computeJumpSequences color board2 startPos
    in
    removeNonMaximalJumpSequences res


removeNonMaximalJumpSequences : List JumpSequence -> List JumpSequence
removeNonMaximalJumpSequences jumpSequences =
    let
        maxlen =
            List.foldr
                (\seq res ->
                    max res <| List.length seq
                )
                0
                jumpSequences
    in
    List.filter (\l -> maxlen == List.length l) jumpSequences


computeJumpSequences : Color -> NewBoard -> RowCol -> List JumpSequence
computeJumpSequences color board startPos =
    let
        mapper : RowCol -> Piece -> List JumpSequence -> List JumpSequence
        mapper jumpedPos jumpedPiece res =
            if color == jumpedPiece.color || NoPiece == jumpedPiece.pieceType then
                res

            else
                let
                    landingPos =
                        stepAgain startPos jumpedPos
                in
                if not <| isValidRowCol landingPos then
                    res

                else
                    let
                        landingPiece =
                            get landingPos board
                    in
                    if landingPiece.pieceType /= NoPiece then
                        res

                    else
                        let
                            jump =
                                { over = jumpedPos
                                , to = landingPos
                                }

                            moreJumps =
                                computeJumpSequences color
                                    (set jumpedPos
                                        -- prevents second jump of same piece.
                                        { jumpedPiece | color = color }
                                        board
                                    )
                                    landingPos
                        in
                        if moreJumps == [] then
                            [ jump ] :: res

                        else
                            List.map (\seq -> jump :: seq) moreJumps
                                ++ res
    in
    mapAllNeighbors mapper board startPos []


computeLongJumpSequences : Color -> NewBoard -> RowCol -> List JumpSequence
computeLongJumpSequences color board startPos =
    let
        findLastPos : RowCol -> RowCol -> Maybe ( RowCol, Piece )
        findLastPos from towards =
            let
                pos =
                    stepAgain from towards
            in
            if not <| isValidRowCol pos then
                Nothing

            else
                let
                    piece =
                        get pos board
                in
                if piece.pieceType == NoPiece then
                    findLastPos towards pos

                else
                    Just ( pos, piece )

        mapper : RowCol -> Piece -> List JumpSequence -> List JumpSequence
        mapper pos piece res =
            let
                maybeJumpedPair =
                    if piece.pieceType /= NoPiece then
                        Just ( pos, piece )

                    else
                        findLastPos startPos pos
            in
            case maybeJumpedPair of
                Nothing ->
                    res

                Just ( jumpedPos, jumpedPiece ) ->
                    if color == jumpedPiece.color then
                        res

                    else
                        let
                            getLandingPoss : RowCol -> List RowCol -> List RowCol
                            getLandingPoss lpos poss =
                                let
                                    landingPos =
                                        stepAgain startPos lpos
                                in
                                if not <| isValidRowCol landingPos then
                                    poss

                                else
                                    let
                                        landingPiece =
                                            get landingPos board
                                    in
                                    if landingPiece.pieceType /= NoPiece then
                                        poss

                                    else
                                        getLandingPoss landingPos <|
                                            (landingPos :: poss)

                            landingPoss =
                                getLandingPoss jumpedPos []

                            landingPosMapper : RowCol -> List JumpSequence -> List JumpSequence
                            landingPosMapper lpos jumpSequences =
                                let
                                    jump =
                                        { over = jumpedPos
                                        , to = lpos
                                        }

                                    moreJumps =
                                        computeLongJumpSequences color
                                            (set jumpedPos
                                                -- prevents second jump of same piece.
                                                { jumpedPiece | color = color }
                                                board
                                            )
                                            lpos
                                in
                                if moreJumps == [] then
                                    [ jump ] :: jumpSequences

                                else
                                    List.map (\seq -> jump :: seq) moreJumps
                                        ++ jumpSequences
                        in
                        List.foldr landingPosMapper [] landingPoss
                            ++ res
    in
    mapAllNeighbors mapper
        (board |> set startPos Types.emptyPiece)
        startPos
        []


legalSlides : NewBoard -> RowCol -> List RowCol
legalSlides board startPos =
    let
        { color, pieceType } =
            get startPos board
    in
    if pieceType == NoPiece then
        []

    else if isHulkType pieceType then
        computeLongSlides color board startPos

    else
        let
            mapper : RowCol -> Piece -> List RowCol -> List RowCol
            mapper slidePos piece res =
                if piece.pieceType == NoPiece then
                    slidePos :: res

                else
                    res
        in
        mapForwardNeighbors color mapper board startPos []


computeLongSlides : Color -> NewBoard -> RowCol -> List RowCol
computeLongSlides color board startPos =
    let
        getSlide pos res =
            if not <| isValidRowCol pos then
                res

            else if (get pos board |> .pieceType) /= NoPiece then
                res

            else
                getSlide (stepAgain startPos pos) (pos :: res)

        mapper : RowCol -> Piece -> List RowCol -> List RowCol
        mapper pos _ res =
            getSlide pos res
    in
    mapForwardNeighbors color mapper board startPos []
