--------------------------------------------------------------------
--
-- Board.elm
-- AGOG board, storage and rendering.
-- Copyright (c) 2019-2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Agog.Board exposing
    ( archiveGame
    , areMovesAJump
    , blackSanctum
    , clear
    , colToString
    , computeJumperLocations
    , computeWinner
    , count
    , countColor
    , empty
    , findSquareSatisfying
    , get
    , illegalRowCol
    , initial
    , isRowColLegal
    , isUniqueMoveTo
    , mapWholeBoard
    , mapWholeBoardWithExit
    , playerSanctum
    , populateLegalMoves
    , rc
    , render
    , rowColToString
    , rowToString
    , set
    , stringToCol
    , stringToRow
    , stringToRowCol
    , unarchiveGame
    , whiteSanctum
    )

import Agog.Types as Types
    exposing
        ( ArchivedGame
        , Board
        , Color(..)
        , GameState
        , HulkAfterJump(..)
        , JumpSequence
        , MovesOrJumps(..)
        , OneCorruptibleJump
        , OneJump
        , OneMove
        , OneMoveSequence(..)
        , Piece
        , PieceType(..)
        , Player(..)
        , RowCol
        , SavedModel
        , Style
        , WinReason(..)
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
    Array.repeat 8 (Array.repeat 8 Types.emptyPiece)


rc : Int -> Int -> RowCol
rc row col =
    { row = row, col = col }


blackSanctum : RowCol
blackSanctum =
    rc 0 7


whiteSanctum : RowCol
whiteSanctum =
    rc 7 0


playerSanctum : Player -> RowCol
playerSanctum player =
    case player of
        WhitePlayer ->
            whiteSanctum

        BlackPlayer ->
            blackSanctum


whiteGolem : Piece
whiteGolem =
    { color = WhiteColor
    , pieceType = Golem
    }


blackGolem : Piece
blackGolem =
    { whiteGolem | color = BlackColor }


whiteJourneyman : Piece
whiteJourneyman =
    { whiteGolem | pieceType = Journeyman }


blackJourneyman : Piece
blackJourneyman =
    { blackGolem | pieceType = Journeyman }


initial : Board
initial =
    let
        fillGolems : Board -> Piece -> Int -> Int -> Int -> Board
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
            set blackSanctum
                blackJourneyman
                (set whiteSanctum whiteJourneyman empty)

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


count : Board -> Int
count board =
    Array.toList board
        |> List.map Array.toList
        |> List.concat
        |> List.filter (\p -> p.pieceType /= NoPiece)
        |> List.length


get : RowCol -> Board -> Piece
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


clear : RowCol -> Board -> Board
clear rowCol board =
    set rowCol Types.emptyPiece board


set : RowCol -> Piece -> Board -> Board
set { row, col } piece board =
    case Array.get row board of
        Nothing ->
            board

        Just r ->
            Array.set row
                (Array.set col piece r)
                board


{-| This finds only WinBySanctum and WinByCapture.
The other two are discovered during play in Interface.chooseMove.
-}
computeWinner : Player -> Board -> Winner
computeWinner whoseTurn board =
    if get whiteSanctum board == blackJourneyman then
        BlackWinner WinBySanctum

    else if get blackSanctum board == whiteJourneyman then
        WhiteWinner WinBySanctum

    else
        let
            color =
                Types.playerColor whoseTurn

            mapper rowCol piece found =
                let
                    ( fw, fb, fm ) =
                        found

                    fm2 =
                        if fm then
                            True

                        else if color == (get rowCol board |> .color) then
                            if NoMoves /= computeLegalMoves board (Just rowCol) then
                                True

                            else
                                False

                        else
                            fm
                in
                if piece == whiteJourneyman then
                    ( True, fb, fm2 )

                else if piece == blackJourneyman then
                    ( fw, True, fm2 )

                else if fm /= fm2 then
                    ( fw, fb, fm2 )

                else
                    found

            ( foundWhite, foundBlack, foundMove ) =
                mapWholeBoard mapper board ( False, False, False )
        in
        if not foundWhite then
            BlackWinner WinByCapture

        else if not foundBlack then
            WhiteWinner WinByCapture

        else if not foundMove then
            case whoseTurn of
                WhitePlayer ->
                    BlackWinner WinByImmobilization

                BlackPlayer ->
                    WhiteWinner WinByImmobilization

        else
            NoWinner


areMovesAJump : MovesOrJumps -> Bool
areMovesAJump legalMoves =
    case legalMoves of
        Jumps _ ->
            True

        _ ->
            False


isUniqueMoveTo : RowCol -> RowCol -> Maybe Piece -> Bool -> Board -> Bool
isUniqueMoveTo from to maybePiece isJump board =
    let
        movingPiece =
            case maybePiece of
                Just p ->
                    p

                Nothing ->
                    get from board

        mapper : RowCol -> Piece -> Bool -> ( Bool, Bool )
        mapper loc piece res =
            if from == loc then
                ( res, False )

            else if piece /= movingPiece then
                ( res, False )

            else
                case computeLegalMoves board <| Just loc of
                    NoMoves ->
                        ( res, False )

                    Moves slides ->
                        if not isJump && List.any ((==) to) slides then
                            ( False, True )

                        else
                            ( res, False )

                    Jumps sequences ->
                        if
                            isJump
                                && List.any
                                    (\seq ->
                                        case List.head seq of
                                            Nothing ->
                                                False

                                            Just oneJump ->
                                                oneJump.to == to
                                    )
                                    sequences
                        then
                            ( False, True )

                        else
                            ( res, False )
    in
    mapWholeBoardWithExit mapper board True



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


render : Style -> Int -> (( Int, Int ) -> msg) -> Maybe Player -> Bool -> GameState -> Html msg
render style size tagger player rotated gameState =
    let
        board =
            gameState.newBoard

        { jumperLocations, legalMoves, jumps, selected } =
            gameState

        whiteSpace =
            10

        innerSize =
            size - (2 * whiteSpace)

        sizeS =
            tos size

        delta =
            round (toFloat (innerSize - lineWidth) / 8 / sqrt 2)

        center =
            4 * delta + fontSize delta

        translate =
            round (toFloat center * (sqrt 2 - 1) / 2) + whiteSpace
    in
    svg
        [ width sizeS
        , height sizeS
        ]
        [ g
            [ transform
                ("translate("
                    ++ tos (whiteSpace + translate)
                    ++ " "
                    ++ tos (whiteSpace + translate)
                    ++ ")"
                )
            ]
            [ g
                [ transform
                    ("rotate("
                        ++ (if rotated then
                                "135,"

                            else
                                "-45,"
                           )
                        ++ (tos center ++ "," ++ tos center)
                        ++ ")"
                    )
                ]
              <|
                List.concat
                    [ drawRows style delta rotated
                    , drawCols style delta rotated board
                    , drawRects style selected jumperLocations legalMoves jumps (List.head gameState.moves) board rotated delta
                    , drawClickRects style delta rotated tagger
                    ]
            ]
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
            tosy delta False yc

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
                tos (yc - 3 + (delta // 2) + (fsize // 2))
        in
        Svg.text_
            (List.concat
                [ if False then
                    --rotated then
                    [ rotate "180" ]

                  else
                    []
                , [ x "0"
                  , y ys
                  , Attributes.style <| fontStyle fsize

                  --, textAnchor "middle"
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


indices : List Int
indices =
    [ 0, 1, 2, 3, 4, 5, 6, 7 ]


drawRects : Style -> Maybe RowCol -> List RowCol -> MovesOrJumps -> List OneCorruptibleJump -> Maybe OneMove -> Board -> Bool -> Int -> List (Svg msg)
drawRects style selected jumperLocations legalMoves jumps maybeLastMove board rotated delta =
    let
        docol f rowidx colidx res =
            f delta rowidx colidx
                :: res

        dorow f rowidx res =
            List.foldl (docol f rowidx) res indices
    in
    (List.foldr (dorow (drawShadeRect style)) [] indices
        |> removeBlankGs
    )
        ++ highlightLastMove style selected maybeLastMove delta
        ++ drawHighlights style selected jumperLocations legalMoves delta
        ++ (drawSomeRemovedPieces style rotated WhiteColor delta <|
                (23 - countColor WhiteColor board)
           )
        ++ (drawSomeRemovedPieces style rotated BlackColor delta <|
                (23 - countColor BlackColor board)
           )
        ++ (List.foldr (dorow (drawPiece style board rotated)) [] indices
                |> removeBlankGs
           )
        ++ drawJumps style board jumps delta


highlightLastMove : Style -> Maybe RowCol -> Maybe OneMove -> Int -> List (Svg msg)
highlightLastMove style selected maybeLastMove delta =
    case selected of
        Just _ ->
            []

        Nothing ->
            case maybeLastMove of
                Nothing ->
                    []

                Just oneMove ->
                    case oneMove.sequence of
                        OneResign ->
                            []

                        OneSlide { from, to, makeHulk } ->
                            [ drawHighlight style.lastMoveFromColor delta from
                            , drawHighlight style.lastMoveToColor delta to
                            ]
                                ++ (case makeHulk of
                                        Nothing ->
                                            []

                                        Just hulkPos ->
                                            [ drawHighlight style.lastMoveToColor delta hulkPos ]
                                   )

                        OneJumpSequence jumps ->
                            let
                                mapper : OneCorruptibleJump -> List (Svg msg) -> List (Svg msg)
                                mapper { from, over, to, hulkAfterJump } res =
                                    (if res == [] then
                                        [ drawHighlight style.lastMoveFromColor
                                            delta
                                            from
                                        ]

                                     else
                                        []
                                    )
                                        ++ [ drawHighlight style.lastMoveToColor
                                                delta
                                                to
                                           ]
                                        ++ (case hulkAfterJump of
                                                NoHulkAfterJump ->
                                                    []

                                                CorruptAfterJump ->
                                                    [ drawHighlight style.lastMoveToColor
                                                        delta
                                                        over
                                                    ]

                                                MakeHulkAfterJump hulkPos ->
                                                    [ drawHighlight
                                                        style.lastMoveToColor
                                                        delta
                                                        hulkPos
                                                    ]
                                           )
                                        ++ res
                            in
                            List.foldl mapper [] jumps


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


drawHighlights : Style -> Maybe RowCol -> List RowCol -> MovesOrJumps -> Int -> List (Svg msg)
drawHighlights style selected jumperLocations legalMoves delta =
    (case legalMoves of
        NoMoves ->
            []

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
                    List.map (drawHighlight style.moveColor delta) jumperLocations

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


drawRemovedPieces : Style -> Bool -> Color -> Int -> RowCol -> Int -> Svg msg
drawRemovedPieces style rotated color delta rowcol cnt =
    let
        { row, col } =
            rowcol

        deltaF =
            toFloat delta

        rowidxF =
            toFloat row

        colidxF =
            toFloat col

        offset =
            (1.0 / 8.0)
                * (if rotated then
                    -1

                   else
                    1
                  )

        draw offsetCnt res =
            let
                rowF =
                    rowidxF - offsetCnt * offset

                colF =
                    colidxF + offsetCnt * offset
            in
            drawCircle style color deltaF rowF colF
                :: res

        range =
            List.range 0 (cnt - 1)
                |> List.map toFloat
    in
    List.foldl draw [] range
        |> List.reverse
        |> g []


whiteRemovedPieceLocs : List RowCol
whiteRemovedPieceLocs =
    [ rc 1 8, rc 2 8, rc 3 8, rc 4 8, rc 5 8, rc 6 8, rc 2 9, rc 3 9 ]


blackRemovedPieceLocs : List RowCol
blackRemovedPieceLocs =
    [ rc -1 6, rc -1 5, rc -1 4, rc -1 3, rc -1 2, rc -1 1, rc -2 5, rc -2 4 ]


drawSomeRemovedPieces : Style -> Bool -> Color -> Int -> Int -> List (Svg msg)
drawSomeRemovedPieces style rotated color delta cnt =
    let
        loop : Int -> List RowCol -> List (Svg msg) -> List (Svg msg)
        loop remaining locs res =
            if remaining <= 0 then
                res

            else
                case locs of
                    [] ->
                        res

                    loc :: tail ->
                        loop (remaining - 3)
                            tail
                        <|
                            (drawRemovedPieces style rotated color delta loc <|
                                min 3 remaining
                            )
                                :: res

        locations =
            case color of
                WhiteColor ->
                    whiteRemovedPieceLocs

                BlackColor ->
                    blackRemovedPieceLocs
    in
    loop cnt locations []


drawPiece : Style -> Board -> Bool -> Int -> Int -> Int -> Svg msg
drawPiece style board rotated delta rowidx colidx =
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
            (1.0 / 8.0)
                * (if rotated then
                    -1

                   else
                    1
                  )

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


drawJumps : Style -> Board -> List OneCorruptibleJump -> Int -> List (Svg msg)
drawJumps style board jumps delta =
    let
        color =
            style.selectedColor

        corruptedColor =
            style.corruptedHulkXColor

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
                    deltaF * colF + deltaF

                yc =
                    deltaF * rowF + deltaF

                len =
                    deltaF * (9.0 / 16.0) * (2.0 / 3.0)

                leno2 =
                    len / 2

                leno2hyp =
                    leno2 * sqrt 2 / 2

                w =
                    "3"
            in
            g [] <|
                case oneJump.hulkAfterJump of
                    CorruptAfterJump ->
                        [ Svg.line
                            [ x1 <| tos (round <| xc - leno2hyp)
                            , x2 <| tos (round <| xc + leno2hyp)
                            , y1 <| tos (round <| yc - leno2hyp)
                            , y2 <| tos (round <| yc + leno2hyp)
                            , strokeWidth w
                            , stroke corruptedColor
                            ]
                            []
                        , Svg.line
                            [ x1 <| tos (round <| xc + leno2hyp)
                            , x2 <| tos (round <| xc - leno2hyp)
                            , y1 <| tos (round <| yc - leno2hyp)
                            , y2 <| tos (round <| yc + leno2hyp)
                            , strokeWidth w
                            , stroke corruptedColor
                            ]
                            []
                        ]

                    _ ->
                        -- Can't ever draw a MakeHulkAfterJump, since it's always
                        -- the last jump
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


drawCols : Style -> Int -> Bool -> Board -> List (Svg msg)
drawCols style delta rotated board =
    List.map (drawCol style delta rotated board) [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        |> List.concat


tosy : Int -> Bool -> Int -> String
tosy delta rotated y =
    tos <|
        if rotated then
            y
            --+ delta // 4

        else
            y


drawCol : Style -> Int -> Bool -> Board -> Int -> List (Svg msg)
drawCol style delta rotated board idx =
    let
        xc =
            delta * idx + delta // 2

        xcs =
            tos xc

        fsize =
            fontSize delta

        ( xi, yi ) =
            if False then
                --rotated then
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
                        [ if False then
                            -- rotated then
                            [ rotate "180" ]

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


rowLetters : Array String
rowLetters =
    "87654321" |> String.toList |> List.map String.fromChar |> Array.fromList


rowToString : Int -> String
rowToString row =
    case Array.get row rowLetters of
        Nothing ->
            ""

        Just letter ->
            letter


findArrayIndex : a -> Array a -> Maybe Int
findArrayIndex elt array =
    let
        len =
            Array.length array

        justElt =
            Just elt

        mapper idx =
            if idx >= len then
                Nothing

            else if justElt == Array.get idx array then
                Just idx

            else
                mapper <| idx + 1
    in
    mapper 0


stringToRow : String -> Int
stringToRow row =
    case findArrayIndex row rowLetters of
        Just idx ->
            idx

        Nothing ->
            -1


colLetters : Array String
colLetters =
    "abcdefgh" |> String.toList |> List.map String.fromChar |> Array.fromList


colToString : Int -> String
colToString col =
    case Array.get col colLetters of
        Nothing ->
            ""

        Just letter ->
            letter


stringToCol : String -> Int
stringToCol col =
    case findArrayIndex col colLetters of
        Just idx ->
            idx

        Nothing ->
            -1


rowColToString : RowCol -> String
rowColToString { row, col } =
    colToString col ++ rowToString row


illegalRowCol : RowCol
illegalRowCol =
    rc -1 -1


isRowColLegal : RowCol -> Bool
isRowColLegal { row, col } =
    row >= 0 && row < 8 && col >= 0 && col < 8


stringToRowCol : String -> RowCol
stringToRowCol rowCol =
    if 2 /= String.length rowCol then
        illegalRowCol

    else
        rc (stringToRow <| String.dropLeft 1 rowCol)
            (stringToCol <| String.left 1 rowCol)


mapAllNeighbors : (RowCol -> Piece -> a -> a) -> Board -> RowCol -> a -> a
mapAllNeighbors =
    mapNeighbors True WhiteColor


mapForwardNeighbors : Color -> (RowCol -> Piece -> a -> a) -> Board -> RowCol -> a -> a
mapForwardNeighbors color =
    mapNeighbors False color


mapNeighbors : Bool -> Color -> (RowCol -> Piece -> a -> a) -> Board -> RowCol -> a -> a
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


mapWholeBoard : (RowCol -> Piece -> a -> a) -> Board -> a -> a
mapWholeBoard mapper board res =
    let
        mapone row col res3 =
            let
                rowCol =
                    rc row col

                piece =
                    get rowCol board
            in
            mapper rowCol piece res3

        mapcols row res2 =
            List.foldr (mapone row) res2 indices
    in
    List.foldr mapcols res indices


mapWholeBoardWithExit : (RowCol -> Piece -> a -> ( a, Bool )) -> Board -> a -> a
mapWholeBoardWithExit mapper board res =
    let
        mapone : Int -> Int -> a -> ( a, Bool )
        mapone row2 col2 res2 =
            let
                rowCol =
                    rc row2 col2

                piece =
                    get rowCol board
            in
            mapper rowCol piece res2

        mapcols row3 ( res3, done3 ) =
            if done3 then
                ( res3, True )

            else
                let
                    maybeMapOne row4 col4 ( res4, done4 ) =
                        if done4 then
                            ( res4, True )

                        else
                            mapone row4 col4 res4
                in
                List.foldr (maybeMapOne row3) ( res3, False ) indices
    in
    List.foldr mapcols ( res, False ) indices
        |> Tuple.first


findSquareSatisfying : (RowCol -> Piece -> Bool) -> Board -> Maybe ( RowCol, Piece )
findSquareSatisfying predicate board =
    let
        illegalRc =
            rc -1 -1

        mapper rowCol piece res =
            if predicate rowCol piece then
                ( ( rowCol, piece ), True )

            else
                ( res, False )

        ( res2, piece2 ) =
            mapWholeBoardWithExit mapper board ( illegalRc, Types.emptyPiece )
    in
    if res2 == illegalRc then
        Nothing

    else
        Just ( res2, piece2 )


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


computeJumperLocations : Color -> Board -> List RowCol
computeJumperLocations color board =
    let
        mapper : RowCol -> Piece -> List ( RowCol, Int ) -> List ( RowCol, Int )
        mapper rowCol piece res =
            if piece.pieceType == NoPiece || piece.color /= color then
                res

            else
                let
                    jumps =
                        legalJumpsWithPiece board rowCol piece
                in
                case List.head jumps of
                    Nothing ->
                        res

                    Just sequence ->
                        ( rowCol, List.length sequence ) :: res

        jumpPairs : List ( RowCol, Int )
        jumpPairs =
            mapWholeBoard mapper board []

        maxer : ( RowCol, Int ) -> Int -> Int
        maxer ( _, cnt ) maxJumps =
            max cnt maxJumps

        maxLen : Int
        maxLen =
            List.foldl maxer 0 jumpPairs

        selector : ( RowCol, Int ) -> List RowCol -> List RowCol
        selector ( rowCol, cnt ) res =
            if cnt == maxLen then
                rowCol :: res

            else
                res
    in
    List.foldl selector [] jumpPairs


computeLegalMoves : Board -> Maybe RowCol -> MovesOrJumps
computeLegalMoves newBoard selected =
    case selected of
        Nothing ->
            NoMoves

        Just rowCol ->
            case legalJumps newBoard rowCol of
                [] ->
                    case legalSlides newBoard rowCol of
                        [] ->
                            NoMoves

                        slides ->
                            Moves slides

                jumpSequences ->
                    Jumps jumpSequences


isHulkType : PieceType -> Bool
isHulkType pieceType =
    pieceType == Hulk || pieceType == CorruptedHulk


legalJumps : Board -> RowCol -> List JumpSequence
legalJumps board startPos =
    legalJumpsWithPiece board startPos <| get startPos board


legalJumpsWithPiece : Board -> RowCol -> Piece -> List JumpSequence
legalJumpsWithPiece board startPos piece =
    let
        { color, pieceType } =
            piece

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


computeJumpSequences : Color -> Board -> RowCol -> List JumpSequence
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


computeLongJumpSequences : Color -> Board -> RowCol -> List JumpSequence
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


legalSlides : Board -> RowCol -> List RowCol
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


computeLongSlides : Color -> Board -> RowCol -> List RowCol
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


countColor : Color -> Board -> Int
countColor color board =
    let
        mapper _ piece total =
            let
                pieceColor =
                    piece.color

                increment =
                    case piece.pieceType of
                        Golem ->
                            if pieceColor == color then
                                1

                            else
                                0

                        Hulk ->
                            if pieceColor == color then
                                2

                            else
                                0

                        CorruptedHulk ->
                            1

                        Journeyman ->
                            if pieceColor == color then
                                2

                            else
                                1

                        _ ->
                            0
            in
            total + increment
    in
    mapWholeBoard mapper board 0


archiveGame : GameState -> ArchivedGame
archiveGame { moves, players, winner } =
    ArchivedGame moves players winner


unarchiveGame : ArchivedGame -> GameState -> GameState
unarchiveGame { moves, players, winner } gameState =
    let
        ( whoseTurn, board ) =
            replayMoves moves initial
    in
    { gameState
        | newBoard = board
        , moves = moves
        , players = players
        , whoseTurn = whoseTurn
        , selected = Nothing
        , jumperLocations = []
        , legalMoves = NoMoves
        , undoStates = []
        , jumps = []
        , winner = winner
        , testMode = Nothing
    }


replayMoves : List OneMove -> Board -> ( Player, Board )
replayMoves moves board =
    -- Should this check that the moves are legal?
    -- List.foldr walks in reverse order, as intended.
    List.foldr replayMove ( WhitePlayer, board ) moves


replayMove : OneMove -> ( Player, Board ) -> ( Player, Board )
replayMove { piece, sequence } ( whoseTurn, board ) =
    case sequence of
        OneResign ->
            ( whoseTurn, board )

        OneSlide { from, to, makeHulk } ->
            let
                board2 =
                    set from Types.emptyPiece board
            in
            ( Types.otherPlayer whoseTurn
            , case makeHulk of
                Nothing ->
                    set to piece board2

                Just hulkPos ->
                    set to Types.emptyPiece board2
                        |> set hulkPos
                            { color = Types.playerColor whoseTurn
                            , pieceType = Hulk
                            }
            )

        OneJumpSequence jumps ->
            ( Types.otherPlayer whoseTurn
            , List.foldl (replayOneCorruptibleJump whoseTurn piece) board jumps
            )


replayOneCorruptibleJump : Player -> Piece -> OneCorruptibleJump -> Board -> Board
replayOneCorruptibleJump whoseTurn piece { from, over, to, hulkAfterJump } board =
    board
        |> set from Types.emptyPiece
        |> (case hulkAfterJump of
                NoHulkAfterJump ->
                    identity

                CorruptAfterJump ->
                    set over
                        { color = Types.playerColor whoseTurn
                        , pieceType = CorruptedHulk
                        }

                MakeHulkAfterJump hulkPos ->
                    set hulkPos
                        { color = Types.playerColor whoseTurn
                        , pieceType = Hulk
                        }
           )
        |> set to piece
