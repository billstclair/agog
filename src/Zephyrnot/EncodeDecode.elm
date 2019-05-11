---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Zephyrnot JSON encoders and decoders
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zephyrnot.EncodeDecode exposing (decodeSavedModel, encodeSavedModel)

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as JE exposing (Value)
import Zephyrnot.Types
    exposing
        ( Board
        , Decoration(..)
        , Page(..)
        , Player(..)
        , SavedModel
        , Winner(..)
        )


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "page", encodePage model.page )
        , ( "decoration", encodeDecoration model.decoration )
        , ( "firstSelection", encodeDecoration model.firstSelection )
        , ( "chooseFirst", encodePlayer model.chooseFirst )
        , ( "player", encodePlayer model.player )
        , ( "winner", encodeWinner model.winner )
        , ( "path", JE.list encodeIntPair model.path )
        , ( "moves", JE.list JE.string model.moves )
        , ( "board", encodeBoard model.board )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "page" pageDecoder MainPage
        |> required "decoration" decorationDecoder
        |> required "firstSelection" decorationDecoder
        |> required "chooseFirst" playerDecoder
        |> required "player" playerDecoder
        |> required "winner" winnerDecoder
        |> required "path" (JD.list intPairDecoder)
        |> required "moves" (JD.list JD.string)
        |> required "board" boardDecoder


encodePage : Page -> Value
encodePage page =
    JE.string <|
        case page of
            MainPage ->
                "MainPage"

            RulesPage ->
                "RulesPage"

            InstructionsPage ->
                "InstructionsPage"


pageDecoder : Decoder Page
pageDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "MainPage" ->
                        JD.succeed MainPage

                    "RulesPage" ->
                        JD.succeed RulesPage

                    "InstructionsPage" ->
                        JD.succeed InstructionsPage

                    _ ->
                        JD.fail <| "Unknown page: " ++ s
            )


encodeDecoration : Decoration -> Value
encodeDecoration decoration =
    case decoration of
        NoDecoration ->
            JE.string "NoDecoration"

        RowSelectedDecoration rowidx ->
            JE.object [ ( "RowSelectedDecoration", JE.int rowidx ) ]

        ColSelectedDecoration colidx ->
            JE.object [ ( "ColSelectedDecoration", JE.int colidx ) ]

        AlreadyFilledDecoration pair ->
            JE.object [ ( "AlreadyFilledDecoration", encodeIntPair pair ) ]


decorationDecoder : Decoder Decoration
decorationDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if s == "NoDecoration" then
                        JD.succeed NoDecoration

                    else
                        JD.fail <| "Unknown Decoration: " ++ s
                )
        , JD.field "RowSelectedDecoration" JD.int
            |> JD.andThen
                (\rowidx ->
                    JD.succeed <| RowSelectedDecoration rowidx
                )
        , JD.field "ColSelectedDecoration" JD.int
            |> JD.andThen
                (\colidx ->
                    JD.succeed <| ColSelectedDecoration colidx
                )
        , JD.field "AlreadyFilledDecoration" intPairDecoder
            |> JD.andThen
                (\pair ->
                    JD.succeed <| AlreadyFilledDecoration pair
                )
        ]


encodePlayer : Player -> Value
encodePlayer player =
    case player of
        Zephyrus ->
            JE.string "Zephyrus"

        Notus ->
            JE.string "Notus"


playerDecoder : Decoder Player
playerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "Zephyrus" ->
                        JD.succeed Zephyrus

                    "Notus" ->
                        JD.succeed Notus

                    _ ->
                        JD.fail <| "Unknown player: " ++ s
            )


encodeWinner : Winner -> Value
encodeWinner winner =
    JE.string <|
        case winner of
            NoWinner ->
                "NoWinner"

            HorizontalWinner ->
                "HorizontalWinner"

            VerticalWinner ->
                "VerticalWinner"


winnerDecoder : Decoder Winner
winnerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "NoWinner" ->
                        JD.succeed NoWinner

                    "HorizontalWinner" ->
                        JD.succeed HorizontalWinner

                    "VerticalWinner" ->
                        JD.succeed VerticalWinner

                    _ ->
                        JD.fail <| "Unknown winner: " ++ s
            )


encodeIntPair : ( Int, Int ) -> Value
encodeIntPair ( x, y ) =
    JE.list JE.int [ x, y ]


intPairDecoder : Decoder ( Int, Int )
intPairDecoder =
    JD.list JD.int
        |> JD.andThen
            (\list ->
                case list of
                    [ rowidx, colidx ] ->
                        JD.succeed ( rowidx, colidx )

                    _ ->
                        JD.fail "Wrong length Int pair"
            )


encodeBoard : Board -> Value
encodeBoard board =
    JE.array (JE.array JE.bool) board


boardDecoder : Decoder Board
boardDecoder =
    JD.list (JD.list JD.bool)
        |> JD.andThen
            (\l ->
                List.map (\l2 -> Array.fromList l2) l
                    |> Array.fromList
                    |> JD.succeed
            )