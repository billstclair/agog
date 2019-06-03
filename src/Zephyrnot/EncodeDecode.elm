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


module Zephyrnot.EncodeDecode exposing
    ( boardToString
    , decodeSavedModel
    , encodeGameState
    , encodeSavedModel
    , gameStateDecoder
    , messageDecoder
    , messageEncoder
    , stringToBoard
    )

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as JE exposing (Value)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , ReqRsp(..)
        , ServerState
        )
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Choice(..)
        , Decoration(..)
        , GameState
        , Message(..)
        , Page(..)
        , Player(..)
        , PlayerNames
        , SavedModel
        , Score
        , Winner(..)
        )


encodeMoves : List String -> Value
encodeMoves moves =
    moves
        |> List.intersperse ","
        |> String.concat
        |> JE.string


movesDecoder : Decoder (List String)
movesDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (String.split "," >> JD.succeed)
        , JD.list JD.string --backward compatibility
        ]


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
        , ( "moves", encodeMoves model.moves )
        , ( "board", encodeBoard model.board )
        , ( "score", encodeScore model.score )
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
        |> required "moves" movesDecoder
        |> required "board" boardDecoder
        |> optional "score" scoreDecoder Types.zeroScore


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

            AuxPage ->
                "AuxPage"


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

                    "AuxPage" ->
                        JD.succeed AuxPage

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


boolToString : Bool -> String
boolToString bool =
    if bool then
        "0"

    else
        "-"


stringToBool : String -> Bool
stringToBool string =
    string == "0"


rowToString : Array Bool -> String
rowToString row =
    Array.toList row
        |> List.map boolToString
        |> String.concat


stringToRow : String -> Array Bool
stringToRow string =
    String.toList string
        |> List.map String.fromChar
        |> List.map stringToBool
        |> Array.fromList


boardToString : Board -> String
boardToString board =
    Array.toList board
        |> List.map rowToString
        |> List.intersperse "|"
        |> String.concat


stringToBoard : String -> Maybe Board
stringToBoard string =
    if String.length string /= 41 then
        Nothing

    else
        let
            rows =
                [ String.slice 0 6 string
                , String.slice 7 13 string
                , String.slice 14 20 string
                , String.slice 21 27 string
                , String.slice 28 34 string
                , String.slice 35 41 string
                ]
        in
        rows
            |> List.map stringToRow
            |> Array.fromList
            |> Just


encodeBoard : Board -> Value
encodeBoard board =
    JE.string <| boardToString board


newBoardDecoder : Decoder Board
newBoardDecoder =
    JD.string
        |> JD.andThen
            (\string ->
                case stringToBoard string of
                    Nothing ->
                        JD.fail "Invalid board string."

                    Just board ->
                        JD.succeed board
            )


boardDecoder : Decoder Board
boardDecoder =
    JD.oneOf
        [ newBoardDecoder
        , oldBoardDecoder
        ]


oldBoardDecoder : Decoder Board
oldBoardDecoder =
    JD.list (JD.list JD.bool)
        |> JD.andThen
            (\l ->
                List.map (\l2 -> Array.fromList l2) l
                    |> Array.fromList
                    |> JD.succeed
            )


encodeScore : Score -> Value
encodeScore score =
    JE.object
        [ ( "zephyrusGames", JE.int score.zephyrusGames )
        , ( "notusGames", JE.int score.notusGames )
        , ( "zephyrusScore", JE.int score.zephyrusScore )
        , ( "notusScore", JE.int score.notusScore )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    JD.succeed Score
        |> required "zephyrusGames" JD.int
        |> required "notusGames" JD.int
        |> required "zephyrusScore" JD.int
        |> required "notusScore" JD.int



---
--- Messages
---


encodePlayerNames : PlayerNames -> Value
encodePlayerNames { zephyrus, notus } =
    JE.object
        [ ( "zephyrus", JE.string zephyrus )
        , ( "notus", JE.string notus )
        ]


playerNamesDecoder : Decoder PlayerNames
playerNamesDecoder =
    JD.succeed PlayerNames
        |> required "zephyrus" JD.string
        |> required "notus" JD.string


encodeGameState : GameState -> Value
encodeGameState { board, moves, players, whoseTurn, score, winner } =
    JE.object
        [ ( "board", encodeBoard board )
        , ( "moves", encodeMoves moves )
        , ( "players", encodePlayerNames players )
        , ( "whoseTurn", encodePlayer whoseTurn )
        , ( "score", encodeScore score )
        , ( "winner", encodeWinner winner )
        ]


gameStateDecoder : Decoder GameState
gameStateDecoder =
    JD.succeed GameState
        |> required "board" boardDecoder
        |> required "moves" movesDecoder
        |> required "players" playerNamesDecoder
        |> required "whoseTurn" playerDecoder
        |> required "score" scoreDecoder
        |> required "winner" winnerDecoder


encodeChoice : Choice -> Value
encodeChoice choice =
    JE.object
        [ case choice of
            ChooseRow row ->
                ( "ChooseRow", JE.int row )

            ChooseCol col ->
                ( "ChooseCol", JE.int col )

            ChooseResign player ->
                ( "ChooseResign", encodePlayer player )

            ChooseNew player ->
                ( "ChooseNew", encodePlayer player )
        ]


choiceDecoder : Decoder Choice
choiceDecoder =
    JD.oneOf
        [ JD.field "ChooseRow" JD.int
            |> JD.andThen (ChooseRow >> JD.succeed)
        , JD.field "ChooseCol" JD.int
            |> JD.andThen (ChooseCol >> JD.succeed)
        , JD.field "ChooseResign" playerDecoder
            |> JD.andThen (ChooseResign >> JD.succeed)
        , JD.field "ChooseNew" playerDecoder
            |> JD.andThen (ChooseNew >> JD.succeed)
        ]


messageEncoder : Message -> ( ReqRsp, Plist )
messageEncoder message =
    case message of
        NewReq { name, isPublic, restoreState } ->
            ( Req "new"
            , [ ( "name", JE.string name )
              , ( "isPublic", JE.bool isPublic )
              , ( "restoreState"
                , case restoreState of
                    Nothing ->
                        JE.null

                    Just state ->
                        encodeGameState state
                )
              ]
            )

        NewRsp { gameid, playerid, name } ->
            ( Rsp "new"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              , ( "name", JE.string name )
              ]
            )

        JoinReq { gameid, name } ->
            ( Req "join"
            , [ ( "gameid", JE.string gameid )
              , ( "name", JE.string name )
              ]
            )

        JoinRsp { gameid, playerid, names, gameState } ->
            ( Rsp "join"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              , ( "names", encodePlayerNames names )
              , ( "gameState", encodeGameState gameState )
              ]
            )

        LeaveReq { playerid } ->
            ( Req "leave"
            , [ ( "playerid", JE.string playerid ) ]
            )

        LeaveRsp { gameid } ->
            ( Rsp "leave"
            , [ ( "gameid", JE.string gameid ) ]
            )

        UpdateReq { playerid } ->
            ( Req "update"
            , [ ( "playerid", JE.string playerid ) ]
            )

        UpdateRsp { gameid, gameState } ->
            ( Rsp "update"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState gameState )
              ]
            )

        PlayReq { playerid, placement } ->
            ( Req "play"
            , [ ( "playerid", JE.string playerid )
              , ( "placement", encodeChoice placement )
              ]
            )

        PlayRsp { gameid, placement } ->
            ( Rsp "play"
            , [ ( "gameid", JE.string gameid )
              , ( "placement", encodeChoice placement )
              ]
            )

        GameOverRsp { gameid, winner } ->
            ( Rsp "gameOver"
            , [ ( "gameid", JE.string gameid )
              , ( "winner", encodeWinner winner )
              ]
            )

        ErrorRsp { request, text } ->
            ( Rsp "error"
            , [ ( "request", JE.string request )
              , ( "text", JE.string text )
              ]
            )

        ChatReq { playerid, text } ->
            ( Req "chat"
            , [ ( "playerid", JE.string playerid )
              , ( "text", JE.string text )
              ]
            )

        ChatRsp { gameid, name, text } ->
            ( Rsp "chat"
            , [ ( "gameid", JE.string gameid )
              , ( "name", JE.string name )
              , ( "text", JE.string text )
              ]
            )


newReqDecoder : Decoder Message
newReqDecoder =
    JD.succeed
        (\name isPublic restoreState ->
            NewReq
                { name = name
                , isPublic = isPublic
                , restoreState = restoreState
                }
        )
        |> required "name" JD.string
        |> required "isPublic" JD.bool
        |> required "restoreState" (JD.nullable gameStateDecoder)


joinReqDecoder : Decoder Message
joinReqDecoder =
    JD.succeed
        (\gameid name ->
            JoinReq
                { gameid = gameid
                , name = name
                }
        )
        |> required "gameid" JD.string
        |> required "name" JD.string


leaveReqDecoder : Decoder Message
leaveReqDecoder =
    JD.succeed
        (\playerid ->
            LeaveReq
                { playerid = playerid
                }
        )
        |> required "playerid" JD.string


updateReqDecoder : Decoder Message
updateReqDecoder =
    JD.succeed
        (\playerid ->
            UpdateReq
                { playerid = playerid
                }
        )
        |> required "playerid" JD.string


playReqDecoder : Decoder Message
playReqDecoder =
    JD.succeed
        (\playerid placement ->
            PlayReq
                { playerid = playerid
                , placement = placement
                }
        )
        |> required "playerid" JD.string
        |> required "placement" choiceDecoder


chatReqDecoder : Decoder Message
chatReqDecoder =
    JD.succeed
        (\playerid text ->
            ChatReq
                { playerid = playerid
                , text = text
                }
        )
        |> required "playerid" JD.string
        |> required "text" JD.string


newRspDecoder : Decoder Message
newRspDecoder =
    JD.succeed
        (\gameid playerid name ->
            NewRsp
                { gameid = gameid
                , playerid = playerid
                , name = name
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string
        |> required "name" JD.string


joinRspDecoder : Decoder Message
joinRspDecoder =
    JD.succeed
        (\gameid playerid names gameState ->
            JoinRsp
                { gameid = gameid
                , playerid = playerid
                , names = names
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string
        |> required "names" playerNamesDecoder
        |> required "gameState" gameStateDecoder


leaveRspDecoder : Decoder Message
leaveRspDecoder =
    JD.succeed
        (\gameid ->
            LeaveRsp
                { gameid = gameid
                }
        )
        |> required "gameid" JD.string


updateRspDecoder : Decoder Message
updateRspDecoder =
    JD.succeed
        (\gameid gameState ->
            UpdateRsp
                { gameid = gameid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder


playRspDecoder : Decoder Message
playRspDecoder =
    JD.succeed
        (\gameid placement ->
            PlayRsp
                { gameid = gameid
                , placement = placement
                }
        )
        |> required "gameid" JD.string
        |> required "placement" choiceDecoder


gameOverRspDecoder : Decoder Message
gameOverRspDecoder =
    JD.succeed
        (\gameid winner ->
            GameOverRsp
                { gameid = gameid
                , winner = winner
                }
        )
        |> required "gameid" JD.string
        |> required "winner" winnerDecoder


errorRspDecoder : Decoder Message
errorRspDecoder =
    JD.succeed
        (\request text ->
            ErrorRsp
                { request = request
                , text = text
                }
        )
        |> required "request" JD.string
        |> required "text" JD.string


chatRspDecoder : Decoder Message
chatRspDecoder =
    JD.succeed
        (\gameid name text ->
            ChatRsp
                { gameid = gameid
                , name = name
                , text = text
                }
        )
        |> required "gameid" JD.string
        |> required "name" JD.string
        |> required "text" JD.string


messageDecoder : ( ReqRsp, Plist ) -> Result String Message
messageDecoder ( reqrsp, plist ) =
    case reqrsp of
        Req msg ->
            case msg of
                "new" ->
                    decodePlist newReqDecoder plist

                "join" ->
                    decodePlist joinReqDecoder plist

                "leave" ->
                    decodePlist leaveReqDecoder plist

                "update" ->
                    decodePlist updateReqDecoder plist

                "play" ->
                    decodePlist playReqDecoder plist

                "chat" ->
                    decodePlist chatReqDecoder plist

                _ ->
                    Err <| "Unknown Req: " ++ msg

        Rsp msg ->
            case msg of
                "new" ->
                    decodePlist newRspDecoder plist

                "join" ->
                    decodePlist joinRspDecoder plist

                "leave" ->
                    decodePlist leaveRspDecoder plist

                "update" ->
                    decodePlist updateRspDecoder plist

                "play" ->
                    decodePlist playRspDecoder plist

                "gameOver" ->
                    decodePlist gameOverRspDecoder plist

                "error" ->
                    decodePlist errorRspDecoder plist

                "chat" ->
                    decodePlist chatRspDecoder plist

                _ ->
                    Err <| "Unknown Rsp: " ++ msg
