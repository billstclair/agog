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
    , encodeMoves
    , encodeSavedModel
    , frameworkToPublicGame
    , gameStateDecoder
    , messageDecoder
    , messageEncoder
    , messageEncoderWithPrivate
    , movesDecoder
    , publicGameToFramework
    , stringToBoard
    )

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
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
        , PrivateGameState
        , PublicGame
        , PublicType(..)
        , SavedModel
        , Score
        , Settings
        , Socket
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
                (String.split "," >> List.filter ((/=) "") >> JD.succeed)
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
        , ( "gameState", encodeGameState True model.gameState )
        , ( "isLocal", JE.bool model.isLocal )
        , ( "isLive", JE.bool model.isLive )
        , ( "gameid", JE.string model.gameid )
        , ( "playerid", JE.string model.playerid )
        , ( "settings", encodeSettings model.settings )
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
        |> required "gameState" gameStateDecoder
        |> optional "isLocal" JD.bool False
        |> optional "isLive" JD.bool False
        |> optional "gameid" JD.string ""
        |> optional "playerid" JD.string ""
        |> optional "settings" settingsDecoder Types.emptySettings


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

            PublicPage ->
                "PublicPage"


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

                    "PublicPage" ->
                        JD.succeed PublicPage

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

            ZephyrusWinner ->
                "ZephyrusWinner"

            NotusWinner ->
                "NotusWinner"


winnerDecoder : Decoder Winner
winnerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "NoWinner" ->
                        JD.succeed NoWinner

                    "ZephyrusWinner" ->
                        JD.succeed ZephyrusWinner

                    -- Backward compatibility
                    "HorizontalWinner" ->
                        JD.succeed ZephyrusWinner

                    "NotusWinner" ->
                        JD.succeed NotusWinner

                    --  Backward compatibility
                    "VerticalWinner" ->
                        JD.succeed NotusWinner

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


encodeSettings : Settings -> Value
encodeSettings { name, isPublic, forName, hideTitle } =
    JE.object
        [ ( "name", JE.string name )
        , ( "isPublic", JE.bool isPublic )
        , ( "forName", JE.string forName )
        , ( "hideTitle", JE.bool hideTitle )
        ]


settingsDecoder : Decoder Settings
settingsDecoder =
    JD.succeed Settings
        |> required "name" JD.string
        |> optional "isPublic" JD.bool False
        |> optional "forName" JD.string ""
        |> required "hideTitle" JD.bool



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


encodePrivateGameState : PrivateGameState -> Value
encodePrivateGameState { decoration, subscribers } =
    List.concat
        [ case decoration of
            NoDecoration ->
                []

            _ ->
                [ ( "decoration", encodeDecoration decoration ) ]
        , case Set.toList subscribers of
            [] ->
                []

            list ->
                [ ( "subscribers", JE.list encodeSubscriberPair list ) ]
        ]
        |> JE.object


encodeSubscriberPair : ( Socket, String ) -> Value
encodeSubscriberPair ( socket, forName ) =
    JE.list identity [ JE.string socket, JE.string forName ]


subscriberPairDecoder : Decoder ( Socket, String )
subscriberPairDecoder =
    JD.list JD.string
        |> JD.andThen
            (\list ->
                case list of
                    [ socket, forName ] ->
                        JD.succeed ( socket, forName )

                    _ ->
                        JD.fail "Not a two-element list"
            )


subscribersListDecoder : Decoder (List ( String, String ))
subscribersListDecoder =
    JD.list subscriberPairDecoder


subscribersDecoder : Decoder (Set ( String, String ))
subscribersDecoder =
    subscribersListDecoder
        |> JD.andThen (Set.fromList >> JD.succeed)


privateGameStateDecoder : Decoder PrivateGameState
privateGameStateDecoder =
    JD.succeed PrivateGameState
        |> optional "decoration" decorationDecoder NoDecoration
        |> optional "subscribers" subscribersDecoder Set.empty


encodeGameState : Bool -> GameState -> Value
encodeGameState includePrivate gameState =
    let
        { board, moves, players, whoseTurn, score, winner, path } =
            gameState

        privateValue =
            if includePrivate then
                encodePrivateGameState gameState.private

            else
                JE.null
    in
    JE.object
        [ ( "board", encodeBoard board )
        , ( "moves", encodeMoves moves )
        , ( "players", encodePlayerNames players )
        , ( "whoseTurn", encodePlayer whoseTurn )
        , ( "score", encodeScore score )
        , ( "winner", encodeWinner winner )
        , ( "path", JE.list encodeIntPair path )
        , ( "private", privateValue )
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
        |> required "path" (JD.list intPairDecoder)
        |> required "private" privateGameStateDecoder


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


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder maybe =
    case maybe of
        Nothing ->
            JE.null

        Just a ->
            encoder a


encodePublicGame : PublicGame -> Value
encodePublicGame game =
    let
        { gameid, creator, player, forName } =
            game
    in
    JE.object
        [ ( "gameid", JE.string gameid )
        , ( "creator", JE.string creator )
        , ( "player", encodePlayer player )
        , ( "forName", encodeMaybe JE.string forName )
        ]


publicGameDecoder : Decoder PublicGame
publicGameDecoder =
    JD.succeed PublicGame
        |> required "gameid" JD.string
        |> required "creator" JD.string
        |> required "player" playerDecoder
        |> required "forName" (JD.nullable JD.string)


publicGameToFramework : PublicGame -> WebSocketFramework.Types.PublicGame
publicGameToFramework { gameid, creator, player, forName } =
    { gameid = gameid
    , playerName =
        JE.object
            [ ( "creator", JE.string creator )
            , ( "player", encodePlayer player )
            , ( "forName", encodeMaybe JE.string forName )
            ]
            |> JE.encode 0
    }


frameworkToPublicGame : WebSocketFramework.Types.PublicGame -> Maybe PublicGame
frameworkToPublicGame { gameid, playerName } =
    JD.decodeString
        (JD.succeed
            (\creator player forName ->
                PublicGame gameid creator player forName
            )
            |> required "creator" JD.string
            |> required "player" playerDecoder
            |> required "forName" (JD.nullable JD.string)
        )
        playerName
        |> Result.toMaybe


encodePublicType : PublicType -> Value
encodePublicType publicType =
    case publicType of
        NotPublic ->
            JE.string "NotPublic"

        EntirelyPublic ->
            JE.string "EntirelyPublic"

        PublicFor name ->
            JE.object [ ( "publicFor", JE.string name ) ]


publicTypeDecoder : Decoder PublicType
publicTypeDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    case s of
                        "NotPublic" ->
                            JD.succeed NotPublic

                        "EntirelyPublic" ->
                            JD.succeed EntirelyPublic

                        _ ->
                            JD.fail "Not a public type"
                )
        , JD.field "publicFor" JD.string
            |> JD.andThen (PublicFor >> JD.succeed)
        ]


messageEncoder : Message -> ( ReqRsp, Plist )
messageEncoder =
    messageEncoderInternal False


messageEncoderWithPrivate : Message -> ( ReqRsp, Plist )
messageEncoderWithPrivate =
    messageEncoderInternal True


messageEncoderInternal : Bool -> Message -> ( ReqRsp, Plist )
messageEncoderInternal includePrivate message =
    case message of
        NewReq { name, player, publicType, restoreState } ->
            ( Req "new"
            , [ ( "name", JE.string name )
              , ( "player", encodePlayer player )
              , ( "publicType", encodePublicType publicType )
              , ( "restoreState"
                , encodeMaybe (encodeGameState includePrivate) restoreState
                )
              ]
            )

        NewRsp { gameid, playerid, player, name, publicType, gameState } ->
            ( Rsp "new"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              , ( "player", encodePlayer player )
              , ( "name", JE.string name )
              , ( "publicType", encodePublicType publicType )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        JoinReq { gameid, name } ->
            ( Req "join"
            , [ ( "gameid", JE.string gameid )
              , ( "name", JE.string name )
              ]
            )

        ReJoinReq { gameid, playerid } ->
            ( Req "rejoin"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              ]
            )

        JoinRsp { gameid, playerid, player, gameState } ->
            ( Rsp "join"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", encodeMaybe JE.string playerid )
              , ( "player", encodePlayer player )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        LeaveReq { playerid } ->
            ( Req "leave"
            , [ ( "playerid", JE.string playerid ) ]
            )

        LeaveRsp { gameid, player } ->
            ( Rsp "leave"
            , [ ( "gameid", JE.string gameid )
              , ( "player", encodePlayer player )
              ]
            )

        UpdateReq { playerid } ->
            ( Req "update"
            , [ ( "playerid", JE.string playerid ) ]
            )

        UpdateRsp { gameid, gameState } ->
            ( Rsp "update"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        PlayReq { playerid, placement } ->
            ( Req "play"
            , [ ( "playerid", JE.string playerid )
              , ( "placement", encodeChoice placement )
              ]
            )

        PlayRsp { gameid, gameState, decoration } ->
            ( Rsp "play"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "decoration", encodeDecoration decoration )
              ]
            )

        ResignRsp { gameid, gameState, player } ->
            ( Rsp "resign"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "player", encodePlayer player )
              ]
            )

        AnotherGameRsp { gameid, gameState, player } ->
            ( Rsp "anotherGame"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "player", encodePlayer player )
              ]
            )

        GameOverRsp { gameid, gameState } ->
            ( Rsp "gameOver"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        PublicGamesReq { subscribe, forName, gameid } ->
            ( Req "publicGames"
            , [ ( "subscribe", JE.bool subscribe )
              , ( "forName", JE.string forName )
              , ( "gameid", encodeMaybe JE.string gameid )
              ]
            )

        PublicGamesRsp { games } ->
            ( Rsp "publicGames"
            , [ ( "games", JE.list encodePublicGame games )
              ]
            )

        PublicGamesUpdateRsp { added, removed } ->
            ( Rsp "publicGamesUpdate"
            , List.concat
                [ case added of
                    [] ->
                        []

                    games ->
                        [ ( "added", JE.list encodePublicGame games ) ]
                , case removed of
                    [] ->
                        []

                    gameids ->
                        [ ( "removed", JE.list JE.string gameids ) ]
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
        (\name player publicType restoreState ->
            NewReq
                { name = name
                , player = player
                , publicType = publicType
                , restoreState = restoreState
                }
        )
        |> required "name" JD.string
        |> required "player" playerDecoder
        |> required "publicType" publicTypeDecoder
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


rejoinReqDecoder : Decoder Message
rejoinReqDecoder =
    JD.succeed
        (\gameid playerid ->
            ReJoinReq
                { gameid = gameid
                , playerid = playerid
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string


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
        (\gameid playerid player name publicType gameState ->
            NewRsp
                { gameid = gameid
                , playerid = playerid
                , player = player
                , name = name
                , publicType = publicType
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string
        |> required "player" playerDecoder
        |> required "name" JD.string
        |> required "publicType" publicTypeDecoder
        |> required "gameState" gameStateDecoder


joinRspDecoder : Decoder Message
joinRspDecoder =
    JD.succeed
        (\gameid playerid player gameState ->
            JoinRsp
                { gameid = gameid
                , playerid = playerid
                , player = player
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" (JD.nullable JD.string)
        |> required "player" playerDecoder
        |> required "gameState" gameStateDecoder


leaveRspDecoder : Decoder Message
leaveRspDecoder =
    JD.succeed
        (\gameid player ->
            LeaveRsp
                { gameid = gameid
                , player = player
                }
        )
        |> required "gameid" JD.string
        |> required "player" playerDecoder


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
        (\gameid gameState decoration ->
            PlayRsp
                { gameid = gameid
                , gameState = gameState
                , decoration = decoration
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder
        |> required "decoration" decorationDecoder


resignRspDecoder : Decoder Message
resignRspDecoder =
    JD.succeed
        (\gameid gameState player ->
            ResignRsp
                { gameid = gameid
                , gameState = gameState
                , player = player
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder
        |> required "player" playerDecoder


anotherGameRspDecoder : Decoder Message
anotherGameRspDecoder =
    JD.succeed
        (\gameid gameState player ->
            AnotherGameRsp
                { gameid = gameid
                , gameState = gameState
                , player = player
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder
        |> required "player" playerDecoder


gameOverRspDecoder : Decoder Message
gameOverRspDecoder =
    JD.succeed
        (\gameid gameState ->
            GameOverRsp
                { gameid = gameid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder


publicGamesReqDecoder : Decoder Message
publicGamesReqDecoder =
    JD.succeed
        (\subscribe forName gameid ->
            PublicGamesReq
                { subscribe = subscribe
                , forName = forName
                , gameid = gameid
                }
        )
        |> required "subscribe" JD.bool
        |> required "forName" JD.string
        |> required "gameid" (JD.nullable JD.string)


publicGamesRspDecoder : Decoder Message
publicGamesRspDecoder =
    JD.succeed
        (\games ->
            PublicGamesRsp { games = games }
        )
        |> required "games" (JD.list publicGameDecoder)


publicGamesUpdateRspDecoder : Decoder Message
publicGamesUpdateRspDecoder =
    JD.succeed
        (\added removed ->
            PublicGamesUpdateRsp
                { added = added
                , removed = removed
                }
        )
        |> optional "added" (JD.list publicGameDecoder) []
        |> optional "removed" (JD.list JD.string) []


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

                "rejoin" ->
                    decodePlist rejoinReqDecoder plist

                "leave" ->
                    decodePlist leaveReqDecoder plist

                "update" ->
                    decodePlist updateReqDecoder plist

                "play" ->
                    decodePlist playReqDecoder plist

                "publicGames" ->
                    decodePlist publicGamesReqDecoder plist

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

                "resign" ->
                    decodePlist resignRspDecoder plist

                "anotherGame" ->
                    decodePlist anotherGameRspDecoder plist

                "gameOver" ->
                    decodePlist gameOverRspDecoder plist

                "publicGames" ->
                    decodePlist publicGamesRspDecoder plist

                "publicGamesUpdate" ->
                    decodePlist publicGamesUpdateRspDecoder plist

                "error" ->
                    decodePlist errorRspDecoder plist

                "chat" ->
                    decodePlist chatRspDecoder plist

                _ ->
                    Err <| "Unknown Rsp: " ++ msg
