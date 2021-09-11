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


module Agog.EncodeDecode exposing
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
    , newBoardToString
    , pieceToString
    , publicGameToFramework
    , stringToBoard
    , stringToNewBoard
    , stringToPiece
    )

import Agog.Types as Types
    exposing
        ( Board
        , Choice(..)
        , ChooseMoveOption(..)
        , Color(..)
        , Decoration(..)
        , GameState
        , JumpSequence
        , Message(..)
        , MovesOrJumps(..)
        , NewBoard
        , OneCorruptibleJump
        , OneJump
        , Page(..)
        , Piece
        , PieceSelected
        , PieceType(..)
        , Player(..)
        , PlayerNames
        , PrivateGameState
        , PublicGame
        , PublicType(..)
        , RowCol
        , SavedModel
        , Score
        , Settings
        , Socket
        , StyleType(..)
        , TestMode
        , UndoState
        , UndoWhichJumps(..)
        , WinReason(..)
        , Winner(..)
        )
import Array exposing (Array)
import Dict exposing (Dict)
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


encodeStyleType : StyleType -> Value
encodeStyleType styleType =
    case styleType of
        DarkStyle ->
            JE.string "DarkStyle"

        _ ->
            JE.string "LightStyle"


styleTypeDecoder : Decoder StyleType
styleTypeDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "LightStyle" ->
                        JD.succeed LightStyle

                    "DarkStyle" ->
                        JD.succeed DarkStyle

                    _ ->
                        JD.fail "Unknown StyleType name."
            )


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "page", encodePage model.page )
        , ( "decoration", encodeDecoration model.decoration )
        , ( "otherDecoration", encodeDecoration model.otherDecoration )
        , ( "firstSelection", encodeDecoration model.firstSelection )
        , ( "chooseFirst", encodePlayer model.chooseFirst )
        , ( "player", encodePlayer model.player )
        , ( "gameState", encodeGameState True model.gameState )
        , ( "isLocal", JE.bool model.isLocal )
        , ( "isLive", JE.bool model.isLive )
        , ( "gameid", JE.string model.gameid )
        , ( "playerid", JE.string model.playerid )
        , ( "settings", encodeSettings model.settings )
        , ( "styleType", encodeStyleType model.styleType )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "page" pageDecoder MainPage
        |> required "decoration" decorationDecoder
        |> optional "otherDecoration" decorationDecoder NoDecoration
        |> required "firstSelection" decorationDecoder
        |> required "chooseFirst" playerDecoder
        |> required "player" playerDecoder
        |> required "gameState" gameStateDecoder
        |> optional "isLocal" JD.bool False
        |> optional "isLive" JD.bool False
        |> optional "gameid" JD.string ""
        |> optional "playerid" JD.string ""
        |> optional "settings" settingsDecoder Types.emptySettings
        |> optional "styleType" styleTypeDecoder LightStyle


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

                    "PublicPage" ->
                        JD.succeed PublicPage

                    _ ->
                        JD.fail <| "Unknown page: " ++ s
            )


encodePieceSelected : PieceSelected -> Value
encodePieceSelected { selected, moves, jumps } =
    JE.object
        [ ( "selected", encodeIntPair selected )
        , ( "moves", JE.list encodeIntPair moves )
        , ( "jumps", JE.list (\pairList -> JE.list encodeIntPair pairList) jumps )
        ]


pieceSelectedDecoder : Decoder PieceSelected
pieceSelectedDecoder =
    JD.succeed PieceSelected
        |> required "selected" intPairDecoder
        |> required "moves" (JD.list intPairDecoder)
        |> required "jumps" (JD.list (JD.list intPairDecoder))


encodeDecoration : Decoration -> Value
encodeDecoration decoration =
    case decoration of
        NoDecoration ->
            JE.string "NoDecoration"

        PieceSelectedDecoration pieceSelected ->
            JE.object [ ( "PieceSelectedDecoration", encodePieceSelected pieceSelected ) ]


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
        , JD.field "PieceSelectedDecoration" pieceSelectedDecoder
            |> JD.andThen
                (\pieceSelected ->
                    JD.succeed <| PieceSelectedDecoration pieceSelected
                )
        ]


encodePlayer : Player -> Value
encodePlayer player =
    case player of
        WhitePlayer ->
            JE.string "White"

        BlackPlayer ->
            JE.string "Black"


playerDecoder : Decoder Player
playerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "White" ->
                        JD.succeed WhitePlayer

                    "Black" ->
                        JD.succeed BlackPlayer

                    _ ->
                        JD.fail <| "Unknown player: " ++ s
            )


encodeWinReason : WinReason -> Value
encodeWinReason reason =
    JE.string <|
        case reason of
            WinByCapture ->
                "WinByCapture"

            WinBySanctum ->
                "WinBySanctum"

            WinByImmobilization ->
                "WinByImmobilization"

            WinByResignation ->
                "WinByResignation"


winReasonDecoder : Decoder WinReason
winReasonDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "WinByCapture" ->
                        JD.succeed WinByCapture

                    "WinBySanctum" ->
                        JD.succeed WinBySanctum

                    "WinByImmobilization" ->
                        JD.succeed WinByImmobilization

                    "WinByResignation" ->
                        JD.succeed WinByResignation

                    _ ->
                        JD.fail <| "Unknown win reason: " ++ s
            )


encodeWinner : Winner -> Value
encodeWinner winner =
    case winner of
        NoWinner ->
            JE.string "NoWinner"

        WhiteWinner reason ->
            JE.object
                [ ( "WhiteWinner", encodeWinReason reason ) ]

        BlackWinner reason ->
            JE.object
                [ ( "BlackWinner", encodeWinReason reason ) ]


winnerDecoder : Decoder Winner
winnerDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if "NoWinner" == s then
                        JD.succeed NoWinner

                    else
                        JD.fail <| "Unknown win type: " ++ s
                )
        , JD.field "WhiteWinner" winReasonDecoder
            |> JD.andThen (\reason -> WhiteWinner reason |> JD.succeed)
        , JD.field "BlackWinner" winReasonDecoder
            |> JD.andThen (\reason -> BlackWinner reason |> JD.succeed)
        ]


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


encodeTestMode : TestMode -> Value
encodeTestMode { piece, clear } =
    JE.object
        [ ( "piece", encodePiece piece )
        , ( "clear", JE.bool clear )
        ]


testModeDecoder : Decoder TestMode
testModeDecoder =
    JD.succeed TestMode
        |> required "piece" pieceDecoder
        |> required "clear" JD.bool


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


newishBoardDecoder : Decoder Board
newishBoardDecoder =
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
        [ newishBoardDecoder
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


pieceToString : Piece -> String
pieceToString { color, pieceType } =
    let
        letter =
            case pieceType of
                Golem ->
                    "G"

                Hulk ->
                    "H"

                CorruptedHulk ->
                    "C"

                Journeyman ->
                    "J"

                NoPiece ->
                    "-"
    in
    case color of
        WhiteColor ->
            letter

        BlackColor ->
            String.toLower letter


stringToPiece : String -> Piece
stringToPiece string =
    let
        upper =
            String.toUpper string

        pieceType =
            case upper of
                "G" ->
                    Golem

                "H" ->
                    Hulk

                "C" ->
                    CorruptedHulk

                "J" ->
                    Journeyman

                _ ->
                    NoPiece

        color =
            case pieceType of
                NoPiece ->
                    WhiteColor

                _ ->
                    if upper == string then
                        WhiteColor

                    else
                        BlackColor
    in
    { color = color
    , pieceType = pieceType
    }


encodePiece : Piece -> Value
encodePiece piece =
    JE.string <| pieceToString piece


pieceDecoder : Decoder Piece
pieceDecoder =
    JD.string
        |> JD.andThen
            (stringToPiece >> JD.succeed)


newRowToString : Array Piece -> String
newRowToString row =
    Array.toList row
        |> List.map pieceToString
        |> String.concat


stringToNewRow : String -> Array Piece
stringToNewRow string =
    String.toList string
        |> List.map String.fromChar
        |> List.map stringToPiece
        |> Array.fromList


newBoardToString : NewBoard -> String
newBoardToString board =
    Array.toList board
        |> List.map newRowToString
        |> List.intersperse "|"
        |> String.concat


stringToNewBoard : String -> Maybe NewBoard
stringToNewBoard string =
    if String.length string /= 8 * 8 + 7 then
        Nothing

    else
        let
            rows =
                [ String.slice 0 8 string
                , String.slice 9 17 string
                , String.slice 18 26 string
                , String.slice 27 35 string
                , String.slice 36 44 string
                , String.slice 45 53 string
                , String.slice 54 62 string
                , String.slice 63 71 string
                ]
        in
        rows
            |> List.map stringToNewRow
            |> Array.fromList
            |> Just


encodeNewBoard : NewBoard -> Value
encodeNewBoard board =
    JE.string <| newBoardToString board


newBoardDecoder : Decoder NewBoard
newBoardDecoder =
    JD.string
        |> JD.andThen
            (\string ->
                case stringToNewBoard string of
                    Nothing ->
                        JD.fail "Invalid board string."

                    Just board ->
                        JD.succeed board
            )


encodeScore : Score -> Value
encodeScore score =
    JE.object
        [ ( "games", JE.int score.games )
        , ( "whiteWins", JE.int score.whiteWins )
        , ( "blackWins", JE.int score.blackWins )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    JD.succeed Score
        |> required "games" JD.int
        |> required "whiteWins" JD.int
        |> required "blackWins" JD.int


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
encodePlayerNames { white, black } =
    JE.object
        [ ( "white", JE.string white )
        , ( "black", JE.string black )
        ]


playerNamesDecoder : Decoder PlayerNames
playerNamesDecoder =
    JD.succeed PlayerNames
        |> required "white" JD.string
        |> required "black" JD.string


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


encodeUndoState : UndoState -> Value
encodeUndoState { board, moves, selected, legalMoves } =
    JE.object
        [ ( "board", encodeNewBoard board )
        , ( "moves", JE.list JE.string moves )
        , ( "selected", encodeMaybe encodeRowCol selected )
        , ( "legalMoves", encodeMovesOrJumps legalMoves )
        ]


undoStateDecoder : Decoder UndoState
undoStateDecoder =
    JD.succeed UndoState
        |> required "board" newBoardDecoder
        |> required "moves" (JD.list JD.string)
        |> required "selected" (JD.nullable rowColDecoder)
        |> required "legalMoves" movesOrJumpsDecoder


encodeGameState : Bool -> GameState -> Value
encodeGameState includePrivate gameState =
    let
        { newBoard, moves, players, whoseTurn, selected, jumperLocations, legalMoves, undoStates, jumps, score, winner, path, testMode } =
            gameState

        privateValue =
            if includePrivate then
                encodePrivateGameState gameState.private

            else
                JE.null
    in
    JE.object
        [ ( "newBoard", encodeNewBoard newBoard )
        , ( "moves", encodeMoves moves )
        , ( "players", encodePlayerNames players )
        , ( "whoseTurn", encodePlayer whoseTurn )
        , ( "selected", encodeMaybe encodeRowCol selected )
        , ( "jumperLocations", JE.list encodeRowCol jumperLocations )
        , ( "legalMoves", encodeMovesOrJumps legalMoves )
        , ( "undoStates", JE.list encodeUndoState undoStates )
        , ( "jumps", encodeCorruptibleJumpSequence jumps )
        , ( "score", encodeScore score )
        , ( "winner", encodeWinner winner )
        , ( "path", JE.list encodeIntPair path )
        , ( "testMode", encodeMaybe encodeTestMode testMode )
        , ( "private", privateValue )
        ]


gameStateDecoder : Decoder GameState
gameStateDecoder =
    JD.succeed GameState
        |> required "newBoard" newBoardDecoder
        |> required "moves" movesDecoder
        |> required "players" playerNamesDecoder
        |> required "whoseTurn" playerDecoder
        |> required "selected" (JD.nullable rowColDecoder)
        |> optional "jumperLocations" (JD.list rowColDecoder) []
        |> required "legalMoves" movesOrJumpsDecoder
        |> required "undoStates" (JD.list undoStateDecoder)
        |> required "jumps" corruptibleJumpSequenceDecoder
        |> required "score" scoreDecoder
        |> required "winner" winnerDecoder
        |> required "path" (JD.list intPairDecoder)
        |> required "testMode" (JD.nullable testModeDecoder)
        |> required "private" privateGameStateDecoder


encodeRowCol : RowCol -> Value
encodeRowCol { row, col } =
    JE.object
        [ ( "row", JE.int row )
        , ( "col", JE.int col )
        ]


rowColDecoder : Decoder RowCol
rowColDecoder =
    JD.succeed RowCol
        |> required "row" JD.int
        |> required "col" JD.int


encodeRowColList : List RowCol -> Value
encodeRowColList rowcols =
    JE.list encodeRowCol rowcols


rowColListDecoder : Decoder (List RowCol)
rowColListDecoder =
    JD.list rowColDecoder


encodeJumpSequence : JumpSequence -> Value
encodeJumpSequence jumps =
    JE.list encodeOneJump jumps


jumpSequenceDecoder : Decoder JumpSequence
jumpSequenceDecoder =
    JD.list oneJumpDecoder


encodeCorruptibleJumpSequence : List OneCorruptibleJump -> Value
encodeCorruptibleJumpSequence jumps =
    JE.list encodeOneCorruptibleJump jumps


corruptibleJumpSequenceDecoder : Decoder (List OneCorruptibleJump)
corruptibleJumpSequenceDecoder =
    JD.list oneCorruptibleJumpDecoder


encodeOneJump : OneJump -> Value
encodeOneJump { over, to } =
    JE.object
        [ ( "over", encodeRowCol over )
        , ( "to", encodeRowCol to )
        ]


oneJumpDecoder : Decoder OneJump
oneJumpDecoder =
    JD.succeed OneJump
        |> required "over" rowColDecoder
        |> required "to" rowColDecoder


encodeOneCorruptibleJump : OneCorruptibleJump -> Value
encodeOneCorruptibleJump { over, to, corrupted } =
    JE.object
        [ ( "over", encodeRowCol over )
        , ( "to", encodeRowCol to )
        , ( "corrupted", JE.bool corrupted )
        ]


oneCorruptibleJumpDecoder : Decoder OneCorruptibleJump
oneCorruptibleJumpDecoder =
    JD.succeed OneCorruptibleJump
        |> required "over" rowColDecoder
        |> required "to" rowColDecoder
        |> required "corrupted" JD.bool


encodeMovesOrJumps : MovesOrJumps -> Value
encodeMovesOrJumps movesOrJumps =
    case movesOrJumps of
        NoMoves ->
            JE.string "NoMoves"

        Moves rowcols ->
            JE.object [ ( "moves", encodeRowColList rowcols ) ]

        Jumps jumps ->
            JE.object [ ( "jumps", JE.list encodeJumpSequence jumps ) ]


movesOrJumpsDecoder : Decoder MovesOrJumps
movesOrJumpsDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if "NoMoves" == s then
                        JD.succeed NoMoves

                    else
                        JD.fail <| "Unknown MovesOrJumps: " ++ s
                )
        , JD.field "moves" rowColListDecoder
            |> JD.andThen (\rowcols -> JD.succeed <| Moves rowcols)
        , JD.field "jumps" (JD.list jumpSequenceDecoder)
            |> JD.andThen (\sequences -> JD.succeed <| Jumps sequences)
        ]


encodeUndoWhichJumps : UndoWhichJumps -> Value
encodeUndoWhichJumps undoWhichJumps =
    JE.string
        (case undoWhichJumps of
            UndoOneJump ->
                "UndoOneJump"

            UndoAllJumps ->
                "UndoAllJumps"
        )


undoWhichJumpsDecoder : Decoder UndoWhichJumps
undoWhichJumpsDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                if s == "UndoOneJump" then
                    JD.succeed UndoOneJump

                else if s == "UndoAllJumps" then
                    JD.succeed UndoAllJumps

                else
                    JD.fail <| "Unknown UndoWhichJumps: " ++ s
            )


encodeChooseMoveOption : ChooseMoveOption -> Value
encodeChooseMoveOption option =
    case option of
        CorruptJumped ->
            JE.string "CorruptJumped"

        MakeHulk rowCol ->
            JE.object [ ( "MakeHulk", encodeRowCol rowCol ) ]


chooseMoveOptionDecoder : Decoder ChooseMoveOption
chooseMoveOptionDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\string ->
                    if string == "CorruptJumped" then
                        JD.succeed CorruptJumped

                    else
                        JD.fail <| "\"CorruptJumped\" /= \"" ++ string ++ "\""
                )
        , JD.field "MakeHulk" rowColDecoder
            |> JD.andThen
                (\rowCol ->
                    JD.succeed <| MakeHulk rowCol
                )
        ]


encodeChoice : Choice -> Value
encodeChoice choice =
    JE.object
        [ case choice of
            ChoosePiece rowCol ->
                ( "ChoosePiece", encodeRowCol rowCol )

            ChooseMove rowCol options ->
                ( "ChooseMove"
                , if options == [] then
                    encodeRowCol rowCol

                  else
                    JE.object
                        [ ( "rowCol", encodeRowCol rowCol )
                        , ( "options", JE.list encodeChooseMoveOption options )
                        ]
                )

            ChooseUndoJump undoWhichJumps ->
                ( "ChooseUndoJump", encodeUndoWhichJumps undoWhichJumps )

            ChooseResign player ->
                ( "ChooseResign", encodePlayer player )

            ChooseNew player ->
                ( "ChooseNew", encodePlayer player )
        ]


choiceDecoder : Decoder Choice
choiceDecoder =
    JD.oneOf
        [ JD.field "ChoosePiece" rowColDecoder
            |> JD.andThen (ChoosePiece >> JD.succeed)
        , JD.field "ChooseMove" <|
            JD.oneOf
                [ rowColDecoder
                    |> JD.andThen
                        (\rowCol ->
                            ChooseMove rowCol [] |> JD.succeed
                        )
                , JD.succeed ChooseMove
                    |> required "rowCol" rowColDecoder
                    |> required "options" (JD.list chooseMoveOptionDecoder)
                ]
        , JD.field "ChooseUndoJump" undoWhichJumpsDecoder
            |> JD.andThen (ChooseUndoJump >> JD.succeed)
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

        SetGameStateReq { playerid, gameState } ->
            ( Req "setGameState"
            , [ ( "playerid", JE.string playerid )
              , ( "gameState", encodeGameState includePrivate gameState )
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


setGameStateReqDecoder : Decoder Message
setGameStateReqDecoder =
    JD.succeed
        (\playerid gameState ->
            SetGameStateReq
                { playerid = playerid
                , gameState = gameState
                }
        )
        |> required "playerid" JD.string
        |> required "gameState" gameStateDecoder


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

                "setGameState" ->
                    decodePlist setGameStateReqDecoder plist

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
