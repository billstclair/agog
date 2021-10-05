---------------------------------------------------------------------
--
-- Types.elm
-- AGOG shared types.
-- Copyright (c) 2019-2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Agog.Types exposing
    ( ArchivedGameState
    , Board
    , ChatSettings
    , Choice(..)
    , ChooseMoveOption(..)
    , Color(..)
    , GameState
    , HulkAfterJump(..)
    , JumpSequence
    , Message(..)
    , MessageForLog(..)
    , MovesOrJumps(..)
    , NamedGame
    , OneCorruptibleJump
    , OneJump
    , OneMove
    , OneMoveSequence(..)
    , OneSlideRecord
    , Page(..)
    , Piece
    , PieceType(..)
    , Player(..)
    , PlayerNames
    , PrivateGameState
    , PublicGame
    , PublicType(..)
    , RotateBoard(..)
    , RowCol
    , SavedModel
    , Score
    , ServerInterface
    , ServerState
    , Settings
    , Socket
    , StatisticsKeys
    , Style
    , StyleType(..)
    , SubscriptionSet
    , TestMode
    , UndoState
    , UndoWhichJumps(..)
    , WinReason(..)
    , Winner(..)
    , darkStyle
    , defaultGamename
    , emptyPiece
    , emptyPlayerNames
    , emptyPrivateGameState
    , emptySettings
    , gameStateIsVerbose
    , gamesEqual
    , lightStyle
    , maybeMakeHulkOption
    , messageToGameid
    , messageToPlayer
    , messageToPlayerid
    , otherColor
    , otherPlayer
    , playerColor
    , posixZero
    , serverIsVerbose
    , statisticsKeyOrder
    , statisticsKeys
    , typeToStyle
    , updateResponseGameState
    , winPlayer
    , zeroScore
    )

import Array exposing (Array)
import Dict exposing (Dict)
import ElmChat
import Set exposing (Set)
import Time exposing (Posix)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , ServerUrl
        , Statistics
        )


type RotateBoard
    = RotateWhiteDown
    | RotatePlayerDown


type Color
    = BlackColor
    | WhiteColor


otherColor : Color -> Color
otherColor color =
    case color of
        BlackColor ->
            WhiteColor

        WhiteColor ->
            BlackColor


type PieceType
    = Golem
    | Hulk
    | CorruptedHulk
    | Journeyman
    | NoPiece


type alias Piece =
    { color : Color
    , pieceType : PieceType
    }


emptyPiece : Piece
emptyPiece =
    { color = BlackColor
    , pieceType = NoPiece
    }


type alias Board =
    Array (Array Piece)


type Player
    = WhitePlayer
    | BlackPlayer


otherPlayer : Player -> Player
otherPlayer player =
    if player == WhitePlayer then
        BlackPlayer

    else
        WhitePlayer


playerColor : Player -> Color
playerColor player =
    case player of
        WhitePlayer ->
            WhiteColor

        BlackPlayer ->
            BlackColor


type WinReason
    = WinByCapture
    | WinBySanctum
    | WinByImmobilization
    | WinByResignation


type Winner
    = NoWinner
    | WhiteWinner WinReason
    | BlackWinner WinReason


winPlayer : Winner -> Maybe Player
winPlayer winner =
    case winner of
        NoWinner ->
            Nothing

        WhiteWinner _ ->
            Just WhitePlayer

        BlackWinner _ ->
            Just BlackPlayer


type Page
    = MainPage
    | RulesPage
    | InstructionsPage
    | PublicPage
    | MovesPage
    | StatisticsPage


type alias Score =
    { games : Int
    , whiteWins : Int
    , blackWins : Int
    }


zeroScore : Score
zeroScore =
    { games = 0
    , whiteWins = 0
    , blackWins = 0
    }


type alias Style =
    { backgroundColor : String
    , lineColor : String
    , pathColor : String
    , alreadyFilledColor : String
    , arrowColor : String
    , shadeColor : String
    , selectedColor : String
    , moveColor : String
    , corruptedHulkXColor : String
    , lastMoveFromColor : String
    , lastMoveToColor : String
    }


lightStyle : Style
lightStyle =
    { backgroundColor = "white"
    , lineColor = "black"
    , pathColor = "orange"
    , alreadyFilledColor = "red"
    , arrowColor = "green"
    , shadeColor = "lightgray"
    , selectedColor = "black"
    , moveColor = "blue"
    , corruptedHulkXColor = "red"
    , lastMoveFromColor = "gold"
    , lastMoveToColor = "lightgreen"
    }


darkStyle : Style
darkStyle =
    { backgroundColor = "black"
    , lineColor = "#BBBBBB"
    , pathColor = "purple"
    , alreadyFilledColor = "red"
    , arrowColor = "green"
    , shadeColor = "lightgray"
    , selectedColor = "orange"
    , moveColor = "green"
    , corruptedHulkXColor = "red"
    , lastMoveFromColor = "gold"
    , lastMoveToColor = "lightgreen"
    }


type StyleType
    = LightStyle
    | DarkStyle
    | CustomStyle Style


typeToStyle : StyleType -> Style
typeToStyle styleType =
    case styleType of
        LightStyle ->
            lightStyle

        DarkStyle ->
            darkStyle

        CustomStyle style ->
            style


type alias Settings =
    { name : String
    , isPublic : Bool
    , forName : String
    , hideTitle : Bool
    }


emptySettings : Settings
emptySettings =
    { name = ""
    , isPublic = False
    , forName = ""
    , hideTitle = False
    }


type alias SavedModel =
    { gamename : String
    , gameGamename : String
    , page : Page
    , chooseFirst : Player
    , lastTestMode : Maybe TestMode
    , gameid : String
    , settings : Settings
    , styleType : StyleType
    , rotate : RotateBoard
    , notificationsEnabled : Bool
    , soundEnabled : Bool
    }



---
--- Talking to the server
---


type alias Socket =
    String


type alias SubscriptionSet =
    Set ( Socket, String )


type alias PrivateGameState =
    { verbose : Maybe Bool
    , subscribers : SubscriptionSet
    , statisticsSubscribers : Set Socket
    , statisticsChanged : Bool

    -- Milliseconds
    , startTime : Maybe Int
    , updateTime : Maybe Int
    }


emptyPrivateGameState : PrivateGameState
emptyPrivateGameState =
    { verbose = Nothing
    , subscribers = Set.empty
    , statisticsSubscribers = Set.empty
    , statisticsChanged = False
    , startTime = Nothing
    , updateTime = Nothing
    }


type alias OneJump =
    { over : RowCol
    , to : RowCol
    }


type alias JumpSequence =
    List OneJump


type MovesOrJumps
    = NoMoves
    | Moves (List RowCol)
    | Jumps (List JumpSequence)


type HulkAfterJump
    = NoHulkAfterJump
    | CorruptAfterJump
    | MakeHulkAfterJump RowCol


type alias OneCorruptibleJump =
    { from : RowCol
    , over : RowCol
    , to : RowCol
    , hulkAfterJump : HulkAfterJump
    }


type alias OneSlideRecord =
    { from : RowCol, to : RowCol, makeHulk : Maybe RowCol }


type OneMoveSequence
    = OneResign
    | OneSlide OneSlideRecord
    | OneJumpSequence (List OneCorruptibleJump)


type alias OneMove =
    { piece : Piece
    , isUnique : Bool
    , sequence : OneMoveSequence
    , winner : Winner
    , time : Posix
    }


posixZero : Posix
posixZero =
    Time.millisToPosix 0


type alias UndoState =
    { board : Board
    , moves : List OneMove -- in reverse order
    , selected : Maybe RowCol
    , legalMoves : MovesOrJumps
    }


type alias TestMode =
    { piece : Piece
    , clear : Bool
    }


type alias GameState =
    { newBoard : Board

    -- Reversed
    , moves : List OneMove
    , players : PlayerNames
    , whoseTurn : Player
    , selected : Maybe RowCol
    , jumperLocations : List RowCol
    , legalMoves : MovesOrJumps
    , undoStates : List UndoState
    , jumps : List OneCorruptibleJump
    , score : Score
    , winner : Winner
    , testMode : Maybe TestMode
    , private : PrivateGameState --not sent over the wire
    }


type alias ArchivedGameState =
    { moves : List OneMove
    , players : PlayerNames
    , winner : Winner
    }


type alias PlayerNames =
    { white : String
    , black : String
    }


emptyPlayerNames : PlayerNames
emptyPlayerNames =
    PlayerNames "" ""


type alias RowCol =
    { row : Int
    , col : Int
    }


type UndoWhichJumps
    = UndoOneJump
    | UndoAllJumps


type ChooseMoveOption
    = NoOption
    | CorruptJumped
    | MakeHulk RowCol


maybeMakeHulkOption : ChooseMoveOption -> Maybe RowCol
maybeMakeHulkOption option =
    case option of
        MakeHulk rc ->
            Just rc

        _ ->
            Nothing


type Choice
    = ChoosePiece RowCol
    | ChooseMove RowCol ChooseMoveOption
    | ChooseUndoJump UndoWhichJumps
    | ChooseResign Player
    | ChooseNew Player


type PublicType
    = NotPublic
    | EntirelyPublic
    | PublicFor String


updateResponseGameState : (GameState -> GameState) -> Message -> Message
updateResponseGameState updater message =
    case message of
        NewRsp ({ gameState } as rec) ->
            NewRsp { rec | gameState = updater gameState }

        JoinRsp ({ gameState } as rec) ->
            JoinRsp { rec | gameState = updater gameState }

        UpdateRsp ({ gameState } as rec) ->
            UpdateRsp { rec | gameState = updater gameState }

        PlayRsp ({ gameState } as rec) ->
            PlayRsp { rec | gameState = updater gameState }

        ResignRsp ({ gameState } as rec) ->
            ResignRsp { rec | gameState = updater gameState }

        AnotherGameRsp ({ gameState } as rec) ->
            AnotherGameRsp { rec | gameState = updater gameState }

        GameOverRsp ({ gameState } as rec) ->
            GameOverRsp { rec | gameState = updater gameState }

        _ ->
            message


type Message
    = NewReq
        { name : String
        , player : Player
        , publicType : PublicType
        , gamename : String
        , restoreState : Maybe GameState
        , maybeGameid : Maybe GameId
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gamename : String
        , gameState : GameState
        , wasRestored : Bool
        }
    | JoinReq
        { gameid : GameId
        , name : String
        , isRestore : Bool
        }
    | ReJoinReq
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRsp
        { gameid : GameId
        , playerid : Maybe PlayerId
        , player : Player
        , gameState : GameState
        , wasRestored : Bool
        }
    | LeaveReq { playerid : PlayerId }
    | LeaveRsp { gameid : GameId, player : Player }
      -- Disallowed if Agog.WhichServer.allowGameState is False
    | SetGameStateReq
        { playerid : PlayerId
        , gameState : GameState
        }
    | UpdateReq { playerid : PlayerId }
    | UpdateRsp
        { gameid : String
        , gameState : GameState
        }
      -- Game Play
    | PlayReq
        { playerid : PlayerId
        , placement : Choice
        }
    | PlayRsp
        { gameid : GameId
        , gameState : GameState
        }
    | ResignRsp
        { gameid : GameId
        , gameState : GameState
        , player : Player
        }
    | AnotherGameRsp
        { gameid : GameId
        , gameState : GameState
        , player : Player
        }
    | GameOverRsp
        { gameid : GameId
        , gameState : GameState
        }
      -- Public games
    | PublicGamesReq
        { subscribe : Bool
        , forName : String
        , gameid : Maybe GameId
        }
    | PublicGamesRsp { games : List PublicGame }
    | PublicGamesUpdateRsp
        { added : List PublicGame
        , removed : List String
        }
    | StatisticsReq
        { subscribe : Bool
        }
    | StatisticsRsp
        { statistics : Maybe Statistics
        , startTime : Maybe Int
        , updateTime : Maybe Int
        }
      -- Errors
    | ErrorRsp
        { request : String
        , text : String
        }
      -- Chat
    | ChatReq
        { playerid : String
        , text : String
        }
    | ChatRsp
        { gameid : String
        , name : String
        , text : String
        }


type alias PublicGame =
    { gameid : GameId
    , creator : String
    , player : Player
    , forName : Maybe String
    }


messageToPlayer : Message -> Maybe Player
messageToPlayer message =
    case message of
        NewReq { player } ->
            Just player

        NewRsp { player } ->
            Just player

        ResignRsp { player } ->
            Just player

        AnotherGameRsp { player } ->
            Just player

        _ ->
            Nothing


messageToPlayerid : Message -> Maybe PlayerId
messageToPlayerid message =
    case message of
        NewRsp { playerid } ->
            Just playerid

        JoinRsp { playerid } ->
            playerid

        LeaveReq { playerid } ->
            Just playerid

        UpdateReq { playerid } ->
            Just playerid

        PlayReq { playerid } ->
            Just playerid

        ChatReq { playerid } ->
            Just playerid

        _ ->
            Nothing


messageToGameid : Message -> Maybe GameId
messageToGameid message =
    case message of
        NewRsp { gameid } ->
            Just gameid

        ReJoinReq { gameid } ->
            Just gameid

        JoinReq { gameid } ->
            Just gameid

        JoinRsp { gameid } ->
            Just gameid

        LeaveRsp { gameid } ->
            Just gameid

        UpdateRsp { gameid } ->
            Just gameid

        PlayRsp { gameid } ->
            Just gameid

        ResignRsp { gameid } ->
            Just gameid

        AnotherGameRsp { gameid } ->
            Just gameid

        GameOverRsp { gameid } ->
            Just gameid

        ChatRsp { gameid } ->
            Just gameid

        _ ->
            Nothing


type MessageForLog
    = NewReqLog
        { name : String
        , player : Player
        , publicType : PublicType
        , gamename : String
        , restoreState : Maybe String
        , maybeGameid : Maybe GameId
        }
    | NewRspLog
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gamename : String
        , gameState : String
        , wasRestored : Bool
        }
    | JoinReqLog
        { gameid : GameId
        , name : String
        , isRestore : Bool
        }
    | RejoinReqLog
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRspLog
        { gameid : GameId
        , playerid : Maybe PlayerId
        , player : Player
        , gameState : String
        , wasRestored : Bool
        }
    | LeaveReqLog { playerid : PlayerId }
    | LeaveRspLog { gameid : GameId, player : Player }
      -- Disallowed if Agog.WhichServer.allowGameState is False
    | SetGameStateReqLog
        { playerid : PlayerId
        , gameState : String
        }
    | UpdateReqLog { playerid : PlayerId }
    | UpdateRspLog
        { gameid : String
        , gameState : String
        }
      -- Game Play
    | PlayReqLog
        { playerid : PlayerId
        , placement : Choice
        }
    | PlayRspLog
        { gameid : GameId
        , gameState : String
        }
    | ResignRspLog
        { gameid : GameId
        , gameState : String
        , player : Player
        }
    | AnotherGameRspLog
        { gameid : GameId
        , gameState : String
        , player : Player
        }
    | GameOverRspLog
        { gameid : GameId
        , gameState : String
        }
      -- Public games
    | PublicGamesReqLog
        { subscribe : Bool
        , forName : String
        , gameid : Maybe GameId
        }
    | PublicGamesRspLog { games : List PublicGame }
    | PublicGamesUpdateRspLog
        { added : List PublicGame
        , removed : List String
        }
    | StatisticsReqLog
        { subscribe : Bool
        }
    | StatisticsRspLog
        { statistics : Maybe Statistics
        , startTime : Maybe Int
        , updateTime : Maybe Int
        }
      -- Errors
    | ErrorRspLog
        { request : String
        , text : String
        }
      -- Chat
    | ChatReqLog
        { playerid : String
        , text : String
        }
    | ChatRspLog
        { gameid : String
        , name : String
        , text : String
        }


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player


serverIsVerbose : ServerState -> Bool
serverIsVerbose state =
    case state.state of
        Nothing ->
            False

        Just gs ->
            gameStateIsVerbose gs


gameStateIsVerbose : GameState -> Bool
gameStateIsVerbose gs =
    case gs.private.verbose of
        Nothing ->
            False

        Just v ->
            v


type alias StatisticsKeys =
    { finishedGames : String
    , totalMoves : String
    , whiteWon : String
    , blackWon : String
    , activeGames : String
    , totalConnections : String
    , totalPublicConnections : String
    , activeConnections : String
    }


statisticsKeys : StatisticsKeys
statisticsKeys =
    { finishedGames = "Finished Games"
    , totalMoves = "Finished Game Total Moves"
    , whiteWon = "White Won"
    , blackWon = "Black Won"
    , activeGames = "Active Games"
    , totalConnections = "Total Sessions"
    , totalPublicConnections = "Total Public Sessions"
    , activeConnections = "Active Sessions"
    }


statisticsKeyOrder : List (StatisticsKeys -> String)
statisticsKeyOrder =
    [ .finishedGames
    , .totalMoves
    , .whiteWon
    , .blackWon
    , .activeGames
    , .totalConnections
    , .totalPublicConnections
    , .activeConnections
    ]


type alias ServerInterface msg =
    WebSocketFramework.Types.ServerInterface GameState Player Message msg


type alias ChatSettings msg =
    ElmChat.Settings msg


defaultGamename : String
defaultGamename =
    "default"


type alias NamedGame msg =
    { gamename : String
    , gameid : GameId
    , gameState : GameState
    , isLocal : Bool
    , serverUrl : String
    , otherPlayerid : PlayerId
    , player : Player
    , playerid : PlayerId
    , isLive : Bool
    , yourWins : Int

    -- Not persistent
    , interfaceIsProxy : Bool
    , interface : ServerInterface msg
    }


{-| Should probably handle interface specially, since it's uncomparable
-}
gamesEqual : NamedGame msg -> NamedGame msg -> Bool
gamesEqual g1 g2 =
    (g1.gamename == g2.gamename)
        && (g1.gameid == g2.gameid)
        && (g1.gameState == g2.gameState)
        && (g1.isLocal == g2.isLocal)
        && (g1.serverUrl == g2.serverUrl)
        && (g1.otherPlayerid == g2.otherPlayerid)
        && (g1.player == g2.player)
        && (g1.playerid == g2.playerid)
        && (g1.isLive == g2.isLive)
        && (g1.yourWins == g2.yourWins)
