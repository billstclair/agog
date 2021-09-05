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
    ( Board
    , Choice(..)
    , Color(..)
    , Decoration(..)
    , GameState
    , JumpSequence
    , Message(..)
    , MovesOrJumps(..)
    , NewBoard
    , OneJump
    , OneScore
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
    , ServerState
    , Settings
    , Socket
    , Style
    , StyleType(..)
    , SubscriptionSet
    , TestMode
    , UndoState
    , Winner(..)
    , darkStyle
    , emptyPiece
    , emptyPrivateGameState
    , emptySettings
    , lightStyle
    , messageToGameid
    , messageToPlayer
    , messageToPlayerid
    , otherColor
    , otherPlayer
    , typeToStyle
    , zeroOneScore
    , zeroScore
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , ServerUrl
        )


type alias Board =
    Array (Array Bool)


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


type alias NewBoard =
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


type Winner
    = NoWinner
    | WhiteWinner
    | BlackWinner


type alias PieceSelected =
    { selected : ( Int, Int )
    , moves : List ( Int, Int )
    , jumps : List (List ( Int, Int ))
    }


type Decoration
    = NoDecoration
    | PieceSelectedDecoration PieceSelected


type Page
    = MainPage
    | RulesPage
    | InstructionsPage
    | PublicPage


type alias OneScore =
    { games : Int
    , score : Int
    }


zeroOneScore : OneScore
zeroOneScore =
    { games = 0
    , score = 0
    }


type alias Score =
    Dict String OneScore


zeroScore : Score
zeroScore =
    Dict.empty


type alias Style =
    { backgroundColor : String
    , lineColor : String
    , pathColor : String
    , alreadyFilledColor : String
    , arrowColor : String
    , highlightOpacity : String
    , compassColor : String
    , compassOpacity : String
    , shadeColor : String
    , selectedColor : String
    , moveColor : String
    }


lightStyle : Style
lightStyle =
    { backgroundColor = "white"
    , lineColor = "black"
    , pathColor = "orange"
    , alreadyFilledColor = "red"
    , arrowColor = "green"
    , highlightOpacity = "0.3"
    , compassColor = "black"
    , compassOpacity = "0.5"
    , shadeColor = "lightgray"
    , selectedColor = "black"
    , moveColor = "lightblue"
    }


darkStyle : Style
darkStyle =
    { backgroundColor = "black"
    , lineColor = "#BBBBBB"
    , pathColor = "purple"
    , alreadyFilledColor = "red"
    , arrowColor = "green"
    , highlightOpacity = "0.4"
    , compassColor = "white"
    , compassOpacity = "0.3"
    , shadeColor = "lightgray"
    , selectedColor = "darkgray"
    , moveColor = "blue"
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
    { page : Page
    , decoration : Decoration
    , otherDecoration : Decoration
    , firstSelection : Decoration
    , chooseFirst : Player
    , player : Player
    , gameState : GameState
    , isLocal : Bool
    , isLive : Bool
    , gameid : String
    , playerid : String
    , settings : Settings
    , styleType : StyleType
    }



---
--- Talking to the server
---


type alias Socket =
    String


type alias SubscriptionSet =
    Set ( Socket, String )


type alias PrivateGameState =
    { decoration : Decoration
    , subscribers : SubscriptionSet
    }


emptyPrivateGameState : PrivateGameState
emptyPrivateGameState =
    PrivateGameState NoDecoration Set.empty


type alias OneJump =
    { over : RowCol
    , to : RowCol
    }


type alias JumpSequence =
    List OneJump


type MovesOrJumps
    = Moves (List RowCol)
    | Jumps (List JumpSequence)


type alias UndoState =
    { board : NewBoard
    , selected : Maybe RowCol
    }


type alias TestMode =
    { piece : Piece
    , clear : Bool
    }


type alias GameState =
    { board : Board
    , newBoard : NewBoard
    , moves : List String
    , players : PlayerNames
    , whoseTurn : Player
    , selected : Maybe RowCol
    , legalMoves : MovesOrJumps
    , undoStates : List UndoState
    , score : Score
    , winner : Winner
    , path : List ( Int, Int )
    , testMode : Maybe TestMode
    , private : PrivateGameState --not sent over the wire
    }


type alias PlayerNames =
    { white : String
    , black : String
    }


type alias RowCol =
    { row : Int
    , col : Int
    }


type Choice
    = ChoosePiece RowCol
    | ChooseMove RowCol
    | ChooseResign Player
    | ChooseNew Player


type PublicType
    = NotPublic
    | EntirelyPublic
    | PublicFor String


type Message
    = NewReq
        { name : String
        , player : Player
        , publicType : PublicType
        , restoreState : Maybe GameState
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gameState : GameState
        }
    | JoinReq
        { gameid : GameId
        , name : String
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
        }
    | LeaveReq { playerid : PlayerId }
    | LeaveRsp { gameid : GameId, player : Player }
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
        , decoration : Decoration
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


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player
