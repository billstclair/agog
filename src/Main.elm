---------------------------------------------------------------------
--
-- Main.elm
-- AGoG top-level
-- Copyright (c) 2019-2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------
---
--- TODO:
---
--- Zooming the screen should make the board bigger, but not fit.
--- This means that sizing should depend on screen size, not browser
--- window size. In any case, we need some way to be able to choose
--- pieces on a small screen.
---
----------------------------------------------------------------------


module Main exposing (main)

import Agog.Documentation as Documentation
import Agog.EncodeDecode as ED
import Agog.Interface as Interface
import Agog.NewBoard as NewBoard exposing (SizerKind(..), rc)
import Agog.Types as Types
    exposing
        ( Choice(..)
        , ChooseMoveOption(..)
        , Color(..)
        , Decoration(..)
        , GameState
        , Message(..)
        , MovesOrJumps(..)
        , NewBoard
        , OneMove
        , Page(..)
        , PieceType(..)
        , Player(..)
        , PlayerNames
        , PublicGame
        , PublicType(..)
        , RotateBoard(..)
        , RowCol
        , SavedModel
        , Score
        , Settings
        , StatisticsKeys
        , Style
        , StyleType(..)
        , TestMode
        , UndoWhichJumps(..)
        , WinReason(..)
        , Winner(..)
        , statisticsKeys
        )
import Agog.WhichServer as WhichServer
import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import DateFormat.Relative
import Dict exposing (Dict)
import ElmChat exposing (LineSpec(..), defaultExtraAttributes)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h1
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , optgroup
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , tr
        )
import Html.Attributes as Attributes
    exposing
        ( align
        , alt
        , autofocus
        , checked
        , class
        , cols
        , colspan
        , disabled
        , height
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Html.Lazy as Lazy
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
import Markdown
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Random exposing (Seed)
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , strokeDasharray
        , strokeWidth
        , textAnchor
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Svg.Button as SB exposing (Button, Content(..))
import Svg.Events
import Task
import Time exposing (Month(..), Posix, Zone)
import Url exposing (Url)
import WebSocketFramework
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , GameId
        , MessageDecoder
        , MessageEncoder
        , PlayerId
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        , Statistics
        )


type alias ServerInterface =
    WebSocketFramework.Types.ServerInterface GameState Player Message Msg


type ConnectionReason
    = NoConnection
    | StartGameConnection
    | JoinGameConnection
    | PublicGamesConnection
    | StatisticsConnection
    | UpdateConnection


type alias ChatSettings =
    ElmChat.Settings Msg


type AskYesNo a
    = AskAsk
    | AskYes a
    | AskNo


type alias ChooseMoveOptionsUI =
    { corruptJumped : AskYesNo ()
    , makeHulk : AskYesNo RowCol
    }


chooseMoveOptionsUINo : ChooseMoveOptionsUI
chooseMoveOptionsUINo =
    { corruptJumped = AskNo
    , makeHulk = AskNo
    }


type alias Model =
    { tick : Posix
    , serverUrl : String
    , interface : ServerInterface
    , interfaceIsProxy : Bool
    , connectionReason : ConnectionReason
    , funnelState : State
    , otherPlayerid : PlayerId
    , key : Key
    , windowSize : ( Int, Int )
    , started : Bool --True when persistent storage is available
    , error : Maybe String
    , chatSettings : ChatSettings
    , publicGames : List PublicGame
    , time : Posix
    , requestedNew : Bool
    , chooseMoveOptionsUI : ChooseMoveOptionsUI
    , delayedClick : Maybe RowCol
    , reallyClearStorage : Bool
    , statistics : Maybe Statistics
    , statisticsTimes : ( Maybe Int, Maybe Int )

    -- persistent below here
    , page : Page
    , decoration : Decoration
    , otherDecoration : Decoration
    , firstSelection : Decoration
    , chooseFirst : Player
    , player : Player
    , gameState : GameState
    , isLocal : Bool
    , lastTestMode : Maybe TestMode
    , gameid : String
    , playerid : PlayerId
    , isLive : Bool
    , settings : Settings
    , styleType : StyleType
    , rotate : RotateBoard
    , yourWins : Int
    }


isPlaying : Model -> Bool
isPlaying model =
    let
        { white, black } =
            model.gameState.players
    in
    model.isLive && white /= "" && black /= ""


type Msg
    = Noop
    | Tick Posix
    | IncomingMessage ServerInterface Message
    | SetDecoration Decoration
    | SetChooseFirst Player
    | SetRotate RotateBoard
    | SetIsLocal Bool
    | SetDarkMode Bool
    | SetName String
    | SetIsPublic Bool
    | SetForName String
    | SetServerUrl String
    | SetGameid String
    | SetPage Page
    | SetHideTitle Bool
    | ResetScore
    | NewGame
    | StartGame
    | Join
    | JoinGame GameId
    | Disconnect
    | SetTestMode Bool
    | EraseBoard
    | InitialBoard
    | SetTestClear Bool
    | SetTestColor Color
    | SetTestPieceType String
    | MaybeClearStorage
    | ClearStorage
    | Click ( Int, Int )
    | CorruptJumpedUI (AskYesNo ())
    | MakeHulkUI (AskYesNo RowCol)
    | SendUndoJumps UndoWhichJumps
    | ChatUpdate ChatSettings (Cmd Msg)
    | ChatSend String ChatSettings
    | ChatClear
    | DelayedAction (Model -> ( Model, Cmd Msg )) Posix
    | SetZone Zone
    | WindowResize Int Int
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | Process Value


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoderWithPrivate
    , decoder = ED.messageDecoder
    , errorWrapper = Just errorMessageEncoder
    }


errorMessageEncoder : WebSocketFramework.Types.Error Message -> Message
errorMessageEncoder error =
    ErrorRsp
        { request = "Unknown"
        , text =
            Debug.toString error.kind
                ++ ":"
                ++ error.description
                ++ " / "
                ++ (case error.message of
                        Ok message ->
                            Debug.toString message

                        Err err ->
                            err
                   )
        }


fullProcessor : ServerMessageProcessor GameState Player Message
fullProcessor =
    ServerInterface.fullMessageProcessor encodeDecode Interface.proxyMessageProcessor


proxyServer : ServerInterface
proxyServer =
    ServerInterface.makeProxyServer fullProcessor IncomingMessage


updateChatAttributes : Int -> StyleType -> ChatSettings -> ChatSettings
updateChatAttributes bsize styleType settings =
    let
        renderStyle =
            Types.typeToStyle styleType

        attributes =
            settings.attributes
    in
    { settings
        | attributes =
            { attributes
                | chatTable =
                    [ style "width" "fit-content"
                    , style "max-width" "90%"
                    , style "margin" "auto"
                    ]
                , textColumn =
                    [ style "width" "fit-content"
                    ]
                , textArea =
                    [ style "width" <| String.fromInt (5 * bsize // 6) ++ "px"
                    , style "height" "6em"
                    , style "border-color" renderStyle.lineColor
                    ]
            }
    }


initialChatSettings : Style -> ChatSettings
initialChatSettings style =
    ElmChat.makeSettings ids.chatOutput 14 True ChatUpdate


init : Value -> url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { tick = Time.millisToPosix 0
            , serverUrl = WhichServer.serverUrl
            , interface = proxyServer
            , interfaceIsProxy = True
            , connectionReason = NoConnection
            , funnelState = initialFunnelState
            , otherPlayerid = ""
            , key = key
            , windowSize = ( 0, 0 )
            , started = False
            , error = Nothing
            , chatSettings = initialChatSettings (Types.typeToStyle LightStyle)
            , publicGames = []
            , time = Time.millisToPosix 0
            , requestedNew = False
            , chooseMoveOptionsUI = chooseMoveOptionsUINo
            , delayedClick = Nothing
            , reallyClearStorage = False
            , statistics = Nothing
            , statisticsTimes = ( Nothing, Nothing )
            , styleType = LightStyle
            , lastTestMode = Nothing

            -- persistent fields
            , page = MainPage
            , decoration = NoDecoration
            , otherDecoration = NoDecoration
            , firstSelection = NoDecoration
            , chooseFirst = WhitePlayer
            , player = WhitePlayer
            , gameState = Interface.emptyGameState (PlayerNames "" "")
            , isLocal = False
            , gameid = ""
            , playerid = ""
            , isLive = False
            , settings = Types.emptySettings
            , rotate = RotateWhiteDown
            , yourWins = 0
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , Task.perform SetZone Time.here
            , Task.perform Tick Time.now
            ]


type alias NewReqBody =
    { name : String
    , player : Player
    , publicType : PublicType
    , restoreState : Maybe GameState
    }


initialNewReqBody : NewReqBody
initialNewReqBody =
    { name = "White"
    , player = WhitePlayer
    , publicType = NotPublic
    , restoreState = Nothing
    }


initialNewReq : Message
initialNewReq =
    NewReq initialNewReqBody


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if LocalStorage.isLoaded state.storage then
                        True

                    else
                        model.started
            }

        cmd =
            if mdl.started && not model.started then
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    handleGetResponse key v model

        _ ->
            mdl |> withCmd cmd


handleGetResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse key value model =
    if key == pk.chat then
        case
            JD.decodeValue (ElmChat.settingsDecoder ChatUpdate) value
        of
            Err _ ->
                model |> withNoCmd

            Ok settings ->
                let
                    chatSettings =
                        { settings
                            | id = ids.chatOutput
                            , zone = model.chatSettings.zone
                        }
                in
                { model
                    | chatSettings = chatSettings
                }
                    |> withCmd (ElmChat.restoreScroll chatSettings)

    else if key == pk.model then
        let
            cmd =
                get pk.chat
        in
        case Debug.log "decodeSavedModel" <| ED.decodeSavedModel value of
            Err e ->
                model |> withCmd cmd

            Ok savedModel ->
                let
                    model2 =
                        savedModelToModel savedModel model

                    ( model3, cmd2 ) =
                        if not model2.isLocal && model2.isLive && model2.playerid /= "" then
                            model2
                                |> webSocketConnect UpdateConnection

                        else if not model2.isLocal && model2.page == PublicPage then
                            { model2 | gameid = "" }
                                |> webSocketConnect PublicGamesConnection

                        else if not model2.isLocal && model2.page == StatisticsPage then
                            { model2 | gameid = "" }
                                |> webSocketConnect StatisticsConnection

                        else if model2.isLocal then
                            { model2 | gameid = "" }
                                |> withCmd (initialNewReqCmd model2)

                        else
                            model2 |> withNoCmd
                in
                model3 |> withCmds [ cmd, cmd2 ]

    else
        model |> withNoCmd


initialNewReqCmd : Model -> Cmd Msg
initialNewReqCmd model =
    send model <|
        NewReq
            { initialNewReqBody
                | restoreState =
                    Just model.gameState
            }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { page = model.page
    , decoration = model.decoration
    , otherDecoration = model.otherDecoration
    , firstSelection = model.firstSelection
    , chooseFirst = model.chooseFirst
    , player = model.player
    , gameState = model.gameState
    , isLocal = model.isLocal
    , isLive = model.isLive
    , gameid = model.gameid
    , playerid = model.playerid
    , settings = model.settings
    , styleType = model.styleType
    , rotate = model.rotate
    , yourWins = model.yourWins
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | page = savedModel.page
        , decoration = savedModel.decoration
        , otherDecoration = savedModel.otherDecoration
        , firstSelection = savedModel.firstSelection
        , chooseFirst = savedModel.chooseFirst
        , player = savedModel.player
        , gameState = savedModel.gameState
        , isLocal = savedModel.isLocal
        , isLive = savedModel.isLive
        , gameid = savedModel.gameid
        , playerid = savedModel.playerid
        , settings = savedModel.settings
        , styleType = savedModel.styleType
        , rotate = savedModel.rotate
        , yourWins = savedModel.yourWins
        , interface = proxyServer
    }


playerName : Player -> Model -> String
playerName player model =
    let
        players =
            model.gameState.players
    in
    case player of
        WhitePlayer ->
            players.white

        BlackPlayer ->
            players.black


incomingMessage : ServerInterface -> Message -> Model -> ( Model, Cmd Msg )
incomingMessage interface message mdl =
    let
        model =
            { mdl
                | interface = interface
                , reallyClearStorage = False
            }

        msgString =
            Debug.log "incomingMessage" <| ED.encodeMessageForLog message
    in
    case message of
        NewRsp { gameid, playerid, player, name, gameState } ->
            let
                ( chatSettings, chatCmd ) =
                    clearChatSettings True model.chatSettings
            in
            { model
                | gameid = gameid
                , playerid = playerid
                , player = player
                , isLive = True
                , connectionReason = JoinGameConnection
                , gameState = gameState
                , decoration = gameState.private.decoration
                , yourWins = 0
                , chatSettings = chatSettings
            }
                |> withCmds
                    [ chatCmd
                    , if not model.isLocal then
                        Cmd.none

                      else if player == WhitePlayer then
                        send model <|
                            JoinReq { gameid = gameid, name = "Black" }

                      else
                        send model <|
                            JoinReq { gameid = gameid, name = "White" }
                    ]

        JoinRsp { gameid, playerid, player, gameState } ->
            let
                ( chatSettings, chatCmd ) =
                    clearChatSettings True model.chatSettings

                model2 =
                    { model
                        | gameState = gameState
                        , isLive = True
                        , connectionReason = NoConnection
                        , decoration = gameState.private.decoration
                        , player =
                            if playerid == Nothing then
                                model.player

                            else
                                player
                        , yourWins = 0
                        , chatSettings = chatSettings
                    }

                model3 =
                    if model2.isLocal then
                        { model2
                            | otherPlayerid =
                                case playerid of
                                    Just p ->
                                        p

                                    Nothing ->
                                        ""
                        }

                    else
                        case playerid of
                            Nothing ->
                                model2

                            Just pid ->
                                { model2 | playerid = pid }
            in
            model3
                |> withCmds
                    [ chatCmd
                    , setPage MainPage
                    ]

        LeaveRsp { gameid, player } ->
            let
                model2 =
                    { model
                        | gameid = ""
                        , playerid = ""
                        , otherPlayerid = ""
                        , error =
                            if player == model.player then
                                Nothing

                            else
                                let
                                    name =
                                        playerName
                                            (Types.otherPlayer model.player)
                                            model
                                in
                                Just <| name ++ " left"
                    }
            in
            if model.isLocal then
                model2 |> withNoCmd

            else
                { model2 | isLive = False }
                    |> withCmd
                        (Cmd.none
                         --WebSocket.makeClose model.serverUrl
                         --|> webSocketSend
                        )

        UpdateRsp { gameid, gameState } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , otherDecoration = NoDecoration
                , firstSelection = NoDecoration
            }
                |> withNoCmd

        PlayRsp { gameid, gameState, decoration } ->
            if not model.isLocal then
                let
                    ( idx, od ) =
                        ( 0, NoDecoration )

                    ( d, od2 ) =
                        if idx < 0 then
                            ( model.decoration
                            , if model.decoration == NoDecoration then
                                decoration

                              else
                                NoDecoration
                            )

                        else
                            ( decoration, NoDecoration )
                in
                { model
                    | gameState = gameState
                    , decoration = d
                    , otherDecoration = od2
                    , yourWins = computeYourWins gameState model
                }
                    |> withNoCmd

            else
                let
                    ( newDecoration, firstSelection ) =
                        ( NoDecoration, decoration )
                in
                { model
                    | gameState = gameState
                    , decoration = newDecoration
                    , firstSelection = firstSelection
                }
                    |> withNoCmd

        ResignRsp { gameid, gameState, player } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , otherDecoration = NoDecoration
                , firstSelection = NoDecoration
                , yourWins =
                    computeYourWins gameState model
                , error =
                    if model.isLocal then
                        Nothing

                    else if model.player == player then
                        Just "You resigned."

                    else
                        Just <| playerName player model ++ " resigned."
            }
                |> withNoCmd

        AnotherGameRsp { gameid, gameState, player } ->
            { model
                | requestedNew = False
                , gameState = gameState
                , decoration = NoDecoration
                , otherDecoration = NoDecoration
                , firstSelection = NoDecoration
                , player = player
                , error =
                    if not model.isLocal && not model.requestedNew then
                        let
                            name =
                                playerName (Types.otherPlayer player)
                                    { model | gameState = gameState }
                        in
                        Just <| name ++ " asked for a new game"

                    else
                        Nothing
            }
                |> withNoCmd

        GameOverRsp { gameid, gameState } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , otherDecoration = NoDecoration
                , firstSelection = NoDecoration
            }
                |> withNoCmd

        PublicGamesRsp { games } ->
            { model | publicGames = games }
                |> withNoCmd

        PublicGamesUpdateRsp { added, removed } ->
            let
                games =
                    List.filter
                        (\{ gameid } -> not <| List.member gameid removed)
                        model.publicGames
            in
            { model | publicGames = List.concat [ games, added ] }
                |> withNoCmd

        StatisticsRsp { statistics, startTime, updateTime } ->
            { model
                | statistics = statistics
                , statisticsTimes = ( startTime, updateTime )
            }
                |> withNoCmd

        ErrorRsp { request, text } ->
            { model | error = Just text }
                |> withNoCmd

        ChatRsp { gameid, name, text } ->
            let
                ( chatSettings, cmd ) =
                    ElmChat.addLineSpec model.chatSettings <|
                        ElmChat.makeLineSpec text
                            (Just name)
                            (Just model.time)
            in
            { model | chatSettings = chatSettings }
                |> withCmds
                    [ cmd

                    -- Kluge. ElmChat is supposed to do this
                    , Task.attempt (\_ -> Noop) <|
                        Dom.setViewportOf ids.chatOutput 0 1000000
                    ]

        _ ->
            model |> withNoCmd


computeYourWins : GameState -> Model -> Int
computeYourWins gameState model =
    let
        winner =
            gameState.winner
    in
    Debug.log "  yourWins" <|
        if
            (Debug.log "PlayRsp, winner" winner /= NoWinner)
                && (Debug.log "  model winner" model.gameState.winner == NoWinner)
                && ((Debug.log "  player" <| Just model.player) == (Debug.log "  winPlayer" <| Types.winPlayer winner))
        then
            model.yourWins + 1

        else
            model.yourWins


setPage : Page -> Cmd Msg
setPage page =
    Task.perform SetPage <| Task.succeed page


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | error = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse received ->
            let
                string =
                    received.message
            in
            case WSFED.decodeMessage ED.messageDecoder string of
                Err errmsg ->
                    { model | error = Just errmsg }
                        |> withNoCmd

                Ok message ->
                    { model | error = Nothing }
                        |> withCmd
                            (Task.perform (IncomingMessage model.interface) <|
                                Task.succeed message
                            )

        ClosedResponse { expected, reason } ->
            { model
                | isLive = False
                , connectionReason = NoConnection
                , error =
                    if Debug.log "ClosedResponse, expected" expected then
                        model.error

                    else
                        Just <| "Connection unexpectedly closed: " ++ reason
            }
                |> withNoCmd

        ConnectedResponse _ ->
            { model | error = Nothing }
                |> withCmd
                    (case model.connectionReason of
                        NoConnection ->
                            Cmd.none

                        StartGameConnection ->
                            let
                                settings =
                                    model.settings
                            in
                            send model <|
                                NewReq
                                    { name = model.settings.name
                                    , player = model.chooseFirst
                                    , publicType =
                                        if not settings.isPublic then
                                            NotPublic

                                        else
                                            case settings.forName of
                                                "" ->
                                                    EntirelyPublic

                                                forName ->
                                                    PublicFor forName
                                    , restoreState = Nothing
                                    }

                        JoinGameConnection ->
                            send model <|
                                JoinReq
                                    { gameid = model.gameid
                                    , name = model.settings.name
                                    }

                        PublicGamesConnection ->
                            send model <|
                                PublicGamesReq
                                    { subscribe = model.page == PublicPage
                                    , forName = model.settings.name
                                    , gameid = Just model.gameid
                                    }

                        StatisticsConnection ->
                            send model <|
                                StatisticsReq
                                    { subscribe = model.page == StatisticsPage
                                    }

                        UpdateConnection ->
                            send model <|
                                UpdateReq
                                    { playerid = model.playerid }
                    )

        _ ->
            model |> withNoCmd


focusId : String -> Cmd Msg
focusId id =
    Task.attempt (\_ -> Noop) (Dom.focus id)


onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    on "keydown" (JD.map tagger keyCode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( mdl, cmd ) =
            updateInternal msg model

        { white, black } =
            mdl.gameState.players

        focus =
            --not mdl.isLocal && mdl.isLive && white /= "" && black /= ""
            --might be able to be smart and do this just on desktop, but not for now
            False

        doSave =
            case msg of
                Noop ->
                    False

                Click _ ->
                    cmd == Cmd.none

                NewGame ->
                    False

                Process _ ->
                    False

                IncomingMessage _ _ ->
                    -- cmd == Cmd.none
                    True

                MaybeClearStorage ->
                    False

                ClearStorage ->
                    False

                ChatUpdate _ _ ->
                    False

                ChatSend _ _ ->
                    False

                ChatClear ->
                    False

                DelayedAction _ _ ->
                    False

                _ ->
                    True
    in
    mdl
        |> withCmds
            [ cmd
            , if focus && doSave then
                focusId ids.chatInput

              else
                Cmd.none
            , if model.started && doSave then
                putModel mdl

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    let
        gameState =
            model.gameState

        settings =
            model.settings
    in
    case msg of
        Noop ->
            model |> withNoCmd

        Tick posix ->
            { model | tick = posix }
                |> withNoCmd

        IncomingMessage interface message ->
            incomingMessage interface message model

        SetDecoration decoration ->
            { model | decoration = decoration }
                |> withNoCmd

        SetChooseFirst player ->
            { model | chooseFirst = player }
                |> withNoCmd

        SetRotate rotate ->
            { model | rotate = rotate }
                |> withNoCmd

        SetIsLocal isLocal ->
            let
                model2 =
                    { model
                        | isLocal = isLocal
                        , isLive = False
                        , gameid = ""
                        , playerid = ""
                        , otherPlayerid = ""
                        , interface =
                            if isLocal then
                                proxyServer

                            else
                                model.interface
                    }
            in
            model2
                |> withCmd
                    (if isLocal && not model.isLocal then
                        Cmd.batch
                            [ if model.isLive then
                                send model <|
                                    LeaveReq { playerid = model.playerid }

                              else
                                Cmd.none
                            , send model2 <| NewReq initialNewReqBody
                            ]

                     else
                        Cmd.none
                    )

        SetDarkMode darkMode ->
            let
                styleType =
                    if darkMode then
                        DarkStyle

                    else
                        LightStyle
            in
            { model | styleType = styleType }
                |> withNoCmd

        SetName name ->
            { model | settings = { settings | name = name } }
                |> withNoCmd

        SetIsPublic isPublic ->
            { model | settings = { settings | isPublic = isPublic } }
                |> withCmd
                    (if isPublic && not settings.isPublic then
                        focusId ids.forName

                     else
                        Cmd.none
                    )

        SetForName forName ->
            { model | settings = { settings | forName = forName } }
                |> withNoCmd

        SetServerUrl serverUrl ->
            { model | serverUrl = serverUrl }
                |> withNoCmd

        SetGameid gameid ->
            { model | gameid = gameid }
                |> withNoCmd

        SetPage page ->
            let
                mdl =
                    { model | page = page }

                ( mdl2, cmd ) =
                    if page == PublicPage then
                        webSocketConnect PublicGamesConnection mdl

                    else if page == StatisticsPage then
                        webSocketConnect StatisticsConnection mdl

                    else
                        ( mdl, Cmd.none )

                cmd2 =
                    if page == StatisticsPage then
                        send mdl2 <|
                            StatisticsReq { subscribe = True }

                    else if model.page == StatisticsPage then
                        send mdl2 <|
                            StatisticsReq { subscribe = False }

                    else if page == MainPage then
                        ElmChat.restoreScroll model.chatSettings

                    else
                        Cmd.none

                cmd3 =
                    if page == PublicPage then
                        send mdl2 <|
                            PublicGamesReq
                                { subscribe = True
                                , forName = ""
                                , gameid = Nothing
                                }

                    else if model.page == PublicPage then
                        send mdl2 <|
                            PublicGamesReq
                                { subscribe = False
                                , forName = ""
                                , gameid = Nothing
                                }

                    else
                        Cmd.none
            in
            mdl2 |> withCmds [ cmd, cmd2, cmd3 ]

        SetHideTitle hideTitle ->
            { model | settings = { settings | hideTitle = hideTitle } }
                |> withNoCmd

        ResetScore ->
            if not <| model.isLocal then
                model |> withNoCmd

            else
                case model.interface of
                    ServerInterface si ->
                        case si.state of
                            Nothing ->
                                model |> withNoCmd

                            Just s ->
                                case si.state of
                                    Nothing ->
                                        model |> withNoCmd

                                    Just state ->
                                        let
                                            gs =
                                                { gameState
                                                    | score = Types.zeroScore
                                                }
                                        in
                                        { model
                                            | gameState = gs
                                            , interface =
                                                ServerInterface
                                                    { si
                                                        | state =
                                                            Just <|
                                                                ServerInterface.updateGame
                                                                    model.gameid
                                                                    gs
                                                                    state
                                                    }
                                        }
                                            |> withNoCmd

        NewGame ->
            let
                resigning =
                    if not model.isLocal then
                        model.player

                    else
                        gameState.whoseTurn

                pid =
                    if not model.isLocal then
                        model.playerid

                    else
                        case resigning of
                            WhitePlayer ->
                                model.playerid

                            BlackPlayer ->
                                model.otherPlayerid

                ( playerid, placement ) =
                    if gameState.winner == NoWinner then
                        ( pid
                        , ChooseResign resigning
                        )

                    else
                        let
                            player =
                                if model.isLocal then
                                    WhitePlayer

                                else
                                    -- This should probably be enforced
                                    -- by the server.
                                    Types.otherPlayer model.player
                        in
                        ( model.playerid, ChooseNew player )
            in
            { model | requestedNew = True }
                |> withCmd
                    (send model <|
                        PlayReq
                            { playerid = playerid
                            , placement = placement
                            }
                    )

        StartGame ->
            startGame model

        Join ->
            join model

        JoinGame gameid ->
            join { model | gameid = gameid }

        Disconnect ->
            disconnect model

        SetTestMode isTestMode ->
            let
                gs =
                    { gameState
                        | selected = Nothing
                        , testMode =
                            if isTestMode then
                                case model.lastTestMode of
                                    Nothing ->
                                        Just
                                            { piece = { pieceType = Golem, color = WhiteColor }
                                            , clear = False
                                            }

                                    jtm ->
                                        jtm

                            else
                                Nothing
                    }

                cmd =
                    if not isTestMode then
                        send model
                            (SetGameStateReq
                                { playerid = model.playerid
                                , gameState = { gs | winner = NoWinner }
                                }
                            )

                    else
                        Cmd.none
            in
            { model
                | gameState = gs
                , lastTestMode =
                    if isTestMode then
                        Nothing

                    else
                        gs.testMode
            }
                |> withCmd cmd

        EraseBoard ->
            { model
                | gameState =
                    { gameState
                        | moves = []
                        , newBoard = NewBoard.empty
                        , selected = Nothing
                        , legalMoves = Moves []
                    }
            }
                |> withNoCmd

        InitialBoard ->
            { model
                | gameState =
                    { gameState
                        | moves = []
                        , newBoard = NewBoard.initial
                        , selected = Nothing
                        , legalMoves = Moves []
                    }
            }
                |> withNoCmd

        SetTestClear testClear ->
            case gameState.testMode of
                Nothing ->
                    model |> withNoCmd

                Just testMode ->
                    { model
                        | gameState =
                            { gameState
                                | testMode =
                                    Just { testMode | clear = testClear }
                            }
                    }
                        |> withNoCmd

        SetTestColor color ->
            case gameState.testMode of
                Nothing ->
                    model |> withNoCmd

                Just testMode ->
                    let
                        testPiece =
                            testMode.piece
                    in
                    { model
                        | gameState =
                            { gameState
                                | testMode =
                                    Just
                                        { testMode
                                            | piece =
                                                { testPiece | color = color }
                                        }
                            }
                    }
                        |> withNoCmd

        SetTestPieceType pieceString ->
            let
                { pieceType } =
                    ED.stringToPiece pieceString
            in
            case gameState.testMode of
                Nothing ->
                    model |> withNoCmd

                Just testMode ->
                    let
                        testPiece =
                            testMode.piece
                    in
                    { model
                        | gameState =
                            { gameState
                                | testMode =
                                    Just
                                        { testMode
                                            | piece =
                                                { testPiece | pieceType = pieceType }
                                        }
                            }
                    }
                        |> withNoCmd

        MaybeClearStorage ->
            { model
                | reallyClearStorage = True
                , error = Just "Click the \"Clear Storage Now!\" button to clear all storage."
            }
                |> withNoCmd

        ClearStorage ->
            let
                ( mdl, cmd ) =
                    init JE.null "url" model.key
            in
            { mdl | started = True }
                |> withCmds [ clear, cmd, initialNewReqCmd mdl ]

        Click ( row, col ) ->
            if gameState.testMode /= Nothing then
                doTestClick row col model

            else if gameState.winner /= NoWinner || (not <| isPlaying model) then
                model |> withNoCmd

            else
                doClick row col model

        CorruptJumpedUI askYesNo ->
            let
                chooseMoveOptionsUI =
                    model.chooseMoveOptionsUI
            in
            maybeDelayedClick
                { model
                    | chooseMoveOptionsUI =
                        { chooseMoveOptionsUI
                            | corruptJumped = askYesNo
                        }
                }

        MakeHulkUI askYesNo ->
            let
                chooseMoveOptionsUI =
                    model.chooseMoveOptionsUI
            in
            maybeDelayedClick
                { model
                    | chooseMoveOptionsUI =
                        { chooseMoveOptionsUI
                            | makeHulk = askYesNo
                        }
                }

        SendUndoJumps undoWhichJumps ->
            model
                |> withCmd
                    (send model
                        (PlayReq
                            { playerid = model.playerid
                            , placement = ChooseUndoJump undoWhichJumps
                            }
                        )
                    )

        ChatUpdate chatSettings cmd ->
            { model | chatSettings = chatSettings }
                |> withCmds [ cmd, putChat chatSettings ]

        ChatSend line chatSettings ->
            chatSend line chatSettings model

        ChatClear ->
            let
                ( chatSettings, chatCmd ) =
                    clearChatSettings False model.chatSettings
            in
            { model | chatSettings = chatSettings }
                |> withCmd chatCmd

        DelayedAction updater time ->
            updater { model | time = time }

        SetZone zone ->
            let
                chatSettings =
                    model.chatSettings
            in
            { model | chatSettings = { chatSettings | zone = zone } }
                |> withNoCmd

        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    -- For now
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange url ->
            model |> withNoCmd

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    -- Maybe we should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res


clearChatSettings : Bool -> ChatSettings -> ( ChatSettings, Cmd Msg )
clearChatSettings clearInput chatSettings =
    let
        newSettings =
            { chatSettings
                | lines = []
                , input =
                    if clearInput then
                        ""

                    else
                        chatSettings.input
            }
    in
    ( newSettings, putChat newSettings )


chatSend : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSend line chatSettings model =
    model
        |> withCmd (delayedAction <| chatSendInternal line chatSettings)


chatSendInternal : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSendInternal line chatSettings model =
    { model | chatSettings = chatSettings }
        |> withCmd
            (send model <|
                ChatReq
                    { playerid = model.playerid
                    , text = line
                    }
            )


delayedAction : (Model -> ( Model, Cmd Msg )) -> Cmd Msg
delayedAction updater =
    Task.perform (DelayedAction updater) Time.now


makeWebSocketServer : Model -> ServerInterface
makeWebSocketServer model =
    WebSocketFramework.makeServer
        (getCmdPort WebSocket.moduleName ())
        ED.messageEncoder
        model.serverUrl
        Noop


webSocketConnect : ConnectionReason -> Model -> ( Model, Cmd Msg )
webSocketConnect reason model =
    if model.isLocal then
        { model
            | interface =
                if model.interfaceIsProxy then
                    model.interface

                else
                    proxyServer
            , isLive = True
        }
            |> withNoCmd

    else
        { model
            | interface =
                if True then
                    --model.interfaceIsProxy then
                    makeWebSocketServer model

                else
                    model.interface
            , interfaceIsProxy = False
            , connectionReason = Debug.log "webSocketConnect" reason
        }
            |> withCmd
                (WebSocket.makeOpen model.serverUrl
                    |> webSocketSend
                )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    webSocketConnect StartGameConnection model


join : Model -> ( Model, Cmd Msg )
join model =
    webSocketConnect JoinGameConnection model


disconnect : Model -> ( Model, Cmd Msg )
disconnect model =
    { model | isLive = False }
        |> withCmd
            (if model.isLive && not model.isLocal then
                send model <|
                    LeaveReq { playerid = model.playerid }

             else
                Cmd.none
            )


send : Model -> Message -> Cmd Msg
send model message =
    ServerInterface.send model.interface <| Debug.log "send" message


doTestClick : Int -> Int -> Model -> ( Model, Cmd Msg )
doTestClick row col model =
    let
        gameState =
            model.gameState

        board =
            gameState.newBoard
    in
    case gameState.testMode of
        Nothing ->
            model |> withNoCmd

        Just testMode ->
            if testMode.clear then
                { model
                    | gameState =
                        { gameState
                            | newBoard =
                                NewBoard.set (rc row col) Types.emptyPiece board
                        }
                            |> NewBoard.populateLegalMoves
                }
                    |> withNoCmd

            else
                let
                    rowcol =
                        rc row col

                    { pieceType } =
                        NewBoard.get rowcol board

                    gs =
                        if pieceType == NoPiece then
                            { gameState
                                | newBoard =
                                    NewBoard.set rowcol testMode.piece board
                            }

                        else
                            { gameState | selected = Just rowcol }
                in
                { model
                    | gameState = gs |> NewBoard.populateLegalMoves
                }
                    |> withNoCmd


doClick : Int -> Int -> Model -> ( Model, Cmd Msg )
doClick row col model =
    let
        gameState =
            model.gameState

        rowCol =
            rc row col

        board =
            gameState.newBoard

        player =
            gameState.whoseTurn

        otherPlayer =
            Types.otherPlayer player

        { pieceType } =
            NewBoard.get rowCol board

        selected =
            gameState.selected

        ( selectedType, selectedColor ) =
            case selected of
                Nothing ->
                    ( NoPiece, WhiteColor )

                Just selectedRc ->
                    let
                        p =
                            NewBoard.get selectedRc board
                    in
                    ( p.pieceType, p.color )
    in
    if
        (selectedType /= NoPiece)
            && (model.chooseMoveOptionsUI.makeHulk == AskAsk)
    then
        -- Handle click when asking whether to make a hulk.
        let
            chooseMoveOptionsUI =
                model.chooseMoveOptionsUI

            askYesNo =
                if pieceType == NoPiece then
                    AskNo

                else
                    AskYes rowCol
        in
        model |> withCmd (Task.perform MakeHulkUI (Task.succeed askYesNo))

    else if
        (selectedType == NoPiece)
            || (pieceType /= NoPiece)
            || (not model.isLocal && player /= model.player)
    then
        delayedClick rowCol model

    else
        let
            jumped =
                case gameState.legalMoves of
                    NoMoves ->
                        Nothing

                    Moves rowCols ->
                        if List.member rowCol rowCols then
                            Just rowCol

                        else
                            Nothing

                    Jumps sequences ->
                        case LE.find (Interface.isFirstJumpTo rowCol) sequences of
                            Nothing ->
                                Nothing

                            Just sequence ->
                                case sequence of
                                    { over } :: _ ->
                                        Just over

                                    _ ->
                                        Nothing
        in
        case jumped of
            Nothing ->
                delayedClick rowCol model

            Just jumpedRc ->
                let
                    chooseMoveOptionsUI =
                        if
                            (rowCol /= NewBoard.playerSanctum otherPlayer)
                                || ((selectedType /= Golem)
                                        && (selectedType /= Hulk)
                                        && (selectedType /= CorruptedHulk)
                                   )
                        then
                            chooseMoveOptionsUINo

                        else
                            let
                                mapper loc p found =
                                    if
                                        (p.pieceType == Golem)
                                            && (p.color == selectedColor)
                                            && (loc /= rowCol && Just loc /= selected)
                                    then
                                        True

                                    else
                                        found
                            in
                            if NewBoard.mapWholeBoard mapper board False then
                                { chooseMoveOptionsUINo
                                    | makeHulk = AskAsk
                                }

                            else
                                chooseMoveOptionsUINo

                    chooseMoveOptionsUI2 =
                        if rowCol == jumpedRc then
                            -- It's not a jump, nothing more to do
                            chooseMoveOptionsUI

                        else
                            let
                                jumpedType =
                                    NewBoard.get jumpedRc board |> .pieceType
                            in
                            if
                                (jumpedType /= Golem && jumpedType /= Hulk)
                                    || (selectedType /= Journeyman)
                            then
                                chooseMoveOptionsUI

                            else
                                { chooseMoveOptionsUI
                                    | corruptJumped = AskAsk
                                }
                in
                if chooseMoveOptionsUI2 == chooseMoveOptionsUINo then
                    delayedClick rowCol model

                else
                    { model
                        | chooseMoveOptionsUI = chooseMoveOptionsUI2
                        , delayedClick = Just rowCol
                    }
                        |> withNoCmd


maybeDelayedClick : Model -> ( Model, Cmd Msg )
maybeDelayedClick model =
    case model.delayedClick of
        Nothing ->
            model |> withNoCmd

        Just rowCol ->
            let
                { corruptJumped, makeHulk } =
                    model.chooseMoveOptionsUI
            in
            if (corruptJumped == AskAsk) || (makeHulk == AskAsk) then
                model |> withNoCmd

            else
                delayedClick rowCol model


delayedClick : RowCol -> Model -> ( Model, Cmd Msg )
delayedClick rowCol model =
    let
        gameState =
            model.gameState

        withPlayReq playerid placement =
            withCmd <|
                send model
                    (PlayReq
                        { playerid = playerid
                        , placement = placement
                        }
                    )

        withACmd =
            withPlayReq model.playerid <|
                let
                    piece =
                        NewBoard.get rowCol gameState.newBoard
                in
                case piece.pieceType of
                    NoPiece ->
                        let
                            { corruptJumped, makeHulk } =
                                model.chooseMoveOptionsUI

                            chooseMoveOptions =
                                (if corruptJumped == AskYes () then
                                    [ CorruptJumped ]

                                 else
                                    []
                                )
                                    ++ (case makeHulk of
                                            AskYes hulkPos ->
                                                [ MakeHulk hulkPos ]

                                            _ ->
                                                []
                                       )
                        in
                        ChooseMove rowCol chooseMoveOptions

                    _ ->
                        ChoosePiece rowCol
    in
    { model
        | error = Nothing
        , chooseMoveOptionsUI = chooseMoveOptionsUINo
    }
        |> withACmd


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    NewBoard.colToString colidx ++ NewBoard.rowToString rowidx


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , PortFunnels.subscriptions Process model
        , Time.every 900 Tick
        ]


br : Html Msg
br =
    Html.br [] []


boardSize : Model -> Int
boardSize model =
    let
        ( w, h ) =
            model.windowSize
    in
    min (90 * w) (65 * h) // 100


herculanumStyle : Attribute msg
herculanumStyle =
    style "font-family" "Herculanum, sans-serif"


view : Model -> Document Msg
view model =
    let
        bsize =
            boardSize model

        settings =
            model.settings

        renderStyle =
            Types.typeToStyle model.styleType
    in
    { title = "A•G•O•G"
    , body =
        [ if bsize == 0 then
            text ""

          else
            div
                [ style "background" renderStyle.backgroundColor
                , style "color" renderStyle.lineColor
                , style "padding" "5px"
                ]
                [ if settings.hideTitle then
                    text ""

                  else
                    div
                        [ align "center"
                        ]
                        [ h1
                            [ style "margin" "0 0 0.2em 0"
                            , herculanumStyle
                            ]
                            [ text "A•G•O•G" ]
                        , h2
                            [ style "margin" "0 0 0.2em 0"
                            , herculanumStyle
                            ]
                            [ text "A Game of Golems" ]
                        , p [ style "margin" "0" ]
                            [ text "Designed by Christopher St. Clair" ]
                        ]
                , case model.page of
                    MainPage ->
                        mainPage bsize model

                    RulesPage ->
                        rulesPage bsize model

                    InstructionsPage ->
                        instructionsPage bsize model

                    PublicPage ->
                        publicPage bsize model

                    MovesPage ->
                        movesPage bsize model

                    StatisticsPage ->
                        statisticsPage bsize model
                ]
        ]
    }


ids =
    { chatOutput = "chatOutput"
    , chatInput = "chatInput"
    , forName = "forName"
    }


winReasonToDescription : WinReason -> String
winReasonToDescription reason =
    case reason of
        WinByCapture ->
            "by capture"

        WinBySanctum ->
            "by sanctum"

        WinByImmobilization ->
            "by immobilization"

        WinByResignation ->
            "by resignation"


mainPage : Int -> Model -> Html Msg
mainPage bsize model =
    let
        settings =
            model.settings

        gameState =
            model.gameState

        score =
            gameState.score

        count =
            NewBoard.count gameState.newBoard

        { white, black } =
            gameState.players

        currentPlayer =
            if not model.isLocal then
                Just model.player

            else
                Just gameState.whoseTurn

        rotated =
            case model.rotate of
                RotateWhiteDown ->
                    False

                RotatePlayerDown ->
                    currentPlayer == Just BlackPlayer

        { corruptJumped, makeHulk } =
            model.chooseMoveOptionsUI

        ( playing, message, yourTurn ) =
            if not model.isLive then
                ( False
                , "Enter \"Your Name\" and either click \"Start Game\" or enter \"Game ID\" and click \"Join\""
                , True
                )

            else if white == "" || black == "" then
                ( False
                , let
                    waitingFor =
                        if white == "" then
                            "White"

                        else
                            "Black"
                  in
                  "Waiting for " ++ waitingFor ++ " to join"
                , False
                )

            else
                let
                    winString player reason =
                        let
                            rawName =
                                playerName player model

                            name =
                                if model.isLocal || player /= model.player then
                                    rawName

                                else
                                    "You (" ++ rawName ++ ")"
                        in
                        name ++ " won " ++ winReasonToDescription reason ++ "!"
                in
                case gameState.winner of
                    WhiteWinner reason ->
                        ( False, winString WhitePlayer reason, False )

                    BlackWinner reason ->
                        ( False, winString BlackPlayer reason, False )

                    NoWinner ->
                        let
                            ( prefixp, action, yourTurn2 ) =
                                if corruptJumped == AskAsk || makeHulk == AskAsk then
                                    ( True
                                    , "follow the instructions in the orange-outlined box below"
                                    , True
                                    )

                                else if
                                    not model.isLocal
                                        && (gameState.whoseTurn /= model.player)
                                then
                                    let
                                        otherName =
                                            if currentPlayer == Just BlackPlayer then
                                                white

                                            else
                                                black
                                    in
                                    ( False
                                    , "Waiting for " ++ otherName ++ " to move"
                                    , False
                                    )

                                else
                                    ( True
                                    , case gameState.selected of
                                        Nothing ->
                                            if gameState.jumperLocations == [] then
                                                "choose a piece to move"

                                            else
                                                "choose one of the selected jumper pieces"

                                        Just _ ->
                                            case gameState.legalMoves of
                                                NoMoves ->
                                                    "selected piece has no legal moves."

                                                Moves _ ->
                                                    "click on a highlighted square to move the selected piece"

                                                Jumps _ ->
                                                    "click on a highlighted square to jump"
                                    , True
                                    )

                            prefix =
                                if prefixp then
                                    let
                                        name =
                                            if currentPlayer == Just WhitePlayer then
                                                white

                                            else
                                                black
                                    in
                                    name ++ ", "

                                else
                                    ""
                        in
                        ( True, prefix ++ action ++ ".", yourTurn2 )

        theStyle =
            Types.typeToStyle model.styleType
    in
    div [ align "center" ]
        [ Lazy.lazy7 NewBoard.render
            theStyle
            bsize
            Click
            (Just <| NewBoard.getSizer DefaultSizer)
            currentPlayer
            rotated
            gameState
        , span
            []
            [ br
            , case model.error of
                Nothing ->
                    if model.otherDecoration == NoDecoration then
                        text ""

                    else
                        span [ style "color" "red" ]
                            [ text <|
                                playerName (Types.otherPlayer model.player)
                                    model
                                    ++ " made a choice"
                            , br
                            ]

                Just err ->
                    span [ style "color" "red" ]
                        [ text err
                        , br
                        ]
            , span
                [ style "color"
                    (if not yourTurn && (not playing || gameState.winner == NoWinner) then
                        "green"

                     else
                        "orange"
                    )
                , style "font-weight"
                    (if gameState.winner == NoWinner then
                        "normal"

                     else
                        "bold"
                    )
                , style "font-size" <|
                    if yourTurn then
                        "150%"

                    else
                        "100%"
                ]
                [ text message ]
            , br
            , if gameState.winner /= NoWinner then
                text ""

              else
                span []
                    [ b "Whose turn: "
                    , let
                        ( name, color ) =
                            case gameState.whoseTurn of
                                WhitePlayer ->
                                    ( gameState.players.white, "white" )

                                BlackPlayer ->
                                    ( gameState.players.black, "black" )

                        label =
                            if model.isLocal then
                                color

                            else if model.player == gameState.whoseTurn then
                                "You (" ++ name ++ "), " ++ color

                            else
                                name ++ ", " ++ color
                      in
                      text label
                    , br
                    ]
            , if corruptJumped == AskAsk || makeHulk == AskAsk then
                div
                    [ style "border" <| "3px solid orange"
                    , style "padding" "5px"
                    , style "width" "fit-content"
                    ]
                    [ if corruptJumped == AskAsk then
                        span []
                            [ b "Corrupt Jumped Piece: "
                            , button [ onClick <| CorruptJumpedUI (AskYes ()) ]
                                [ text "Yes" ]
                            , button [ onClick <| CorruptJumpedUI AskNo ]
                                [ text "No" ]
                            , br
                            ]

                      else
                        text ""
                    , if makeHulk == AskAsk then
                        span []
                            [ b "Click a Golem to make it a Hulk, Click an empty square to not make a hulk."
                            , br
                            ]

                      else
                        text ""
                    ]

              else
                text ""
            , if not model.isLocal && model.isLive then
                span []
                    [ if white == "" || black == "" then
                        text ""

                      else
                        let
                            chatSettings =
                                model.chatSettings
                                    |> updateChatAttributes bsize model.styleType
                        in
                        span []
                            [ ElmChat.styledInputBox [ id ids.chatInput ]
                                []
                                --width in chars
                                40
                                --id
                                "Send"
                                ChatSend
                                chatSettings
                            , text " "
                            , button [ onClick ChatClear ]
                                [ text "Clear" ]
                            , ElmChat.chat chatSettings
                            ]
                    , b "White: "
                    , text <|
                        case white of
                            "" ->
                                ""

                            _ ->
                                if model.player == WhitePlayer then
                                    "You (" ++ white ++ ")"

                                else
                                    white
                    , br
                    , b "Black: "
                    , text <|
                        case black of
                            "" ->
                                ""

                            _ ->
                                if model.player == BlackPlayer then
                                    "You (" ++ black ++ ")"

                                else
                                    black
                    ]

              else
                text ""
            , let
                undoLen =
                    List.length gameState.undoStates
              in
              if undoLen == 0 then
                text ""

              else
                span []
                    [ br
                    , button [ onClick <| SendUndoJumps UndoOneJump ]
                        [ text "Undo Jump" ]
                    , if undoLen <= 1 then
                        text ""

                      else
                        span []
                            [ text " "
                            , button [ onClick <| SendUndoJumps UndoAllJumps ]
                                [ text "Undo All Jumps" ]
                            ]
                    ]
            , br
            , a
                [ href "#"
                , onClick <| SetPage MovesPage
                ]
                [ b "Moves" ]
            , b ": "
            , text <| moveString gameState.moves
            , let
                { games, whiteWins, blackWins } =
                    gameState.score

                yourWins =
                    model.yourWins

                player =
                    model.player

                otherPlayer =
                    Types.otherPlayer player
              in
              if games == 0 then
                text ""

              else
                let
                    ( yourWinString, otherWinString ) =
                        if model.isLocal then
                            ( "White won " ++ String.fromInt whiteWins
                            , "Black won " ++ String.fromInt blackWins
                            )

                        else
                            ( "You ("
                                ++ playerName player model
                                ++ ") won "
                                ++ String.fromInt yourWins
                            , playerName otherPlayer model
                                ++ " won "
                                ++ String.fromInt (games - yourWins)
                            )
                in
                span []
                    [ br
                    , text <|
                        String.fromInt games
                            ++ (if games == 1 then
                                    "game, "

                                else
                                    " games, "
                               )
                    , text yourWinString
                    , text ", "
                    , text otherWinString
                    , text " "
                    , if not model.isLocal then
                        text ""

                      else
                        button [ onClick ResetScore ]
                            [ text "Reset" ]
                    ]
            , br
            , br
            , b "Rotate: "
            , radio "rotate"
                "white down"
                (model.rotate == RotateWhiteDown)
                False
                (SetRotate RotateWhiteDown)
            , text " "
            , radio "rotate"
                "player down"
                (model.rotate == RotatePlayerDown)
                False
                (SetRotate RotatePlayerDown)
            , br
            , b "Local: "
            , input
                [ type_ "checkbox"
                , checked model.isLocal
                , onCheck SetIsLocal
                , disabled <| not model.isLocal && model.isLive
                ]
                []
            , text " "
            , button
                [ onClick NewGame
                , disabled (not <| isPlaying model)
                ]
                [ text <|
                    if gameState.winner == NoWinner then
                        "Resign"

                    else
                        "New Game"
                ]
            , if not (model.isLocal || WhichServer.isLocal) then
                text ""

              else
                div [ align "center" ]
                    [ b "Test Mode: "
                    , input
                        [ type_ "checkbox"
                        , checked <| gameState.testMode /= Nothing
                        , onCheck SetTestMode
                        ]
                        []
                    , case gameState.testMode of
                        Nothing ->
                            text ""

                        Just testMode ->
                            let
                                testPiece =
                                    testMode.piece

                                pieceString =
                                    ED.pieceToString testPiece
                                        |> String.toUpper

                                testColor =
                                    testPiece.color

                                testClear =
                                    testMode.clear
                            in
                            div [ align "center" ]
                                [ button [ onClick EraseBoard ]
                                    [ text "Erase Board!" ]
                                , text " "
                                , button [ onClick InitialBoard ]
                                    [ text "Initial Setup" ]
                                , br
                                , b "Remove clicked: "
                                , input
                                    [ type_ "checkbox"
                                    , checked <| testMode.clear
                                    , onCheck SetTestClear
                                    ]
                                    []
                                , if testMode.clear then
                                    text ""

                                  else
                                    span []
                                        [ br
                                        , b "Test piece: "
                                        , select [ onInput SetTestPieceType ]
                                            [ option
                                                [ value "G"
                                                , selected <| pieceString == "G"
                                                ]
                                                [ text "Golem" ]
                                            , option
                                                [ value "H"
                                                , selected <| pieceString == "H"
                                                ]
                                                [ text "Hulk" ]
                                            , option
                                                [ value "C"
                                                , selected <| pieceString == "C"
                                                ]
                                                [ text "Corrupted Hulk" ]
                                            , option
                                                [ value "J"
                                                , selected <| pieceString == "J"
                                                ]
                                                [ text "Journeyman" ]
                                            ]
                                        , text " "
                                        , radio "testColor"
                                            "white"
                                            (testColor == WhiteColor)
                                            False
                                            (SetTestColor WhiteColor)
                                        , text " "
                                        , radio "testColor"
                                            "black"
                                            (testColor == BlackColor)
                                            False
                                            (SetTestColor BlackColor)
                                        ]
                                ]
                    ]
            , if model.isLocal then
                text ""

              else
                div [ align "center" ]
                    [ if model.isLive then
                        div [ align "center" ]
                            [ b "Game ID: "
                            , text model.gameid
                            , br
                            , button
                                [ onClick Disconnect ]
                                [ text "Disconnect" ]
                            ]

                      else
                        div [ align "center" ]
                            [ b "Your Name: "
                            , input
                                [ onInput SetName
                                , value settings.name
                                , size 20
                                ]
                                []
                            , br

                            {-
                               , b "Server: "
                               , input
                                   [ onInput SetServerUrl
                                   , value model.serverUrl
                                   , size 40
                                   , disabled True
                                   ]
                                   []
                               , text " "
                            -}
                            , b "Public: "
                            , input
                                [ type_ "checkbox"
                                , checked settings.isPublic
                                , onCheck SetIsPublic
                                ]
                                []
                            , if not settings.isPublic then
                                text ""

                              else
                                span []
                                    [ b " for name: "
                                    , input
                                        [ onInput SetForName
                                        , value settings.forName
                                        , size 20
                                        , id ids.forName
                                        ]
                                        []
                                    ]
                            , text " "
                            , button
                                [ onClick StartGame
                                , disabled <| settings.name == ""
                                ]
                                [ text "Start Game" ]
                            , br
                            , b "Game ID: "
                            , input
                                [ onInput SetGameid
                                , value model.gameid
                                , size 16
                                , onKeydown
                                    (\code ->
                                        if code == 13 then
                                            Join

                                        else
                                            Noop
                                    )
                                ]
                                []
                            , text " "
                            , button
                                [ onClick Join
                                , disabled <|
                                    (settings.name == "")
                                        || (model.gameid == "")
                                ]
                                [ text "Join"
                                ]
                            ]
                    ]
            ]
        , p []
            [ b " Dark Mode: "
            , input
                [ type_ "checkbox"
                , checked <| model.styleType == DarkStyle
                , onCheck SetDarkMode
                ]
                []
            ]
        , footerParagraph
        , let
            really =
                model.reallyClearStorage

            ( msg, label ) =
                if really then
                    ( ClearStorage, "Clear Storage Now!" )

                else
                    ( MaybeClearStorage, "Clear Storage!" )
          in
          p []
            [ button
                [ onClick msg
                , title "Clear Local Storage. Cannot be undone!"
                ]
                [ text label ]
            ]
        ]


moveString : List OneMove -> String
moveString moves =
    let
        len =
            4

        ellipsis =
            if List.length moves > len then
                ", ..."

            else
                ""

        head =
            List.take 4 moves
    in
    movesToString head ++ ellipsis


footerParagraph : Html Msg
footerParagraph =
    p []
        [ a
            [ href "#"
            , onClick <| SetPage PublicPage
            ]
            [ text "Public" ]
        , text " "
        , a
            [ href "#"
            , onClick <| SetPage InstructionsPage
            ]
            [ text "Instructions" ]
        , text " "
        , a
            [ href "#"
            , onClick <| SetPage RulesPage
            ]
            [ text "Rules" ]
        , text " "
        , a
            [ href "#"
            , onClick <| SetPage StatisticsPage
            ]
            [ text "Statistics" ]
        , br
        , a
            [ href "https://github.com/billstclair/agog/"
            , target "_blank"
            ]
            [ text "GitHub" ]
        , text " "
        , a
            [ href "https://gibgoygames.com/"
            , target "_blank"
            ]
            [ text "Gib Goy Games" ]
        , br
        , text <| chars.copyright ++ " 2019-2021 Bill St. Clair <"
        , a [ href "mailto:GibGoyGames@gmail.com" ]
            [ text "GibGoyGames@gmail.com" ]
        , text ">"
        , br
        , a
            [ href "https://github.com/billstclair/agog/blob/main/LICENSE"
            , target "_blank"
            ]
            [ text "MIT License" ]
        ]


pairup : List String -> List ( String, String )
pairup strings =
    let
        loop list res =
            case list of
                [] ->
                    List.reverse res

                [ x ] ->
                    List.reverse <| ( x, "" ) :: res

                x :: (y :: tail) ->
                    loop tail (( x, y ) :: res)
    in
    loop strings []


movesToString : List OneMove -> String
movesToString moves =
    List.map ED.oneMoveToString moves
        |> String.join ", "


pairToString : ( String, String ) -> String
pairToString ( s1, s2 ) =
    if s2 == "" then
        s1

    else
        "(" ++ s1 ++ "," ++ chars.nbsp ++ s2 ++ ")"



-- For testing. No longer used.


radio : String -> String -> Bool -> Bool -> msg -> Html msg
radio group name isChecked isDisabled msg =
    label []
        [ input
            [ type_ "radio"
            , Attributes.name group
            , onClick msg
            , checked isChecked
            , disabled isDisabled
            ]
            []
        , text name
        ]


rulesDiv : Bool -> List (Html Msg) -> Html Msg
rulesDiv =
    Documentation.rulesDiv


playButton : Html Msg
playButton =
    Documentation.playButtonHtml <| SetPage MainPage


instructionsPage : Int -> Model -> Html Msg
instructionsPage bsize model =
    Documentation.instructions (SetPage MainPage) <| Just footerParagraph


rulesPage : Int -> Model -> Html Msg
rulesPage bsize model =
    Documentation.rules (SetPage MainPage) <| Just footerParagraph


th : String -> Html Msg
th string =
    Html.th [] [ text string ]


publicPage : Int -> Model -> Html Msg
publicPage bsize model =
    let
        settings =
            model.settings

        name =
            settings.name
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Public Games" ]
            , playButton
            , p [ align "center" ]
                [ b "Your Name: "
                , input
                    [ onInput SetName
                    , value name
                    , size 20
                    ]
                    []
                , if name /= "" then
                    text ""

                  else
                    span [ style "color" "red" ]
                        [ br
                        , text "To join a game, you must enter a name."
                        ]
                ]
            , p [ align "center" ]
                [ if model.isLive then
                    p [ style "color" "red" ]
                        [ text "You're playing a game. What are you doing here?" ]

                  else
                    text ""
                ]
            , table
                [ class "prettytable"
                , style "color" "black"
                ]
              <|
                List.concat
                    [ [ tr []
                            [ th "GameId"
                            , th "Creator"
                            , th "Player"
                            , th "For you"
                            ]
                      ]
                    , List.map
                        (renderPublicGameRow model.gameid name model.isLive)
                        model.publicGames
                    ]
            , playButton
            , footerParagraph
            ]
        ]


renderPublicGameRow : String -> String -> Bool -> PublicGame -> Html Msg
renderPublicGameRow myGameid name connected { gameid, creator, player, forName } =
    let
        center =
            style "text-align" "center"
    in
    tr []
        [ td [ center ]
            [ if gameid == myGameid || connected || name == "" then
                text gameid

              else
                a
                    [ href "#"
                    , onClick <| JoinGame gameid
                    ]
                    [ text gameid ]
            ]
        , td [ center ]
            [ if gameid == myGameid then
                text <| "You (" ++ creator ++ ")"

              else
                text creator
            ]
        , td [ center ] [ text <| playerString player ]
        , td [ center ]
            [ if myGameid == gameid then
                text <| Maybe.withDefault "" forName

              else
                input
                    [ type_ "checkbox"
                    , checked <| name /= "" && Interface.forNameMatches name forName
                    , disabled True
                    ]
                    []
            ]
        ]


alignCenterStyle : Html.Attribute msg
alignCenterStyle =
    style "text-align" "center"


movesPage : Int -> Model -> Html Msg
movesPage bsize model =
    let
        settings =
            model.settings

        name =
            settings.name

        gameState =
            model.gameState

        moves =
            gameState.moves

        { white, black } =
            gameState.players

        pairList : List a -> List (List a)
        pairList list =
            let
                mapper a ( evenp, res ) =
                    if evenp then
                        ( False, [ a ] :: res )

                    else
                        case res of
                            [ a1 ] :: rest ->
                                ( True, [ a1, a ] :: rest )

                            _ ->
                                -- Can't happen
                                ( True, res )
            in
            List.foldl mapper ( True, [] ) list
                |> Tuple.second
                |> List.reverse
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Moves" ]
            , p [] [ playButton ]
            , if model.isLocal then
                text ""

              else
                p []
                    [ b "White: "
                    , text white
                    , text ", "
                    , text "Black: "
                    , text black
                    ]
            , table
                [ class "prettytable"
                , style "color" "black"
                ]
              <|
                [ tr []
                    [ th chars.nbsp
                    , th "White"
                    , th "Black"
                    ]
                ]
                    ++ List.indexedMap movesRow (List.reverse moves |> pairList)
                    ++ (let
                            ( winString, isResign ) =
                                case List.head moves of
                                    Nothing ->
                                        ( "", False )

                                    Just { winner } ->
                                        let
                                            winDescription color reason =
                                                (color ++ " won by ")
                                                    ++ winReasonToDescription reason
                                        in
                                        case winner of
                                            NoWinner ->
                                                ( "", False )

                                            WhiteWinner reason ->
                                                ( winDescription "White" reason
                                                , reason == WinByResignation
                                                )

                                            BlackWinner reason ->
                                                ( winDescription "Black" reason
                                                , reason == WinByResignation
                                                )
                        in
                        if winString == "" then
                            [ text "" ]

                        else
                            [ tr []
                                [ td
                                    [ colspan 3
                                    , alignCenterStyle
                                    ]
                                    [ text winString ]
                                ]
                            ]
                       )
            , p [] [ playButton ]
            , footerParagraph
            ]
        ]


isEven : Int -> Bool
isEven x =
    x == x // 2 * 2


movesRow : Int -> List OneMove -> Html Msg
movesRow index moves =
    let
        ( maybeWhite, maybeBlack ) =
            case List.take 2 moves of
                [ white ] ->
                    ( Just white, Nothing )

                [ white, black ] ->
                    ( Just white, Just black )

                _ ->
                    ( Nothing, Nothing )
    in
    tr []
        [ td [ alignCenterStyle ] [ text (String.fromInt <| index + 1) ]
        , td [ alignCenterStyle ]
            [ case maybeWhite of
                Nothing ->
                    text chars.nbsp

                Just white ->
                    text <| ED.oneMoveToPrettyString white
            ]
        , td [ alignCenterStyle ]
            [ case maybeBlack of
                Nothing ->
                    text chars.nbsp

                Just black ->
                    text <| ED.oneMoveToPrettyString black
            ]
        ]


playerString : Player -> String
playerString player =
    case player of
        WhitePlayer ->
            "White"

        BlackPlayer ->
            "Black"


statisticsPage : Int -> Model -> Html Msg
statisticsPage bsize model =
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Statistics" ]
            , p [] [ playButton ]
            , if model.isLocal then
                p [] [ text "There are no live updates in local mode." ]

              else
                text ""
            , case model.statistics of
                Nothing ->
                    p []
                        [ text "There are no statistics." ]

                Just statistics ->
                    let
                        ( startTime, updateTime ) =
                            model.statisticsTimes

                        uptime =
                            case startTime of
                                Nothing ->
                                    0

                                Just time ->
                                    Time.posixToMillis model.tick - time
                    in
                    span []
                        [ table
                            [ class "prettytable"
                            , style "color" "black"
                            ]
                          <|
                            [ tr []
                                [ th "Statistic"
                                , th "Count"
                                ]
                            ]
                                ++ List.map (statisticsRow statistics) Types.statisticsKeyOrder
                        , case Dict.get statisticsKeys.finishedGames statistics of
                            Nothing ->
                                text ""

                            Just finishedGames ->
                                span []
                                    [ case Dict.get statisticsKeys.whiteWon statistics of
                                        Nothing ->
                                            text ""

                                        Just whiteWon ->
                                            span []
                                                [ b "White wins: "
                                                , text <| String.fromInt (whiteWon * 100 // finishedGames)
                                                , text "%"
                                                , br
                                                ]
                                    , case Dict.get statisticsKeys.totalMoves statistics of
                                        Nothing ->
                                            text ""

                                        Just totalMoves ->
                                            span []
                                                [ b "Average moves/game: "
                                                , text <| String.fromInt (totalMoves // finishedGames)
                                                , br
                                                ]
                                    ]
                        , case updateTime of
                            Nothing ->
                                text ""

                            Just time ->
                                span []
                                    [ b "Last update time: "
                                    , text <|
                                        DateFormat.Relative.relativeTime
                                            model.tick
                                            (Time.millisToPosix <|
                                                min time
                                                    (Time.posixToMillis model.tick)
                                            )
                                    , br
                                    ]
                        , case startTime of
                            Nothing ->
                                text ""

                            Just time ->
                                span []
                                    [ b "Server uptime: "
                                    , text <|
                                        uptimeString model.tick
                                            (Time.millisToPosix time)
                                    , br
                                    ]
                        ]
            , p [] [ playButton ]
            , footerParagraph
            ]
        ]


monthNumber : Time.Month -> Int
monthNumber month =
    let
        months =
            [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
    in
    case LE.elemIndex month months of
        Nothing ->
            0

        Just idx ->
            idx


uptimeString : Posix -> Posix -> String
uptimeString now start =
    let
        delta =
            (Time.posixToMillis now - Time.posixToMillis start) // 1000

        days =
            delta // (3600 * 24)

        deltaMinusDays =
            delta - days * 3600 * 24

        hours =
            deltaMinusDays // 3600

        deltaMinusHours =
            deltaMinusDays - hours * 3600

        minutes =
            deltaMinusHours // 60

        showDays =
            days /= 0

        showHours =
            showDays || hours /= 0

        dayString =
            if showDays then
                String.fromInt days ++ plural days " day, " " days, "

            else
                ""

        hourString =
            if showHours then
                String.fromInt hours ++ plural hours " hour, " " hours, "

            else
                ""

        minuteString =
            String.fromInt minutes ++ plural minutes " minute" " minutes"

        plural num one notone =
            if num == 1 then
                one

            else
                notone
    in
    dayString ++ hourString ++ minuteString


statisticsRow : Statistics -> (StatisticsKeys -> String) -> Html Msg
statisticsRow statistics keystring =
    let
        property =
            keystring Types.statisticsKeys
    in
    case Dict.get property statistics of
        Nothing ->
            text ""

        Just value ->
            tr []
                [ td [] [ text property ]
                , td [] [ text <| String.fromInt value ]
                ]


b : String -> Html msg
b s =
    Html.b [] [ text s ]


codestr : Int -> String
codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }



---
--- Persistence
---


putModel : Model -> Cmd Msg
putModel model =
    let
        savedModel =
            modelToSavedModel model

        value =
            ED.encodeSavedModel savedModel

        playerid =
            model.playerid
    in
    put pk.model <| Just value


putChat : ChatSettings -> Cmd Msg
putChat settings =
    ElmChat.settingsEncoder settings
        |> Just
        |> put pk.chat


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put key value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get key)


clear : Cmd Msg
clear =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "AGOG"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , chat = "chat"
    }
