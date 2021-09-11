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

import Agog.Board as Board exposing (SizerKind(..))
import Agog.EncodeDecode as ED
import Agog.Interface as Interface
import Agog.NewBoard as NewBoard exposing (rc)
import Agog.Types as Types
    exposing
        ( Board
        , Choice(..)
        , ChooseMoveOption(..)
        , Color(..)
        , Decoration(..)
        , GameState
        , Message(..)
        , MovesOrJumps(..)
        , NewBoard
        , Page(..)
        , PieceType(..)
        , Player(..)
        , PlayerNames
        , PublicGame
        , PublicType(..)
        , RowCol
        , SavedModel
        , Score
        , Settings
        , Style
        , StyleType(..)
        , TestMode
        , UndoWhichJumps(..)
        , Winner(..)
        )
import Agog.WhichServer as WhichServer
import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
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
import Time exposing (Posix, Zone)
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
        )


type alias SimulatorState =
    { gameCount : Int
    , gameCountString : String
    , gamesLeft : Int
    , simulatorResult : SimulatorResult
    }


type alias SimulatorResult =
    { horizontalWins : Int
    , verticalWins : Int
    , horizontalScore : Int
    , verticalScore : Int
    }


type alias ServerInterface =
    WebSocketFramework.Types.ServerInterface GameState Player Message Msg


type ConnectionReason
    = NoConnection
    | StartGameConnection
    | JoinGameConnection
    | PublicGamesConnection
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
    { serverUrl : String
    , interface : ServerInterface
    , connectionReason : ConnectionReason
    , funnelState : State
    , otherPlayerid : PlayerId
    , key : Key
    , windowSize : ( Int, Int )
    , started : Bool --True when persistent storage is available
    , simulatorState : SimulatorState --for the "Aux"  page
    , seed : Seed
    , error : Maybe String
    , chatSettings : ChatSettings
    , publicGames : List PublicGame
    , time : Posix
    , requestedNew : Bool
    , chooseMoveOptionsUI : ChooseMoveOptionsUI
    , delayedClick : Maybe RowCol

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
    | IncomingMessage ServerInterface Message
    | SetDecoration Decoration
    | SetChooseFirst Player
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
    | ClearStorage
    | Click ( Int, Int )
    | CorruptJumpedUI (AskYesNo ())
    | MakeHulkUI (AskYesNo RowCol)
    | CancelChooseMoveOptionsUI
    | SendUndoJumps UndoWhichJumps
    | ChatUpdate ChatSettings (Cmd Msg)
    | ChatSend String ChatSettings
    | ChatClear
    | DelayedAction (Model -> ( Model, Cmd Msg )) Posix
    | SetZone Zone
    | SetGameCount String
    | ToggleSimulator
    | SimulatorStep
    | InitializeSeed Posix
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


{-| No longer used. An initial state to test Board.render
-}
initializeBoard : Board -> Board
initializeBoard board =
    board
        |> Board.set 0 3
        |> Board.set 1 1
        |> Board.set 1 2
        |> Board.set 2 2
        |> Board.set 3 2
        |> Board.set 3 1
        |> Board.set 3 3
        |> Board.set 3 4
        |> Board.set 2 4
        |> Board.set 4 4
        |> Board.set 4 5


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoderWithPrivate
    , decoder = ED.messageDecoder
    , errorWrapper = Nothing
    }


fullProcessor : ServerMessageProcessor GameState Player Message
fullProcessor =
    ServerInterface.fullMessageProcessor encodeDecode Interface.messageProcessor


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
            { serverUrl = WhichServer.serverUrl
            , interface = proxyServer
            , connectionReason = NoConnection
            , funnelState = initialFunnelState
            , otherPlayerid = ""
            , key = key
            , windowSize = ( 0, 0 )
            , started = False
            , simulatorState =
                { gameCount = 1000
                , gameCountString = "1000"
                , gamesLeft = 0
                , simulatorResult = SimulatorResult 0 0 0 0
                }
            , seed = Random.initialSeed 0 --get time for this
            , error = Nothing
            , chatSettings = initialChatSettings (Types.typeToStyle LightStyle)
            , publicGames = []
            , time = Time.millisToPosix 0
            , requestedNew = False
            , chooseMoveOptionsUI = chooseMoveOptionsUINo
            , delayedClick = Nothing
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
            , isLocal = True
            , gameid = ""
            , playerid = ""
            , isLive = False
            , settings = Types.emptySettings
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , Task.perform InitializeSeed Time.now
            , Task.perform SetZone Time.here
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
                        { settings | id = ids.chatOutput }
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
            { mdl | interface = interface }
    in
    case Debug.log "incomingMessage" message of
        NewRsp { gameid, playerid, player, name, gameState } ->
            { model
                | gameid = gameid
                , playerid = playerid
                , player = player
                , isLive = True
                , connectionReason = JoinGameConnection
                , gameState = gameState
                , decoration = gameState.private.decoration
            }
                |> withCmd
                    (if not model.isLocal then
                        Cmd.none

                     else if player == WhitePlayer then
                        send model <|
                            JoinReq { gameid = gameid, name = "Black" }

                     else
                        send model <|
                            JoinReq { gameid = gameid, name = "White" }
                    )

        JoinRsp { gameid, playerid, player, gameState } ->
            let
                chatSettings =
                    model.chatSettings

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
                        , chatSettings =
                            { chatSettings
                                | lines = []
                                , input = ""
                            }
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
            model3 |> withCmd (setPage MainPage)

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

        ErrorRsp { request, text } ->
            let
                model2 =
                    { model
                        | error = Just text
                        , connectionReason = NoConnection
                        , publicGames = []
                    }

                model3 =
                    if model.isLocal then
                        model2

                    else
                        { model2
                            | isLive = False
                            , playerid = ""
                            , gameid = ""
                        }
            in
            model3
                |> withCmds
                    [ setPage MainPage
                    , if
                        not model2.isLocal
                            && (model.connectionReason /= NoConnection)
                      then
                        Cmd.none
                        --WebSocket.makeClose model2.serverUrl
                        --  |> webSocketSend

                      else
                        Cmd.none
                    ]

        ChatRsp { gameid, name, text } ->
            let
                ( chatSettings, cmd ) =
                    ElmChat.addLineSpec model.chatSettings <|
                        ElmChat.makeLineSpec text
                            (Just name)
                            --(Just model.time)
                            Nothing
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
                    cmd == Cmd.none

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

        IncomingMessage interface message ->
            incomingMessage interface message model

        SetDecoration decoration ->
            { model | decoration = decoration }
                |> withNoCmd

        SetChooseFirst player ->
            { model | chooseFirst = player }
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
                unsubscribe =
                    if page /= PublicPage || model.page == PublicPage then
                        send model <|
                            PublicGamesReq
                                { subscribe = False
                                , forName = ""
                                , gameid = Nothing
                                }

                    else
                        Cmd.none

                ( mdl, cmd ) =
                    { model | page = page }
                        |> webSocketConnect PublicGamesConnection
            in
            mdl |> withCmds [ cmd, unsubscribe ]

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
                        | testMode =
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
                                , gameState = gs
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
                        | newBoard = NewBoard.empty
                        , selected = Nothing
                        , legalMoves = Moves []
                    }
            }
                |> withNoCmd

        InitialBoard ->
            { model
                | gameState =
                    { gameState
                        | newBoard = NewBoard.initial
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

        CancelChooseMoveOptionsUI ->
            maybeDelayedClick
                { model
                    | chooseMoveOptionsUI = chooseMoveOptionsUINo
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
                |> withCmd (putChat chatSettings)

        ChatSend line chatSettings ->
            chatSend line chatSettings model

        ChatClear ->
            let
                chatSettings =
                    model.chatSettings

                newSettings =
                    { chatSettings | lines = [] }
            in
            { model | chatSettings = newSettings }
                |> withCmd (putChat newSettings)

        DelayedAction updater time ->
            updater { model | time = time }

        SetZone zone ->
            let
                chatSettings =
                    model.chatSettings
            in
            { model | chatSettings = { chatSettings | zone = zone } }
                |> withNoCmd

        SetGameCount string ->
            let
                simulatorState =
                    model.simulatorState
            in
            { model
                | simulatorState =
                    { simulatorState
                        | gameCount =
                            String.toInt string
                                |> Maybe.withDefault simulatorState.gameCount
                    }
            }
                |> withNoCmd

        ToggleSimulator ->
            let
                simulatorState =
                    model.simulatorState

                gamesLeft =
                    if simulatorState.gamesLeft == 0 then
                        simulatorState.gameCount

                    else
                        0
            in
            { model
                | simulatorState =
                    { simulatorState
                        | gamesLeft = gamesLeft
                        , simulatorResult = SimulatorResult 0 0 0 0
                    }
            }
                |> withCmd
                    (if gamesLeft > 0 then
                        simulatorStepCmd ()

                     else
                        Cmd.none
                    )

        SimulatorStep ->
            let
                simulatorState =
                    model.simulatorState

                count =
                    simulatorState.gamesLeft

                loop seed left c1 c2 s1 s2 =
                    if left <= 0 then
                        ( SimulatorResult c1 c2 s1 s2, seed )

                    else
                        let
                            ( winner, score, seed2 ) =
                                Board.simulateGame seed

                            ( ( c12, c22 ), ( s12, s22 ) ) =
                                case winner of
                                    WhiteWinner ->
                                        ( ( c1 + 1, c2 ), ( s1 + score, s2 ) )

                                    BlackWinner ->
                                        ( ( c1, c2 + 1 ), ( s1, s2 + score ) )

                                    _ ->
                                        ( ( c1, c2 ), ( s2, s2 ) )
                        in
                        loop seed2 (left - 1) c12 c22 s12 s22

                gamesLeft =
                    simulatorState.gamesLeft - count

                oldResult =
                    simulatorState.simulatorResult

                ( newResult, newSeed ) =
                    loop model.seed count 0 0 0 0

                simulatorResult =
                    { horizontalWins =
                        oldResult.horizontalWins + newResult.horizontalWins
                    , verticalWins =
                        oldResult.verticalWins + newResult.verticalWins
                    , horizontalScore =
                        oldResult.horizontalScore + newResult.horizontalScore
                    , verticalScore =
                        oldResult.verticalScore + newResult.verticalScore
                    }
            in
            { model
                | simulatorState =
                    { simulatorState
                        | simulatorResult = simulatorResult
                        , gamesLeft = gamesLeft
                    }
                , seed = newSeed
            }
                |> withCmd
                    (if gamesLeft > 0 then
                        simulatorStepCmd ()

                     else
                        Cmd.none
                    )

        InitializeSeed posix ->
            { model
                | seed = Random.initialSeed <| Time.posixToMillis posix
            }
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
            | interface = proxyServer
            , isLive = True
        }
            |> withNoCmd

    else
        { model
            | interface = makeWebSocketServer model
            , connectionReason = reason
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


simulatorStepCmd : () -> Cmd Msg
simulatorStepCmd x =
    Task.perform (\_ -> SimulatorStep) <| Task.succeed x


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

        selectedType =
            case gameState.selected of
                Nothing ->
                    NoPiece

                Just selectedRc ->
                    NewBoard.get selectedRc board |> .pieceType
    in
    if
        (selectedType /= NoPiece)
            && (model.chooseMoveOptionsUI.makeHulk == AskAsk)
    then
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
                                case List.tail sequence of
                                    Just _ ->
                                        Nothing

                                    Nothing ->
                                        case List.head sequence of
                                            Nothing ->
                                                Nothing

                                            Just { over } ->
                                                Just over
        in
        case jumped of
            Nothing ->
                delayedClick rowCol model

            Just jumpedRc ->
                let
                    chooseMoveOptionsUI =
                        if
                            (rowCol /= NewBoard.playerSanctum otherPlayer)
                                || (selectedType /= Golem && selectedType /= Hulk)
                        then
                            chooseMoveOptionsUINo

                        else
                            { chooseMoveOptionsUINo
                                | makeHulk = AskAsk
                            }

                    chooseMoveOptionsUI2 =
                        if rowCol == jumpedRc then
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
    Board.colToString colidx ++ Board.rowToString rowidx


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , PortFunnels.subscriptions Process model
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
                ]
        ]
    }


ids =
    { chatOutput = "chatOutput"
    , chatInput = "chatInput"
    , forName = "forName"
    }


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
            Board.count gameState.board

        { white, black } =
            gameState.players

        currentPlayer =
            if not model.isLocal then
                Just model.player

            else
                Just gameState.whoseTurn

        rotated =
            currentPlayer == Just WhitePlayer

        ( playing, message ) =
            if not model.isLive then
                ( False
                , "Enter \"Your Name\" and either click \"Start Game\" or enter \"Game ID\" and click \"Join\""
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
                )

            else
                ( True
                , let
                    winString player =
                        let
                            rawName =
                                playerName player model

                            name =
                                if model.isLocal || player /= model.player then
                                    rawName

                                else
                                    "You (" ++ rawName ++ ")"
                        in
                        name ++ " won!"
                  in
                  case gameState.winner of
                    WhiteWinner ->
                        winString WhitePlayer

                    BlackWinner ->
                        winString BlackPlayer

                    NoWinner ->
                        let
                            action =
                                case gameState.selected of
                                    Nothing ->
                                        if gameState.jumperLocations == [] then
                                            "choose a piece to move"

                                        else
                                            "choose one of the selected jumper pieces"

                                    Just _ ->
                                        case gameState.legalMoves of
                                            Moves [] ->
                                                "selected piece has no legal moves."

                                            Moves _ ->
                                                "click on a highlighted square to move the selected piece"

                                            Jumps _ ->
                                                "click on a highlighted square to jump"

                            name =
                                if currentPlayer == Just WhitePlayer then
                                    white

                                else
                                    black
                        in
                        name ++ ", " ++ action ++ "."
                )

        { corruptJumped, makeHulk } =
            model.chooseMoveOptionsUI

        theStyle =
            Types.typeToStyle model.styleType
    in
    div [ align "center" ]
        [ NewBoard.render theStyle
            bsize
            Click
            (Just <| Board.getSizer DefaultSizer)
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
                    (if not playing || gameState.winner == NoWinner then
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
                ]
                [ text message ]
            , br
            , b " Dark Mode: "
            , input
                [ type_ "checkbox"
                , checked <| model.styleType == DarkStyle
                , onCheck SetDarkMode
                ]
                []
            , br
            , if gameState.winner /= NoWinner then
                text ""

              else
                span []
                    [ b "Whose turn: "
                    , text <|
                        case gameState.whoseTurn of
                            WhitePlayer ->
                                gameState.players.white

                            BlackPlayer ->
                                gameState.players.black
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
                            [ b "Click a Golem to make it a Hulk. Otherwise, any empty square."
                            , br
                            ]

                      else
                        text ""
                    , button [ onClick CancelChooseMoveOptionsUI ]
                        [ text "No Hulks or Corruption" ]
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
                            , br
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
            , if gameState.score == Dict.empty then
                text ""

              else
                span []
                    [ br
                    , b "Games/Score for"
                    , span [] <|
                        let
                            yourName =
                                playerName model.player model
                        in
                        List.foldl
                            (\( name, oneScore ) html ->
                                let
                                    nameString =
                                        if not model.isLocal && name == yourName then
                                            "You (" ++ name ++ ")"

                                        else
                                            name
                                in
                                List.concat
                                    [ html
                                    , [ span []
                                            [ if html == [] then
                                                text " "

                                              else
                                                text ", "
                                            , text nameString
                                            , text ": "
                                            , text <| String.fromInt oneScore.games
                                            , text "/"
                                            , text <| String.fromInt oneScore.score
                                            ]
                                      ]
                                    ]
                            )
                            []
                            (Dict.toList gameState.score)
                    , text " "
                    , if not model.isLocal then
                        text ""

                      else
                        button [ onClick ResetScore ]
                            [ text "Reset" ]
                    ]
            , br
            , b "Local: "
            , input
                [ type_ "checkbox"
                , checked model.isLocal
                , onCheck SetIsLocal
                , disabled True -- <| not model.isLocal && model.isLive
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
            , if model.isLocal then
                div [ align "center" ]
                    [ text "Test Mode: "
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
                                , text "Remove clicked: "
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
                                        , text "Test piece: "
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
            [ text "Moves: "
            , text <| movesToString gameState.moves
            ]
        , p []
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
            ]
        , p []
            [ button
                [ onClick ClearStorage
                , title "Clear Local Storage. Cannot be undone!"
                ]
                [ text <| "Clear!" ]
            ]
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


movesToString : List String -> String
movesToString moves =
    pairup moves
        |> List.map pairToString
        |> List.intersperse ", "
        |> String.concat


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
rulesDiv alignCenter body =
    div
        [ style "width" "25em"
        , style "max-width" "95%"
        , if alignCenter then
            align "center"

          else
            style "margin" "auto"
        ]
        body


playButton : Html Msg
playButton =
    rulesDiv True
        [ button
            [ onClick <| SetPage MainPage
            , style "font-size" "110%"
            ]
            [ text "Play" ]
        ]


instructionsPage : Int -> Model -> Html Msg
instructionsPage bsize model =
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Instructions" ]
        , rulesDiv False
            [ Markdown.toHtml [] """
TO BE DONE
"""
            ]
        , playButton
        ]


rulesPage : Int -> Model -> Html Msg
rulesPage bsize model =
    rulesDiv False
        [ br
        , playButton
        , rulesDiv False
            [ h2 [ align "center" ]
                [ text "Story" ]
            , Markdown.toHtml [] """ 
_In a secret complex deep in the mountains, the Ancient Guild of Golemancy plies its trade, creating life where before was only base matter. Two journeyman golemancers feverishly prepare their master works, for there is but one position available among the Golemancery Elect. Both know that if they could but obtain their rival’s research, they would surely be able to create the greater work. The night before their creations are to be judged, each plots to infiltrate the other’s sanctum of knowledge, using their legion of early prototypes to subtly outmaneuver the other._

_In __A•G•O•G__, players attempt to be first reach their opponent’s sanctum, or failing that, to take their opponent captive and in so doing keep them from completing their masterpiece. If one of your golems manages to enter their sanctum, they may bring you back some sundry inventions to better equip your remaining creations, but your eyes alone will know what to look for within their volumes of research. Will you have what it takes to outwit your rival and take your place among the Masters of Golemancy?_
"""
            ]
        , playButton
        , rulesDiv False
            [ h2 [ align "center" ]
                [ text "SETUP" ]
            , Markdown.toHtml [] """
__A•G•O•G__ is a game of subtle strategy for two players. Played on a standard 8x8 checkers board, players sit at opposite dark-colored corner squares. The square closest to each player is their “sanctum.” One player’s units are white and the other’s are black.

Each player starts with 20 golems, shaped like checkers pieces, and 1 journeyman golemancer (journeyman for short), comprised of a stack of 3 checkers: white-black-white for White; black-white-black for Black. The journeyman starts on the player’s sanctum with the golems filling the 5 rows of squares in front of them, leaving the three middle rows unoccupied between the two players’ ranks.

White goes first. On their turn, the acting player must move one unit or make a capture with one unit. Players alternate turns until one of them is victorious.
"""
            ]
        , playButton
        , rulesDiv False
            [ h2 [ align "center" ]
                [ text "UNITS" ]
            , Markdown.toHtml [] """
All unit movement in __A•G•O•G__ is orthogonal relative to the squares; diagonal relative to the players. Units can only move forward (towards the enemy sanctum), but can capture enemy units forward or backward.

Units capture by jumping over an enemy unit and landing on an unoccupied square beyond them. Units cannot jump over more than one enemy at a time, and cannot move or jump off the edge of the board. If a unit making a capture lands on a square from which they can make another capture, they are compelled to do so and continue the capturing sequence until they land on a square from which no further captures are available. Captured units are immediately removed from play. All jumped units remain on the board until the sequence is complete, and no unit can be jumped twice in the sequence.

If a capture is available on the acting player’s turn, they are compelled to take it instead of moving a unit, and if multiple captures are available, they are compelled to take the capturing sequence that will capture the most enemy units. For example, if the acting player could either capture two enemy golems or capture the enemy journeyman alone, they are compelled to capture the two golems even though capturing the journeyman would win them the game.

__The Golem:__ The basic unit of __A•G•O•G__. Golems can move forward one square at a time and can capture only adjacent enemy units, landing on the square immediately beyond them.

__The Hulk:__ If a golem ends the turn placed on the enemy sanctum, its player may choose to either to place it atop any of their other golems or remove it from play. If the player chooses the former, the two stacked golems become a new unit: the hulk. If a hulk ends the turn placed on the enemy sanctum, its player may either create a new hulk with the top golem of the stack and remove the bottom from play, or may remove both stacked golems from play. If they have no other golems in play when one of their units ends the turn on the enemy sanctum, that unit must be removed from play.

Augmented with stolen inventions from the enemy sanctum, the hulk is the most powerful unit in the game. They can move forward any distance in a straight line, and can capture at any distance in a straight line, landing on any unoccupied square beyond the captured unit (provided they do do not jump over more than one unit at a time and that they land such that they can capture the most possible enemy units in the sequence).

__The Journeyman:__ The most important unit. If captured, their player loses the game, whereas if a journeyman ends the turn on the enemy sanctum, their player wins the game.

The journeymen move and capture the same as golems. However, when jumping over an enemy unit, they may “corrupt” the jumped unit instead of capturing it. To create a corrupted unit, the acting player places one of their golems that has been removed from play atop the jumped unit. This unit is now under the control of the player that corrupted it and becomes a “corrupted hulk”. If an enemy hulk is jumped by the acting player’s journeyman, it can be corrupted by removing the top stacked enemy golem and replacing it with one of the corrupting player’s that has been removed from play. If the corrupting player does not have any golems that have been removed from play, they cannot corrupt units and must capture them instead.

Corrupted hulks behave the same as regular hulks of their color, with the following exceptions:

1. A corrupted hulk cannot be corrupted again and must be captured if jumped by the enemy journeyman.
2. If a corrupted hulk ends the turn on the enemy sanctum, the acting player may either place the top stacked golem of their color atop another of their golems, creating a standard hulk, and remove the bottom stacked enemy golem from play, or they may remove both stacked golems from play. 
"""
            ]
        , playButton
        , rulesDiv False
            [ h2 [ align "center" ]
                [ text "WINNING" ]
            , Markdown.toHtml [] """
A player wins immediately under any of the following conditions:

1. Win by Capture: The player captures the enemy journeyman.
2. Win by Sanctum: The player’s journeyman ends the turn on the enemy sanctum.
3. Win by Immobilization: The player’s opponent has no legal moves on their turn.
4. Win by Resignation: The player’s opponent resigns.

Draws are not possible in __A•G•O•G__.
"""
            ]
        , playButton
        , rulesDiv False
            [ h2 [ align "center" ]
                [ text "SCOREKEEPING" ]
            , Markdown.toHtml [] """
If desired, players may keep a game record while playing. Each pair of turns for White and Black is denoted with a numbered move, with White’s turn recorded on the left and Black’s on the right. The board is labelled with coordinates the same as a chess board, with the squares to White’s right labelled alphabetically "a" through "h" and the squares to White’s left labelled numerically "1" through "8". The units and actions are labelled as follows:

* The journeyman is denoted as "J".
* Standard hulks are denoted as "H".
* Corrupted hulks are denoted as "C".
* Golems require no unit label, and their movement is indicated by the coordinates alone.

When a golem moves, record the row or column from which they moved and the coordinates of the square they moved to. For example, a golem moving from a6 to b6 would be recorded as "ab6". When another unit moves, record the unit and the coordinates of the square they moved to. It is not necessary to record the row or column from which they moved unless two or more of the same unit could have moved to that square. For example, if corrupted hulk is on a6 and another is on b5, the corrupted hulk moving from b5 to b6 would be recorded as "C5b6".

When a unit makes a capture, record "x" between the unit label/starting row or column and the coordinates of the square they moved to. To record a journeyman creating a corrupted hulk, record "X" instead (capital "x"). For example, a journeyman making a capturing sequence from a1 to a3 to c3, capturing the first unit and creating a corrupted hulk from the second, would be recorded as "Jxa3Xc3".

If White wins, record "1-0". If Black wins, record "0-1".
"""
            ]
        , playButton
        , rulesDiv False
            [ h2 [ align "center" ]
                [ text "HANDICAPPING" ]
            , Markdown.toHtml [] """
If players of differing skill levels wish to play an even game, the stronger player can be given a handicap by removing 1 or more of their golems from the board before play starts. The handicapped player always plays Black, and the golems must be removed such that the remaining forces are symmetrically arranged and are removed from the squares closest to the center of the board. For example, for a handicap of 1 remove the golem on f6; for a handicap of 2, remove the golems on e6 and f5; for a handicap of 3 remove the golems on e6, f5, and f6, etc. If the weaker player wins the majority of their games at a particular handicap, reduce the handicap by 1.
"""
            ]
        , playButton
        ]


th : String -> Html Msg
th string =
    Html.th [] [ text string ]


publicPage : Int -> Model -> Html Msg
publicPage bsize model =
    let
        settings =
            model.settings
    in
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Public Games" ]
        , p [ align "center" ]
            [ if isPlaying model then
                p [ style "color" "red" ]
                    [ text "You're playing a game. What are you doing here?" ]

              else
                text ""
            , table [ class "prettytable" ] <|
                List.concat
                    [ [ tr []
                            [ th "GameId"
                            , th "Creator"
                            , th "Player"
                            , th "For you"
                            ]
                      ]
                    , List.map
                        (renderPublicGameRow model.gameid
                            settings.name
                            (isPlaying model)
                        )
                        model.publicGames
                    ]
            ]
        , playButton
        ]


renderPublicGameRow : String -> String -> Bool -> PublicGame -> Html Msg
renderPublicGameRow myGameid name playing { gameid, creator, player, forName } =
    let
        center =
            style "text-align" "center"
    in
    tr []
        [ td [ center ]
            [ if gameid == myGameid || playing then
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
                    , checked <| Interface.forNameMatches name forName
                    ]
                    []
            ]
        ]


playerString : Player -> String
playerString player =
    case player of
        WhitePlayer ->
            "White"

        BlackPlayer ->
            "Black"


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
