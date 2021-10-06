---------------------------------------------------------------------
--
-- Main.elm
-- AGOG top-level
-- Copyright (c) 2019-2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------
---
--- TODO:
---
--- Recover from client when server had forgotten the game.
--- Eventually want cryptographic security here, so that someone who
--- grabbed the gameid from the public board can't snarf a game.
--- Mix gameid with secret and give to only the real players.
--- Or mint a new gameid when the second player joins,
--- so that the gameid is no longer public.
--- That is addable at a later date.
---
--- After the recovery code works, errors from the server during gameplay
--- may be unfound playerid, because the server was restarted.
--- ErrorRsp needs to encode this as other than "unknown playerid" in the
--- text. Instead of the encoded request, it would be good to have an actual
--- error field. This could also be solved by persisting the gamedicts on
--- the server, but I'd rather not.
---
--- Zooming the screen should make the board bigger, but not fit.
--- This means that sizing should depend on screen size, not browser
--- window size. In any case, we need some way to be able to choose
--- pieces on a small screen.
---
----------------------------------------------------------------------


port module Main exposing (main)

import Agog.Board as Board exposing (rc)
import Agog.Documentation as Documentation
import Agog.EncodeDecode as ED
import Agog.Interface as Interface
import Agog.Types as Types
    exposing
        ( Board
        , Choice(..)
        , ChooseMoveOption(..)
        , Color(..)
        , GameState
        , Message(..)
        , MovesOrJumps(..)
        , NamedGame
        , OneMove
        , OneMoveSequence(..)
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
        , ServerState
        , Settings
        , StatisticsKeys
        , Style
        , StyleType(..)
        , TestMode
        , TestModeInitialState
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
import DateFormat
import DateFormat.Relative
import Dict exposing (Dict)
import Dict.Extra as DE
import ElmChat exposing (LineSpec(..), defaultExtraAttributes)
import Fifo exposing (Fifo)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , audio
        , blockquote
        , button
        , div
        , embed
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
        , source
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
        , autoplay
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
import PortFunnel.LocalStorage as LocalStorage exposing (Label)
import PortFunnel.Notification as Notification exposing (Permission(..))
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


port onVisibilityChange : (Bool -> msg) -> Sub msg


port playAudio : String -> Cmd msg


type alias ServerInterface =
    Types.ServerInterface Msg


type ConnectionReason
    = StartGameConnection
    | JoinGameConnection GameId
    | PublicGamesConnection
    | StatisticsConnection
    | UpdateConnection PlayerId
    | RestoreGameConnection Game
    | JoinRestoredGameConnection GameId


type alias ConnectionSpec =
    { gamename : GameName
    , connectionReason : ConnectionReason
    }


type alias ChatSettings =
    Types.ChatSettings Msg


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


type alias Game =
    NamedGame Msg


type alias Model =
    { tick : Posix
    , zone : Zone
    , game : Game
    , gameDict : Dict String Game
    , chatDict : Dict String ChatSettings
    , connectionSpecQueue : Fifo ConnectionSpec
    , funnelState : State
    , key : Key
    , windowSize : ( Int, Int )
    , started : Bool --True when persistent storage is available
    , error : Maybe String
    , publicGames : List PublicGame
    , time : Posix
    , requestedNew : Bool
    , chooseMoveOptionsUI : ChooseMoveOptionsUI
    , delayedClick : Maybe RowCol
    , reallyClearStorage : Bool
    , statistics : Maybe Statistics
    , statisticsTimes : ( Maybe Int, Maybe Int )
    , notificationAvailable : Maybe Bool
    , notificationPermission : Maybe Permission
    , visible : Bool
    , soundFile : Maybe String
    , showArchive : Maybe ( Game, Int )
    , showMove : Maybe ( Game, Int )

    -- persistent below here
    , gamename : String
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


isPlaying : Model -> Bool
isPlaying model =
    let
        game =
            model.game

        { white, black } =
            game.gameState.players
    in
    game.isLive && white /= "" && black /= ""


type alias GameName =
    String


type Msg
    = Noop
    | Tick Posix
    | IncomingMessage ServerInterface Message
    | SetChooseFirst Player
    | SetRotate RotateBoard
    | SetIsLocal Bool
    | SetDarkMode Bool
    | SetName String
    | SetIsPublic Bool
    | SetForName String
    | SetServerUrl String
    | SetGameid String
    | SetGameName GameName
    | SwitchGame GameName
    | RenameGame
    | SetPage Page
    | SetHideTitle Bool
    | NewGame
    | StartGame
    | Join
    | JoinGame GameId
    | Disconnect
    | SetTestMode Bool
    | SetNotificationsEnabled Bool
    | SetSoundEnabled Bool
    | EraseBoard
    | RevertBoard
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
    | ChatUpdate String ChatSettings (Cmd Msg)
    | ChatSend String ChatSettings
    | ChatClear
    | PlaySound String
    | DelayedAction (Model -> ( Model, Cmd Msg )) Posix
    | SetZone Zone
    | WindowResize Int Int
    | VisibilityChange Bool
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | DoConnectedResponse
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


updateServerState : (ServerState -> ServerState) -> ServerInterface -> ServerInterface
updateServerState updater serverInterface =
    let
        (ServerInterface interface) =
            serverInterface
    in
    case interface.state of
        Nothing ->
            let
                state =
                    WebSocketFramework.Types.emptyServerState Nothing
            in
            ServerInterface
                { interface
                    | state = Just <| updater state
                }

        Just state ->
            ServerInterface
                { interface
                    | state = Just <| updater state
                }


updateServerTime : Posix -> ServerInterface -> ServerInterface
updateServerTime posix serverInterface =
    serverInterface
        |> updateServerState (\state -> { state | time = posix })


updateServerSeed : Maybe Seed -> ServerInterface -> ServerInterface
updateServerSeed maybeSeed serverInterface =
    case maybeSeed of
        Nothing ->
            serverInterface

        Just seed ->
            serverInterface
                |> updateServerState (\state -> { state | seed = seed })


proxyServer : Maybe Seed -> ServerInterface
proxyServer seed =
    ServerInterface.makeProxyServer fullProcessor IncomingMessage
        |> updateServerSeed seed


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


initialChatSettings : String -> Zone -> ChatSettings
initialChatSettings gamename zone =
    let
        settings =
            ElmChat.makeSettings ids.chatOutput 14 True (ChatUpdate gamename)
    in
    { settings | zone = zone }


initialGame : Maybe Seed -> Game
initialGame seed =
    { gamename = Types.defaultGamename
    , gameid = ""
    , gameState = Interface.emptyGameState Types.emptyPlayerNames
    , isLocal = False
    , serverUrl = WhichServer.serverUrl
    , otherPlayerid = ""
    , player = WhitePlayer
    , playerid = ""
    , isLive = False
    , yourWins = 0
    , archives = []

    -- not persistent
    , interfaceIsProxy = True
    , interface = proxyServer seed
    }


insertConnectionSpec : ConnectionSpec -> Model -> Model
insertConnectionSpec spec model =
    { model
        | connectionSpecQueue =
            Fifo.insert spec model.connectionSpecQueue
    }


removeConnectionSpec : Model -> ( Maybe ConnectionSpec, Model )
removeConnectionSpec model =
    let
        ( spec, queue ) =
            Fifo.remove model.connectionSpecQueue
    in
    ( spec, { model | connectionSpecQueue = queue } )


isConnectionSpecQueueEmpty : Model -> Bool
isConnectionSpecQueueEmpty model =
    model.connectionSpecQueue == Fifo.empty


zeroTick : Posix
zeroTick =
    Time.millisToPosix 0


makeSeed : Posix -> Seed
makeSeed posix =
    Random.initialSeed (Time.posixToMillis posix)


modelSeed : Model -> Maybe Seed
modelSeed model =
    if model.tick == zeroTick then
        Nothing

    else
        Just <| makeSeed model.tick


init : Value -> url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        game =
            initialGame Nothing

        model =
            { tick = zeroTick
            , zone = Time.utc
            , game = game
            , gameDict = Dict.empty
            , chatDict =
                [ ( game.gamename, initialChatSettings game.gamename Time.utc ) ]
                    |> Dict.fromList
            , connectionSpecQueue = Fifo.empty
            , funnelState = initialFunnelState
            , key = key
            , windowSize = ( 0, 0 )
            , started = False
            , error = Nothing
            , publicGames = []
            , time = Time.millisToPosix 0
            , requestedNew = False
            , chooseMoveOptionsUI = chooseMoveOptionsUINo
            , delayedClick = Nothing
            , reallyClearStorage = False
            , statistics = Nothing
            , statisticsTimes = ( Nothing, Nothing )
            , notificationAvailable = Nothing
            , notificationPermission = Nothing
            , visible = True
            , soundFile = Nothing
            , showArchive = Nothing
            , showMove = Nothing
            , styleType = LightStyle
            , lastTestMode = Nothing

            -- persistent fields
            , gamename = game.gamename
            , page = MainPage
            , chooseFirst = WhitePlayer
            , gameid = ""
            , settings = Types.emptySettings
            , rotate = RotateWhiteDown
            , notificationsEnabled = False
            , soundEnabled = False
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
    , gamename : String
    , restoreState : Maybe GameState
    , maybeGameid : Maybe GameId
    }


initialNewReqBody : String -> NewReqBody
initialNewReqBody gamename =
    { name = "White"
    , player = WhitePlayer
    , publicType = NotPublic
    , gamename = gamename
    , restoreState = Nothing
    , maybeGameid = Nothing
    }


initialNewReq : String -> Message
initialNewReq gamename =
    NewReq <| initialNewReqBody gamename


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
            if mdl.started && not model.started && model.tick /= zeroTick then
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys mdl

        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    handleGetResponse label key v model

        _ ->
            mdl |> withCmd cmd


handleListKeysResponse : Label -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse label prefix keys model =
    case label of
        Nothing ->
            model |> withNoCmd

        Just lab ->
            if lab == pk.game then
                let
                    getter key cmd =
                        Cmd.batch
                            [ cmd
                            , getLabeled pk.game key
                            ]
                in
                ( model, List.foldr getter Cmd.none <| Debug.log "Getting games" keys )

            else
                model |> withNoCmd


handleGetResponse : Label -> String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse label key value model =
    case label of
        Just lab ->
            if lab == pk.game then
                handleGetGameResponse key value model

            else if lab == pk.chat then
                handleGetChatResponse key value model

            else
                model |> withNoCmd

        Nothing ->
            if key == pk.model then
                let
                    cmd =
                        listKeysLabeled pk.game gamePrefix
                in
                case ED.decodeSavedModel value of
                    Err e ->
                        model |> withCmd cmd

                    Ok savedModel ->
                        savedModelToModel savedModel model
                            |> withCmd cmd

            else
                model |> withNoCmd


foldrGames : (Game -> a -> a) -> a -> Model -> a
foldrGames folder a model =
    (( model.game.gamename, model.game )
        :: Dict.toList model.gameDict
    )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> List.foldr folder a


updateAllGames : (Game -> Maybe Game) -> Model -> Model
updateAllGames updater model =
    let
        model2 =
            case updater model.game of
                Nothing ->
                    model

                Just game2 ->
                    { model | game = game2 }

        ( dictChanged, gameDict ) =
            let
                folder name game ( changed, dict ) =
                    case updater game of
                        Nothing ->
                            ( changed, dict )

                        Just game2 ->
                            ( True, Dict.insert name game2 dict )
            in
            Dict.foldl folder ( False, model.gameDict ) model.gameDict
    in
    if not dictChanged then
        model2

    else
        { model2 | gameDict = gameDict }


lookupGame : GameName -> Model -> Maybe Game
lookupGame gamename model =
    if gamename == model.game.gamename then
        Just model.game

    else
        Dict.get gamename model.gameDict


updateGame : String -> (Game -> Game) -> Model -> ( Model, Maybe Game )
updateGame gamename updater model =
    if gamename == model.game.gamename then
        let
            game =
                updater model.game
        in
        ( { model | game = game }
        , Just game
        )

    else
        case Dict.get gamename model.gameDict of
            Nothing ->
                let
                    name =
                        Debug.log "updateGame, game not found" gamename
                in
                ( model, Nothing )

            Just game ->
                let
                    game2 =
                        updater game
                in
                ( { model
                    | gameDict =
                        Dict.insert gamename game2 model.gameDict
                  }
                , Just game2
                )


handleGetGameResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetGameResponse key value model =
    case Debug.log "handleGetGameResponse, gamename" <| gameKeyToName key of
        Nothing ->
            { model
                | error = Just <| "Unrecognized game key: " ++ key
            }
                |> withNoCmd

        Just gamename ->
            case
                JD.decodeValue (ED.namedGameDecoder <| proxyServer (modelSeed model))
                    value
            of
                Err _ ->
                    { model
                        | error = Just <| "Couldn't decode game: " ++ gamename
                    }
                        |> withNoCmd

                Ok game ->
                    if game.gamename /= gamename then
                        { model
                            | error =
                                Just <|
                                    "Bug: Loaded "
                                        ++ gamename
                                        ++ " from localStorage. Got "
                                        ++ game.gamename
                        }
                            |> withNoCmd

                    else
                        let
                            model2 =
                                if gamename == model.game.gamename then
                                    { model | game = game }

                                else
                                    { model
                                        | gameDict =
                                            Dict.insert gamename game model.gameDict
                                    }

                            model3 =
                                -- Empty chat in case it's not in localStorage
                                { model2
                                    | chatDict =
                                        Dict.insert gamename
                                            (initialChatSettings gamename model2.zone)
                                            model2.chatDict
                                }

                            getChatCmd =
                                getChat gamename

                            ( model4, cmd4 ) =
                                reconnectToGame game model3
                        in
                        model4 |> withCmds [ getChatCmd, cmd4 ]


reconnectToGame : Game -> Model -> ( Model, Cmd Msg )
reconnectToGame game model =
    let
        gamename =
            game.gamename
    in
    if not game.isLocal && game.isLive && game.playerid /= "" then
        model
            |> webSocketConnect
                game
                (ConnectionSpec gamename <| UpdateConnection game.playerid)

    else if game.isLocal then
        model
            |> withCmd (initialNewReqCmd game model)

    else if gamename == model.game.gamename then
        if model.page == PublicPage then
            { model | gameid = "" }
                |> webSocketConnect
                    game
                    (ConnectionSpec gamename PublicGamesConnection)

        else if model.page == StatisticsPage then
            { model | gameid = "" }
                |> webSocketConnect
                    game
                    (ConnectionSpec gamename StatisticsConnection)

        else
            model |> withNoCmd

    else
        model |> withNoCmd


updateChat : String -> Model -> (ChatSettings -> ChatSettings) -> ( Model, Maybe ChatSettings )
updateChat gamename model updater =
    case Dict.get gamename model.chatDict of
        Nothing ->
            ( model, Nothing )

        Just chat ->
            let
                chat2 =
                    updater chat
            in
            ( { model
                | chatDict =
                    Dict.insert gamename chat2 model.chatDict
              }
            , Just chat2
            )


updateChat2 : String -> Model -> (ChatSettings -> ( ChatSettings, a )) -> ( Model, Maybe ( ChatSettings, a ) )
updateChat2 gamename model updater =
    case Dict.get gamename model.chatDict of
        Nothing ->
            ( model, Nothing )

        Just chat ->
            let
                ( chat2, a ) =
                    updater chat
            in
            ( { model
                | chatDict =
                    Dict.insert gamename chat2 model.chatDict
              }
            , Just ( chat2, a )
            )


handleGetChatResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetChatResponse key value model =
    case chatKeyToName key of
        Nothing ->
            { model
                | error = Just <| "Unrecognized chat key: " ++ key
            }
                |> withNoCmd

        Just gamename ->
            case JD.decodeValue (ElmChat.settingsDecoder (ChatUpdate gamename)) value of
                Err _ ->
                    { model
                        | error = Just <| "Couldn't decode chat: " ++ gamename
                    }
                        |> withNoCmd

                Ok settings ->
                    let
                        ( model2, maybeChat ) =
                            updateChat gamename
                                model
                                (\chat ->
                                    { settings
                                        | id = ids.chatOutput
                                        , zone = chat.zone
                                    }
                                )
                    in
                    case maybeChat of
                        Nothing ->
                            { model2
                                | error = Debug.log "" (Just <| "Unfound chat for game: " ++ gamename)
                            }
                                |> withNoCmd

                        Just chat2 ->
                            model2
                                |> withCmd (ElmChat.restoreScroll chat2)


initialNewReqCmd : Game -> Model -> Cmd Msg
initialNewReqCmd game model =
    send game.interface <|
        let
            req =
                initialNewReqBody game.gamename
        in
        NewReq
            { req
                | restoreState =
                    Just game.gameState
            }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { gamename = model.gamename
    , gameGamename = model.game.gamename
    , page = model.page
    , chooseFirst = model.chooseFirst
    , lastTestMode = model.lastTestMode
    , gameid = model.gameid
    , settings = model.settings
    , styleType = model.styleType
    , rotate = model.rotate
    , notificationsEnabled = model.notificationsEnabled
    , soundEnabled = model.soundEnabled
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    let
        game =
            model.game
    in
    { model
        | gamename = savedModel.gamename
        , game = { game | gamename = savedModel.gameGamename }
        , page = savedModel.page
        , chooseFirst = savedModel.chooseFirst
        , lastTestMode = savedModel.lastTestMode
        , gameid = savedModel.gameid
        , settings = savedModel.settings
        , styleType = savedModel.styleType
        , rotate = savedModel.rotate
        , notificationsEnabled = savedModel.notificationsEnabled
        , soundEnabled = savedModel.soundEnabled
    }


playerName : Player -> Game -> String
playerName player game =
    let
        players =
            game.gameState.players
    in
    case player of
        WhitePlayer ->
            players.white

        BlackPlayer ->
            players.black


findGame : (Game -> Bool) -> Model -> Maybe Game
findGame predicate model =
    if predicate model.game then
        Just model.game

    else
        case
            DE.find (\_ game -> predicate game) model.gameDict
        of
            Nothing ->
                Nothing

            Just ( _, game ) ->
                Just game


gameFromPlayerId : PlayerId -> Model -> Maybe Game
gameFromPlayerId playerid model =
    findGame (.playerid >> (==) playerid) model


gameFromId : GameId -> Model -> Maybe Game
gameFromId gameid model =
    findGame (.gameid >> (==) gameid) model


withGameFromId : String -> Model -> (Game -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withGameFromId gameid model thunk =
    case gameFromId gameid model of
        Nothing ->
            model |> withNoCmd

        Just game ->
            thunk game


incomingMessage : ServerInterface -> Message -> Model -> ( Model, Cmd Msg )
incomingMessage interface message mdl =
    let
        messageLog =
            Debug.log "incomingMessage" <| ED.messageToLogMessage message

        model =
            { mdl | reallyClearStorage = False }

        ( maybeGame, ( model2, cmd2 ) ) =
            case Types.messageToGameid message of
                Nothing ->
                    incomingMessageInternal interface Nothing message model

                Just gameid ->
                    let
                        maybeGame2 =
                            case gameFromId gameid model of
                                Nothing ->
                                    Nothing

                                Just game ->
                                    Just { game | interface = interface }
                    in
                    incomingMessageInternal interface maybeGame2 message model
    in
    case maybeGame of
        Nothing ->
            model2 |> withCmd cmd2

        Just game ->
            let
                ( model3, maybeGame2 ) =
                    updateGame game.gamename (always game) model2
            in
            model3
                |> withCmds [ cmd2, putGame game ]


{-| Do the work for `incomingMessage`.

If `maybeGame` is not not `Nothing`, then a game with its `gameid` was found.
If the `Maybe Game` in the result is not `Nothing`, then the `Game` was changed,
and needs to be persisted by `incomingMessage`. Otherwise, it was NOT changed.

It is expected that the game's `gamename` is NOT changed. That can be done from the UI, but only `RenameGame` knows how to do what is necessary.

-}
incomingMessageInternal : ServerInterface -> Maybe Game -> Message -> Model -> ( Maybe Game, ( Model, Cmd Msg ) )
incomingMessageInternal interface maybeGame message model =
    let
        withRequiredGame gameid thunk =
            case maybeGame of
                Just game ->
                    thunk game

                Nothing ->
                    ( Nothing
                    , { model
                        | error =
                            Just <| "Bug: there is no game for gameid: " ++ gameid
                      }
                        |> withNoCmd
                    )
    in
    case message of
        NewRsp { gameid, playerid, player, name, gamename, gameState, wasRestored } ->
            if maybeGame /= Nothing && not wasRestored then
                case maybeGame of
                    Nothing ->
                        -- Can't happen
                        ( Nothing, model |> withNoCmd )

                    Just game ->
                        let
                            returnedGame =
                                if
                                    (model.game.gamename == game.gamename)
                                        && game.isLocal
                                then
                                    -- Otherwise, there's no way out in the UI.
                                    Just { game | isLive = False }

                                else
                                    Nothing
                        in
                        ( returnedGame
                        , { model
                            | error =
                                -- This is actually remotely possible, if the
                                -- remote server happens to randomly generate a
                                -- gameid that matches a local one (or vice-versa).
                                -- TODO: make isLocal an arg to gameFromId
                                Just <| "Bug: NewRsp found existing gameid: " ++ gameid
                          }
                            |> withNoCmd
                        )

            else
                case lookupGame gamename model of
                    Nothing ->
                        ( Nothing
                        , { model
                            | error =
                                Just <| "Can't find game named \"" ++ gamename ++ "\""
                          }
                            |> withNoCmd
                        )

                    Just game ->
                        let
                            ( model2, chatCmd ) =
                                if not wasRestored then
                                    clearChatSettings game.gamename True model

                                else
                                    model |> withNoCmd

                            game2 =
                                { game
                                    | gameid = gameid
                                    , gameState = gameState
                                    , player = player
                                    , playerid = playerid
                                    , isLive = True
                                    , yourWins = 0
                                    , interface = interface
                                }

                            model3 =
                                if game.gamename == model.game.gamename then
                                    { model2 | gameid = gameid }

                                else
                                    model2
                        in
                        ( Just game2
                        , model3
                            |> withCmds
                                [ chatCmd
                                , if not game.isLocal then
                                    Cmd.none

                                  else if player == WhitePlayer then
                                    send interface <|
                                        JoinReq
                                            { gameid = gameid
                                            , name = "Black"
                                            , isRestore = False
                                            }

                                  else
                                    send interface <|
                                        JoinReq
                                            { gameid = gameid
                                            , name = "White"
                                            , isRestore = False
                                            }
                                ]
                        )

        JoinRsp { gameid, playerid, player, gameState, wasRestored } ->
            case maybeGame of
                Nothing ->
                    ( Nothing
                    , { model
                        | error =
                            Just <| "JoinRsp found no game for id: " ++ gameid
                      }
                        |> withNoCmd
                    )

                Just game ->
                    let
                        ( model2, chatCmd ) =
                            if not wasRestored then
                                clearChatSettings game.gamename True model

                            else
                                model |> withNoCmd

                        game2 =
                            { game
                                | gameid = gameid
                                , gameState = gameState
                                , isLive = True
                                , player =
                                    if playerid == Nothing then
                                        game.player

                                    else
                                        player
                                , yourWins = 0
                                , interface = interface
                            }

                        game3 =
                            if game2.isLocal then
                                { game2
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
                                        game2

                                    Just pid ->
                                        { game2 | playerid = pid }

                        ( model3, _ ) =
                            updateGame game3.gamename (always game3) model2

                        msg =
                            if game3.gamename == model.gamename then
                                "The game is on!"

                            else
                                playerName player game3
                                    ++ " joined hidden game "
                                    ++ game3.gamename
                    in
                    ( Just game3
                    , model3
                        |> withCmds
                            [ chatCmd
                            , setPage MainPage
                            , maybeSendNotification game3 False msg model2
                            ]
                    )

        LeaveRsp { gameid, player } ->
            withRequiredGame gameid
                (\game ->
                    let
                        name =
                            playerName
                                (Types.otherPlayer game.player)
                                game

                        leftMsg =
                            name ++ " left"

                        game2 =
                            { game
                                | gameid = ""
                                , playerid = ""
                                , otherPlayerid = ""
                            }

                        model2 =
                            { model
                                | error =
                                    if player == game2.player then
                                        Nothing

                                    else
                                        Just leftMsg
                            }
                    in
                    if game2.isLocal then
                        ( Just game2, model2 |> withNoCmd )

                    else
                        ( Just { game2 | isLive = False }
                        , model2
                            |> withCmds
                                [ if player /= game2.player then
                                    maybeSendNotification game2 True leftMsg model2

                                  else
                                    Cmd.none
                                ]
                        )
                )

        UpdateRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    ( Just { game | gameState = gameState }
                    , model |> withNoCmd
                    )
                )

        PlayRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    let
                        sound =
                            if gameState.whoseTurn /= game.gameState.whoseTurn then
                                Task.perform PlaySound <| Task.succeed "sounds/move.mp3"

                            else if
                                (gameState.jumps /= [])
                                    && (gameState.jumps /= game.gameState.jumps)
                            then
                                Task.perform PlaySound <| Task.succeed "sounds/jump.mp3"

                            else
                                Cmd.none
                    in
                    if not game.isLocal then
                        ( Just
                            { game
                                | gameState = gameState
                                , yourWins = computeYourWins gameState model
                            }
                        , model
                            |> withCmds
                                [ maybeSendNotification
                                    game
                                    False
                                    "It's your turn in AGOG."
                                    model
                                , sound
                                ]
                        )

                    else
                        ( Just { game | gameState = gameState }
                        , model
                            |> withCmd sound
                        )
                )

        ResignRsp { gameid, gameState, player } ->
            withRequiredGame gameid
                (\game ->
                    let
                        resignMsg =
                            if game.player == player then
                                "You resigned."

                            else
                                playerName player game ++ " resigned."
                    in
                    ( Just
                        { game
                            | gameState = gameState
                            , yourWins =
                                computeYourWins gameState model
                        }
                    , { model
                        | error =
                            if game.isLocal then
                                Nothing

                            else
                                Just resignMsg
                      }
                        |> withCmd (maybeSendNotification game True resignMsg model)
                    )
                )

        AnotherGameRsp { gameid, gameState, player } ->
            withRequiredGame gameid
                (\game ->
                    let
                        ( error, msg ) =
                            if not game.isLocal && not model.requestedNew then
                                let
                                    m =
                                        playerName (Types.otherPlayer player)
                                            { game | gameState = gameState }
                                            ++ " asked for a new game"
                                in
                                ( Just m, m )

                            else
                                ( Nothing, "" )

                        mdl2 =
                            { model
                                | requestedNew = False
                                , error = error
                            }

                        cmd =
                            if error == Nothing then
                                Cmd.none

                            else
                                maybeSendNotification game True msg mdl2
                    in
                    ( Just
                        { game
                            | gameState = gameState
                            , player = player
                            , archives =
                                Board.archiveGame gameState :: game.archives
                        }
                    , mdl2 |> withCmd cmd
                    )
                )

        GameOverRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    ( Just { game | gameState = gameState }
                    , model |> withNoCmd
                    )
                )

        PublicGamesRsp { games } ->
            ( Nothing
            , { model | publicGames = games }
                |> withNoCmd
            )

        PublicGamesUpdateRsp { added, removed } ->
            let
                games =
                    List.filter
                        (\{ gameid } -> not <| List.member gameid removed)
                        model.publicGames
            in
            ( Nothing
            , { model | publicGames = List.concat [ games, added ] }
                |> withNoCmd
            )

        StatisticsRsp { statistics, startTime, updateTime } ->
            ( Nothing
            , { model
                | statistics = statistics
                , statisticsTimes = ( startTime, updateTime )
              }
                |> withNoCmd
            )

        ErrorRsp { request, text } ->
            let
                errorReturn () =
                    { model | error = Just text }
                        |> withNoCmd
            in
            ( Nothing
            , case WSFED.decodeMessage ED.messageDecoder request of
                Ok (UpdateReq { playerid }) ->
                    -- Server has forgotten the game.
                    -- Restore it.
                    case gameFromPlayerId playerid model of
                        Nothing ->
                            { model
                                | error =
                                    Just "Bug: Can't restore game."
                            }
                                |> withNoCmd

                        Just game ->
                            webSocketConnect
                                game
                                (ConnectionSpec game.gamename <|
                                    RestoreGameConnection game
                                )
                                model

                Ok (NewReq { maybeGameid }) ->
                    case maybeGameid of
                        Nothing ->
                            errorReturn ()

                        Just gameid ->
                            case gameFromId gameid model of
                                Nothing ->
                                    errorReturn ()

                                Just restoredGame ->
                                    case playerName restoredGame.player restoredGame of
                                        "" ->
                                            errorReturn ()

                                        _ ->
                                            webSocketConnect
                                                restoredGame
                                                (ConnectionSpec restoredGame.gamename <|
                                                    JoinRestoredGameConnection gameid
                                                )
                                                model

                _ ->
                    errorReturn ()
            )

        ChatRsp { gameid, name, text } ->
            withRequiredGame gameid
                (\game ->
                    let
                        ( model2, maybeTuple ) =
                            updateChat2 game.gamename
                                model
                                (\chat ->
                                    ElmChat.addLineSpec chat <|
                                        ElmChat.makeLineSpec text
                                            (Just name)
                                            (Just model.time)
                                )
                    in
                    ( Nothing
                    , case maybeTuple of
                        Nothing ->
                            model2 |> withNoCmd

                        Just ( _, cmd ) ->
                            model2
                                |> withCmds
                                    [ cmd

                                    -- Kluge. ElmChat is supposed to do this
                                    , Task.attempt (\_ -> Noop) <|
                                        Dom.setViewportOf ids.chatOutput 0 1000000
                                    , maybeSendNotification game
                                        True
                                        ("You got an AGOG chat message from " ++ name)
                                        model
                                    ]
                    )
                )

        _ ->
            ( Nothing, model |> withNoCmd )


computeYourWins : GameState -> Model -> Int
computeYourWins gameState model =
    let
        game =
            model.game

        winner =
            gameState.winner
    in
    if
        (winner /= NoWinner)
            && (game.gameState.winner == NoWinner)
            && Just game.player
            == Types.winPlayer winner
    then
        game.yourWins + 1

    else
        game.yourWins


setPage : Page -> Cmd Msg
setPage page =
    Task.perform SetPage <| Task.succeed page


notificationHandler : Notification.Response -> State -> Model -> ( Model, Cmd Msg )
notificationHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        Notification.NoResponse ->
            model |> withNoCmd

        Notification.AvailableResponse available ->
            { model | notificationAvailable = Just available }
                |> withNoCmd

        Notification.PermissionResponse permission ->
            let
                enabled =
                    case model.notificationPermission of
                        Nothing ->
                            permission == PermissionGranted

                        _ ->
                            model.notificationsEnabled
            in
            { model
                | notificationPermission =
                    Just permission
                , error =
                    if permission == PermissionDenied then
                        Just "You denied notification permission. This can only be changed in your brower's settings."

                    else
                        model.error
                , notificationsEnabled = enabled
            }
                |> withCmd
                    (if enabled && not model.notificationsEnabled then
                        sendNotification "Notifications Enabled!"

                     else
                        Cmd.none
                    )

        Notification.NotificationResponse notification ->
            let
                n =
                    Debug.log "notification" notification
            in
            model |> withNoCmd

        Notification.ClickResponse id ->
            model
                |> withCmds
                    [ setPage MainPage
                    , notificationCmd (Notification.dismissNotification id)
                    ]

        Notification.ErrorResponse s ->
            { model | error = Just <| "Notification error: " ++ s }
                |> withNoCmd


maybeSendNotification : Game -> Bool -> String -> Model -> Cmd Msg
maybeSendNotification game ignoreWhoseTurn title model =
    if
        model.notificationsEnabled
            && (ignoreWhoseTurn || game.gameState.whoseTurn == game.player)
            && not game.isLocal
            && not model.visible
    then
        sendNotification title

    else
        Cmd.none


sendNotification : String -> Cmd Msg
sendNotification title =
    Notification.displayNotification title
        |> notificationCmd


notificationCmd : Notification.Message -> Cmd Msg
notificationCmd message =
    message
        |> Notification.send (getCmdPort Notification.moduleName ())


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
                    case findGame (.isLocal >> not) model of
                        Nothing ->
                            { model
                                | error =
                                    Just "Bug: Can't find a non-local game for an incoming message."
                            }
                                |> withNoCmd

                        Just game ->
                            { model | error = Nothing }
                                |> withCmd
                                    (Task.perform (IncomingMessage game.interface) <|
                                        Task.succeed message
                                    )

        ClosedResponse { expected, reason } ->
            { model
                | connectionSpecQueue = Fifo.empty
                , error =
                    if Debug.log "ClosedResponse, expected" expected then
                        model.error

                    else
                        Just <| "Connection unexpectedly closed: " ++ reason
            }
                |> updateAllGames
                    (\g ->
                        if g.isLocal then
                            Nothing

                        else
                            Just { g | isLive = False }
                    )
                |> withNoCmd

        ConnectedResponse crrec ->
            connectedResponse model

        _ ->
            model |> withNoCmd


connectedResponse : Model -> ( Model, Cmd Msg )
connectedResponse model =
    let
        ( maybeConnectionSpec, model2 ) =
            removeConnectionSpec model
    in
    { model2 | error = Nothing }
        |> withCmds
            [ if isConnectionSpecQueueEmpty model2 then
                Cmd.none

              else
                Task.perform identity <|
                    Task.succeed DoConnectedResponse
            , case maybeConnectionSpec of
                Nothing ->
                    Cmd.none

                Just { gamename, connectionReason } ->
                    case lookupGame gamename model of
                        Nothing ->
                            let
                                mcs =
                                    Debug.log "connectedResponse: can't find gamname" maybeConnectionSpec
                            in
                            Cmd.none

                        Just game ->
                            processConnectionReason game connectionReason model2
            ]


processConnectionReason : Game -> ConnectionReason -> Model -> Cmd Msg
processConnectionReason game connectionReason model =
    let
        interface =
            game.interface
    in
    case Debug.log "processConnectionReason" connectionReason of
        StartGameConnection ->
            let
                settings =
                    model.settings
            in
            send interface <|
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
                    , gamename = game.gamename
                    , restoreState = Nothing
                    , maybeGameid = Nothing
                    }

        JoinGameConnection gameid ->
            send interface <|
                JoinReq
                    { gameid = gameid
                    , name = model.settings.name
                    , isRestore = False
                    }

        PublicGamesConnection ->
            send interface <|
                PublicGamesReq
                    { subscribe = model.page == PublicPage
                    , forName = model.settings.name
                    , gameid = Just model.game.gameid
                    }

        StatisticsConnection ->
            send interface <|
                StatisticsReq
                    { subscribe = model.page == StatisticsPage
                    }

        UpdateConnection playerid ->
            send interface <|
                UpdateReq
                    { playerid = playerid }

        RestoreGameConnection localGame ->
            let
                player =
                    localGame.player

                name =
                    playerName player localGame
            in
            if name == "" then
                Cmd.none

            else
                send interface <|
                    NewReq
                        { name = name
                        , player = player
                        , publicType = NotPublic
                        , gamename = localGame.gamename
                        , restoreState = Just localGame.gameState
                        , maybeGameid = Just localGame.gameid
                        }

        JoinRestoredGameConnection gameid ->
            -- Errors are generated in ErrorRsp handler,
            -- before it generates the Cmd that gets here.
            case gameFromId gameid model of
                Nothing ->
                    Cmd.none

                Just localGame ->
                    let
                        name =
                            playerName localGame.player localGame
                    in
                    if name == "" then
                        Cmd.none

                    else
                        send interface <|
                            JoinReq
                                { gameid = gameid
                                , name = name
                                , isRestore = True
                                }


focusId : String -> Cmd Msg
focusId id =
    Task.attempt (\_ -> Noop) (Dom.focus id)


onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    on "keydown" (JD.map tagger keyCode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        clearClearStorage =
            model.reallyClearStorage
                && (case msg of
                        ClearStorage ->
                            False

                        Tick _ ->
                            False

                        _ ->
                            True
                   )

        ( mdl, cmd ) =
            updateInternal msg <|
                if clearClearStorage then
                    { model | reallyClearStorage = False, error = Nothing }

                else
                    model

        { white, black } =
            mdl.game.gameState.players

        focus =
            --not game.isLocal && game.isLive && white /= "" && black /= ""
            --Might be able to be smart and do this just on desktop, but not for now.
            --Focusing on mobile zooms the screen and shows the keyboard.
            False

        doSave =
            case msg of
                Noop ->
                    False

                Tick _ ->
                    model.tick == zeroTick

                Click _ ->
                    cmd == Cmd.none

                NewGame ->
                    --False
                    True

                Process _ ->
                    False

                IncomingMessage _ _ ->
                    -- cmd == Cmd.none
                    True

                MaybeClearStorage ->
                    False

                ClearStorage ->
                    False

                ChatUpdate _ _ _ ->
                    False

                ChatSend _ _ ->
                    False

                ChatClear ->
                    False

                PlaySound _ ->
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


showingArchiveOrMove : Model -> Bool
showingArchiveOrMove model =
    model.showArchive /= Nothing || model.showMove /= Nothing


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    let
        game =
            model.game

        gameState =
            game.gameState

        settings =
            model.settings
    in
    case msg of
        Noop ->
            model |> withNoCmd

        Tick posix ->
            { model
                | tick = posix
                , game =
                    if model.tick /= zeroTick then
                        if not game.isLocal then
                            game

                        else
                            { game
                                | interface =
                                    updateServerTime model.tick game.interface
                            }

                    else
                        { game
                            | interface =
                                updateServerSeed (Just <| makeSeed posix)
                                    game.interface
                        }
            }
                |> withCmd
                    (if model.tick == zeroTick && model.started then
                        -- This is also done by storageHandler,
                        -- but only if the seed has been
                        -- initialized.
                        get pk.model

                     else
                        Cmd.none
                    )

        IncomingMessage interface message ->
            incomingMessage interface message model

        SetChooseFirst player ->
            { model | chooseFirst = player }
                |> withNoCmd

        SetRotate rotate ->
            { model | rotate = rotate }
                |> withNoCmd

        SetIsLocal isLocal ->
            if game.isLocal == isLocal || showingArchiveOrMove model then
                model |> withNoCmd

            else
                let
                    interface =
                        if isLocal then
                            proxyServer <| modelSeed model

                        else
                            game.interface

                    game2 =
                        { game
                            | isLocal = isLocal
                            , isLive = False
                            , playerid = ""
                            , otherPlayerid = ""
                            , interface = interface
                            , interfaceIsProxy = isLocal
                        }

                    model2 =
                        { model
                            | game = game2
                            , gameid = ""
                        }
                in
                model2
                    |> withCmds
                        [ putGame game2
                        , if isLocal && not game.isLocal then
                            Cmd.batch
                                [ if game.isLive then
                                    send interface <|
                                        LeaveReq { playerid = game.playerid }

                                  else
                                    Cmd.none
                                , initialNewReqCmd game2 model
                                ]

                          else
                            Cmd.none
                        ]

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
            { model | game = { game | serverUrl = serverUrl } }
                |> withNoCmd

        SetGameid gameid ->
            { model | gameid = gameid }
                |> withNoCmd

        SetGameName gamename ->
            { model | gamename = gamename }
                |> withNoCmd

        SwitchGame gamename ->
            let
                mdl =
                    { model | error = Nothing }
            in
            if gamename == game.gamename then
                mdl |> withNoCmd

            else if gamename == "" then
                -- New Game
                let
                    newGamename =
                        mdl.gamename

                    newChat =
                        initialChatSettings newGamename mdl.zone

                    newGame =
                        let
                            agame =
                                initialGame Nothing
                        in
                        { agame | gamename = newGamename }
                in
                case lookupGame newGamename mdl of
                    Just _ ->
                        mdl |> withNoCmd

                    Nothing ->
                        { mdl
                            | game = newGame
                            , gameid = newGame.gameid
                            , gameDict =
                                Dict.insert game.gamename game model.gameDict
                            , chatDict =
                                Dict.insert newGamename newChat model.chatDict
                            , showArchive = Nothing
                            , showMove = Nothing
                        }
                            |> withCmds
                                [ putGame newGame
                                , putChat newGamename newChat
                                ]

            else
                case Dict.get gamename model.gameDict of
                    Nothing ->
                        { mdl
                            | error =
                                Just <| "Bug: can't find game named " ++ gamename
                        }
                            |> withNoCmd

                    Just newGame ->
                        { mdl
                            | gamename = gamename
                            , game = newGame
                            , gameid = newGame.gameid
                            , gameDict =
                                Dict.remove gamename model.gameDict
                                    |> Dict.insert model.game.gamename model.game
                            , showArchive = Nothing
                            , showMove = Nothing
                        }
                            |> withNoCmd

        RenameGame ->
            let
                newGamename =
                    model.gamename

                oldGamename =
                    game.gamename

                chatDict =
                    model.chatDict

                gameDict =
                    model.gameDict

                chat =
                    case Dict.get oldGamename chatDict of
                        Just achat ->
                            achat

                        Nothing ->
                            initialChatSettings newGamename model.zone
            in
            if newGamename == "" then
                -- Delete game
                if not game.isLocal && game.isLive then
                    model |> withNoCmd

                else
                    case List.head <| Dict.toList gameDict of
                        Nothing ->
                            model |> withNoCmd

                        Just ( nextName, nextGame ) ->
                            { model
                                | gamename = nextName
                                , game = nextGame
                                , gameid = nextGame.gameid
                                , gameDict = Dict.remove nextName gameDict
                                , chatDict = Dict.remove oldGamename chatDict
                                , showArchive = Nothing
                                , showMove = Nothing
                            }
                                |> withCmds
                                    [ put (gameKey oldGamename) Nothing
                                    , put (chatKey oldGamename) Nothing
                                    ]

            else if Nothing /= lookupGame model.gamename model then
                model |> withNoCmd

            else
                let
                    g =
                        { game | gamename = newGamename }

                    ( savedGame, renamedGame, ( showArchive, showMove ) ) =
                        case model.showMove of
                            Nothing ->
                                case model.showArchive of
                                    Nothing ->
                                        ( g, g, ( Nothing, Nothing ) )

                                    Just ( archivedGame, index ) ->
                                        let
                                            ag =
                                                { archivedGame
                                                    | gamename = newGamename
                                                }
                                        in
                                        ( ag, g, ( Nothing, Just ( ag, index ) ) )

                            Just ( movedGame, index ) ->
                                let
                                    mg =
                                        { movedGame | gamename = newGamename }
                                in
                                ( mg, g, ( Just ( mg, index ), model.showArchive ) )
                in
                { model
                    | game = renamedGame
                    , chatDict =
                        Dict.remove oldGamename chatDict
                            |> Dict.insert newGamename chat
                    , showArchive = showArchive
                    , showMove = showMove
                }
                    |> withCmds
                        [ put (gameKey oldGamename) Nothing
                        , putGame savedGame
                        , put (chatKey oldGamename) Nothing
                        , putChat newGamename chat
                        ]

        SetPage page ->
            let
                mdl =
                    { model | page = page }

                ( mdl2, cmd ) =
                    if page == PublicPage then
                        webSocketConnect
                            model.game
                            (ConnectionSpec mdl.gamename PublicGamesConnection)
                            mdl

                    else if page == StatisticsPage then
                        webSocketConnect
                            model.game
                            (ConnectionSpec mdl.gamename StatisticsConnection)
                            mdl

                    else
                        ( mdl, Cmd.none )

                interface =
                    mdl2.game.interface

                cmd2 =
                    if page == StatisticsPage then
                        send interface <|
                            StatisticsReq { subscribe = True }

                    else if model.page == StatisticsPage then
                        send interface <|
                            StatisticsReq { subscribe = False }

                    else if page == MainPage then
                        case Dict.get game.gamename model.chatDict of
                            Nothing ->
                                Cmd.none

                            Just chat ->
                                ElmChat.restoreScroll chat

                    else
                        Cmd.none

                cmd3 =
                    if page == PublicPage then
                        send interface <|
                            PublicGamesReq
                                { subscribe = True
                                , forName = ""
                                , gameid = Nothing
                                }

                    else if model.page == PublicPage then
                        send interface <|
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

        NewGame ->
            let
                resigning =
                    if not game.isLocal then
                        game.player

                    else
                        gameState.whoseTurn

                pid =
                    if not game.isLocal then
                        game.playerid

                    else
                        case resigning of
                            WhitePlayer ->
                                game.playerid

                            BlackPlayer ->
                                game.otherPlayerid

                ( playerid, placement ) =
                    if gameState.winner == NoWinner then
                        ( pid
                        , ChooseResign resigning
                        )

                    else
                        let
                            player =
                                if game.isLocal then
                                    WhitePlayer

                                else
                                    -- This should probably be enforced
                                    -- by the server.
                                    Types.otherPlayer game.player
                        in
                        ( game.playerid, ChooseNew player )
            in
            { model | requestedNew = True }
                |> withCmd
                    (send model.game.interface <|
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
            if showingArchiveOrMove model then
                model |> withNoCmd

            else
                disconnect model

        SetTestMode isTestMode ->
            setTestMode isTestMode model

        SetNotificationsEnabled enabled ->
            if not enabled then
                { model | notificationsEnabled = False }
                    |> withNoCmd

            else
                case model.notificationPermission of
                    Nothing ->
                        model
                            |> withCmd
                                (notificationCmd Notification.requestPermission)

                    Just PermissionDenied ->
                        { model | notificationsEnabled = False }
                            |> withNoCmd

                    _ ->
                        { model | notificationsEnabled = enabled }
                            |> withCmd
                                (if enabled then
                                    sendNotification "Notifications Enabled!"

                                 else
                                    Cmd.none
                                )

        SetSoundEnabled bool ->
            { model | soundEnabled = bool }
                |> withNoCmd

        EraseBoard ->
            let
                game2 =
                    { game
                        | gameState =
                            { gameState
                                | newBoard = Board.empty
                                , selected = Nothing
                                , legalMoves = NoMoves
                            }
                    }
            in
            { model | game = game2 }
                |> withCmd (putGame game2)

        RevertBoard ->
            case gameState.testModeInitialState of
                Nothing ->
                    model |> withNoCmd

                Just { board, moves, whoseTurn, selected, legalMoves } ->
                    let
                        game2 =
                            { game
                                | gameState =
                                    { gameState
                                        | newBoard = board
                                        , moves = moves
                                        , whoseTurn = whoseTurn
                                        , selected = selected
                                        , legalMoves = legalMoves
                                    }
                            }
                    in
                    { model | game = game2 }
                        |> withCmd (putGame game2)

        InitialBoard ->
            let
                game2 =
                    { game
                        | gameState =
                            { gameState
                                | moves = []
                                , newBoard = Board.initial
                                , selected = Nothing
                                , whoseTurn = WhitePlayer
                                , legalMoves = NoMoves
                            }
                    }
            in
            { model | game = game2 }
                |> withCmd (putGame game2)

        SetTestClear testClear ->
            case gameState.testMode of
                Nothing ->
                    model |> withNoCmd

                Just testMode ->
                    { model
                        | game =
                            { game
                                | gameState =
                                    { gameState
                                        | testMode =
                                            Just { testMode | clear = testClear }
                                    }
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
                        | game =
                            { game
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
                        | game =
                            { game
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
            { mdl
                | started = True
                , windowSize = model.windowSize
                , notificationAvailable = model.notificationAvailable
                , tick = model.tick
            }
                |> withCmds [ clearStorage ]

        Click ( row, col ) ->
            if showingArchiveOrMove model then
                model |> withNoCmd

            else if gameState.testMode /= Nothing then
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
                    (send model.game.interface
                        (PlayReq
                            { playerid = game.playerid
                            , placement = ChooseUndoJump undoWhichJumps
                            }
                        )
                    )

        ChatUpdate gamename chatSettings cmd ->
            updateChat gamename model (always chatSettings)
                |> Tuple.first
                |> withCmds [ cmd, putChat gamename chatSettings ]

        ChatSend line chatSettings ->
            chatSend line chatSettings model

        ChatClear ->
            clearChatSettings game.gamename False model

        PlaySound file ->
            if not model.soundEnabled then
                model |> withNoCmd

            else
                { model | soundFile = Just file }
                    |> withCmd (playAudio file)

        DelayedAction updater time ->
            updater { model | time = time }

        SetZone zone ->
            let
                mapper _ chat =
                    { chat | zone = zone }
            in
            { model
                | zone = zone
                , chatDict =
                    Dict.map mapper model.chatDict
            }
                |> withNoCmd

        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        VisibilityChange visibility ->
            { model | visible = visibility }
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

        DoConnectedResponse ->
            connectedResponse model

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


gameStateToTestModeInitialState : GameState -> TestModeInitialState
gameStateToTestModeInitialState gameState =
    let
        { newBoard, moves, whoseTurn, selected, legalMoves } =
            gameState
    in
    { board = newBoard
    , moves = moves
    , whoseTurn = whoseTurn
    , selected = selected
    , legalMoves = legalMoves
    }


setTestMode : Bool -> Model -> ( Model, Cmd Msg )
setTestMode isTestMode model =
    let
        game =
            model.game

        gameState =
            game.gameState
    in
    if isTestMode == (gameState.testMode /= Nothing) then
        model |> withNoCmd

    else
        let
            gs =
                if isTestMode then
                    { gameState
                        | testMode =
                            case model.lastTestMode of
                                Nothing ->
                                    Just
                                        { piece = { pieceType = Golem, color = WhiteColor }
                                        , clear = False
                                        }

                                jtm ->
                                    jtm
                        , testModeInitialState =
                            Just <| gameStateToTestModeInitialState gameState
                    }

                else
                    let
                        ( initialBoard, moves ) =
                            if Just (gameStateToTestModeInitialState gameState) == gameState.testModeInitialState then
                                ( gameState.initialBoard, gameState.moves )

                            else
                                ( Just gameState.newBoard, [] )
                    in
                    { gameState
                        | initialBoard = initialBoard
                        , moves = moves
                        , testMode = Nothing
                        , testModeInitialState = Nothing
                    }

            cmd =
                if not isTestMode then
                    send model.game.interface
                        (SetGameStateReq
                            { playerid = game.playerid
                            , gameState =
                                { gs
                                    | winner = NoWinner
                                    , whoseTurn =
                                        if gs.newBoard == Board.initial then
                                            WhitePlayer

                                        else
                                            gs.whoseTurn
                                }
                            }
                        )

                else
                    Cmd.none

            game2 =
                { game | gameState = gs }
        in
        { model
            | game = game2
            , lastTestMode =
                if isTestMode then
                    Nothing

                else
                    gameState.testMode
        }
            |> withCmds [ cmd, putGame game2 ]


clearChatSettings : String -> Bool -> Model -> ( Model, Cmd Msg )
clearChatSettings gamename clearInput model =
    let
        ( model2, maybeChat ) =
            updateChat (Debug.log "Clear chat for game" gamename)
                model
                (\chat ->
                    { chat
                        | lines = []
                        , input =
                            if clearInput then
                                ""

                            else
                                chat.input
                    }
                )
    in
    case maybeChat of
        Nothing ->
            model2 |> withNoCmd

        Just chat ->
            model2 |> withCmd (putChat gamename chat)


chatSend : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSend line chatSettings model =
    model
        |> withCmd (delayedAction <| chatSendInternal line chatSettings)


chatSendInternal : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSendInternal line chatSettings model =
    let
        ( model2, _ ) =
            updateChat model.game.gamename model (always chatSettings)
    in
    model2
        |> withCmd
            (send model2.game.interface <|
                ChatReq
                    { playerid = model.game.playerid
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
        model.game.serverUrl
        Noop


webSocketConnect : Game -> ConnectionSpec -> Model -> ( Model, Cmd Msg )
webSocketConnect game spec model =
    if game.isLocal then
        let
            newGame =
                { game
                    | interface =
                        if game.interfaceIsProxy then
                            game.interface

                        else
                            proxyServer <| modelSeed model
                    , interfaceIsProxy = True
                    , isLive = True
                }
        in
        updateGame game.gamename (always game) model
            |> Tuple.first
            |> withNoCmd

    else
        let
            newGame =
                { game
                    | interface =
                        if True then
                            --game.interfaceIsProxy then
                            makeWebSocketServer model

                        else
                            game.interface
                    , interfaceIsProxy = False
                }
        in
        updateGame game.gamename (always newGame) model
            |> Tuple.first
            |> insertConnectionSpec (Debug.log "webSocketConnect" spec)
            |> withCmd
                (WebSocket.makeOpen game.serverUrl
                    |> webSocketSend
                )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    webSocketConnect model.game
        (ConnectionSpec model.game.gamename StartGameConnection)
        model


join : Model -> ( Model, Cmd Msg )
join model =
    let
        gameid =
            model.gameid

        game =
            model.game

        model2 =
            { model
                | game =
                    -- Needed by JoinRsp
                    { game | gameid = gameid }
            }
    in
    webSocketConnect model2.game
        (ConnectionSpec model.game.gamename (JoinGameConnection gameid))
        model2


disconnect : Model -> ( Model, Cmd Msg )
disconnect model =
    let
        game =
            model.game

        gameState =
            if not game.isLocal then
                game.gameState

            else
                let
                    gs =
                        game.gameState
                in
                { gs | score = Types.zeroScore }

        game2 =
            if not game.isLocal then
                { game | isLive = False }

            else
                { game
                    | archives = []
                    , gameState = gameState
                }
    in
    { model | game = game2 }
        |> withCmds
            [ putGame game2
            , if game.isLive && not game.isLocal then
                send model.game.interface <|
                    LeaveReq { playerid = game.playerid }

              else
                Cmd.none
            , if game.isLocal then
                send model.game.interface <|
                    SetGameStateReq
                        { playerid = game.playerid
                        , gameState = gameState
                        }

              else
                Cmd.none
            ]


send : ServerInterface -> Message -> Cmd Msg
send interface message =
    let
        logMessage =
            Debug.log "send" <| ED.messageToLogMessage message
    in
    ServerInterface.send interface message


doTestClick : Int -> Int -> Model -> ( Model, Cmd Msg )
doTestClick row col model =
    let
        game =
            model.game

        gameState =
            game.gameState

        board =
            gameState.newBoard
    in
    case gameState.testMode of
        Nothing ->
            model |> withNoCmd

        Just testMode ->
            if testMode.clear then
                { model
                    | game =
                        { game
                            | gameState =
                                { gameState
                                    | newBoard =
                                        Board.set (rc row col) Types.emptyPiece board
                                }
                                    |> Board.populateLegalMoves
                        }
                }
                    |> withNoCmd

            else
                let
                    rowcol =
                        rc row col

                    { pieceType } =
                        Board.get rowcol board

                    gs =
                        if pieceType == NoPiece then
                            { gameState
                                | newBoard =
                                    Board.set rowcol testMode.piece board
                            }

                        else
                            { gameState | selected = Just rowcol }
                in
                { model
                    | game =
                        { game | gameState = gs |> Board.populateLegalMoves }
                }
                    |> withNoCmd


doClick : Int -> Int -> Model -> ( Model, Cmd Msg )
doClick row col model =
    let
        game =
            model.game

        gameState =
            game.gameState

        rowCol =
            rc row col

        board =
            gameState.newBoard

        player =
            gameState.whoseTurn

        otherPlayer =
            Types.otherPlayer player

        { pieceType } =
            Board.get rowCol board

        selected =
            gameState.selected

        ( selectedType, selectedColor ) =
            case selected of
                Nothing ->
                    ( NoPiece, WhiteColor )

                Just selectedRc ->
                    let
                        p =
                            Board.get selectedRc board
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
            || (not game.isLocal && player /= game.player)
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
                            (rowCol /= Board.playerSanctum otherPlayer)
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
                            if Board.mapWholeBoard mapper board False then
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
                                    Board.get jumpedRc board |> .pieceType
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
        game =
            model.game

        gameState =
            game.gameState

        withPlayReq playerid placement =
            withCmd <|
                send game.interface
                    (PlayReq
                        { playerid = playerid
                        , placement = placement
                        }
                    )

        withACmd =
            withPlayReq game.playerid <|
                let
                    piece =
                        Board.get rowCol gameState.newBoard
                in
                case piece.pieceType of
                    NoPiece ->
                        let
                            { corruptJumped, makeHulk } =
                                model.chooseMoveOptionsUI

                            chooseMoveOption =
                                if corruptJumped == AskYes () then
                                    CorruptJumped

                                else
                                    case makeHulk of
                                        AskYes hulkPos ->
                                            MakeHulk hulkPos

                                        _ ->
                                            NoOption
                        in
                        ChooseMove rowCol chooseMoveOption

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
        , onVisibilityChange VisibilityChange -- Browser.Events.onVisibilityChange doesn't work
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

        game =
            model.game

        gameState =
            game.gameState

        score =
            gameState.score

        count =
            Board.count gameState.newBoard

        { white, black } =
            gameState.players

        currentPlayer =
            if not game.isLocal then
                Just game.player

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
            if not game.isLive then
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
                                playerName player game

                            name =
                                if game.isLocal || player /= game.player then
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
                                    not game.isLocal
                                        && (gameState.whoseTurn /= game.player)
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
        [ Lazy.lazy6 Board.render
            theStyle
            bsize
            Click
            currentPlayer
            rotated
            gameState
        , span
            []
            [ br
            , case model.error of
                Nothing ->
                    text ""

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
                        "125%"

                    else
                        "100%"
                ]
                [ text message ]
            , br
            , if
                (gameState.winner /= NoWinner)
                    || (gameState.moves == [] && gameState.players == Types.emptyPlayerNames)
              then
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
                            if game.isLocal then
                                color

                            else if game.player == gameState.whoseTurn then
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
            , if not game.isLocal && game.isLive then
                span []
                    [ if white == "" || black == "" then
                        text ""

                      else
                        case Dict.get model.game.gamename model.chatDict of
                            Nothing ->
                                text ""

                            Just chat ->
                                let
                                    chatSettings =
                                        chat
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
                                if game.player == WhitePlayer then
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
                                if game.player == BlackPlayer then
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
            , let
                ( moveText, moveCnt, moveIndex ) =
                    moveString model
              in
              if moveCnt == 0 then
                text ""

              else
                span []
                    [ br
                    , a
                        [ href "#"
                        , onClick <| SetPage MovesPage
                        ]
                        [ b "Moves" ]
                    , b ": "
                    , text moveText
                    ]
            , let
                { games, whiteWins, blackWins } =
                    gameState.score

                yourWins =
                    game.yourWins

                player =
                    game.player

                otherPlayer =
                    Types.otherPlayer player
              in
              if games == 0 then
                text ""

              else
                let
                    ( yourWinString, otherWinString ) =
                        if game.isLocal then
                            ( "White won " ++ String.fromInt whiteWins
                            , "Black won " ++ String.fromInt blackWins
                            )

                        else
                            ( "You ("
                                ++ playerName player game
                                ++ ") won "
                                ++ String.fromInt yourWins
                            , playerName otherPlayer game
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
                , checked game.isLocal
                , onCheck SetIsLocal
                , disabled <|
                    (not game.isLocal && game.isLive)
                        || showingArchiveOrMove model
                ]
                []
            , case model.notificationAvailable of
                Just True ->
                    let
                        isDisabled =
                            model.notificationPermission == Just PermissionDenied

                        theTitle =
                            if isDisabled then
                                "Notifications are disabled in the browser. You'll have to fix this in browser settings."

                            else
                                "Check to use system notifications."
                    in
                    span []
                        [ text " "
                        , b "Notifications: "
                        , input
                            [ type_ "checkbox"
                            , checked model.notificationsEnabled
                            , onCheck SetNotificationsEnabled
                            , disabled isDisabled
                            , title theTitle
                            ]
                            []
                        ]

                _ ->
                    text ""
            , text " "
            , b "Sound: "
            , input
                [ type_ "checkbox"
                , checked model.soundEnabled
                , onCheck SetSoundEnabled
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
            , br
            , b "Game name: "
            , input
                [ onInput SetGameName
                , value model.gamename
                , size 20
                ]
                []
            , text " "
            , let
                delete =
                    model.gamename == ""

                cantDelete =
                    delete
                        && model.game.isLive
                        && not model.game.isLocal

                onlyGame =
                    delete && Dict.isEmpty model.gameDict

                isCurrent =
                    model.gamename == model.game.gamename

                exists =
                    Nothing /= lookupGame model.gamename model
              in
              button
                [ disabled <| isCurrent || cantDelete || onlyGame || exists
                , title <|
                    if isCurrent then
                        "Rename to the same name? Nope."

                    else if cantDelete then
                        "You may not delete an active network game."

                    else if onlyGame then
                        "You may not delete the only game."

                    else if delete then
                        "Delete the current game."

                    else if exists then
                        "You may not rename to an existing name."

                    else
                        "Rename the current game."
                , onClick RenameGame
                ]
                [ text <|
                    if delete then
                        "Delete"

                    else
                        "Rename"
                ]
            , text " "
            , select
                [ onInput SwitchGame ]
              <|
                let
                    cantMakeNew =
                        (model.gamename == "")
                            || (Nothing /= lookupGame model.gamename model)
                in
                option
                    [ value ""
                    , disabled cantMakeNew
                    , title <|
                        if cantMakeNew then
                            "Type a non-blank \"Game name\" that does not yet exist."

                        else
                            "Make a new game named \"" ++ model.gamename ++ "\""
                    ]
                    [ text <| toMdashes "-- New Game --" ]
                    :: foldrGames
                        (\agame res ->
                            let
                                isCurrent =
                                    agame.gamename == game.gamename
                            in
                            option
                                [ value agame.gamename
                                , selected isCurrent
                                , title <|
                                    if isCurrent then
                                        "This is the current game."

                                    else
                                        "Switch to this game."
                                ]
                                [ text agame.gamename ]
                                :: res
                        )
                        []
                        model
            , if not (game.isLocal || WhichServer.isLocal) then
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
                                , button [ onClick RevertBoard ]
                                    [ text "Revert" ]
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
            , if game.isLocal then
                div [ align "center" ]
                    [ button
                        [ onClick Disconnect
                        , disabled <|
                            (game.archives == [])
                                && (gameState.score == Types.zeroScore)
                        ]
                        [ text "Clear Archive" ]
                    ]

              else
                div [ align "center" ]
                    [ if game.isLive then
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


moveString : Model -> ( String, Int, Int )
moveString model =
    let
        gameState =
            model.game.gameState

        moves =
            gameState.moves

        len =
            4

        movesLength =
            List.length moves

        ellipsis =
            if movesLength > len then
                ", ..."

            else
                ""

        ( prefix, count, index ) =
            case model.showMove of
                Nothing ->
                    ( "", movesLength, 0 )

                Just ( fullGame, idx ) ->
                    ( if idx > 0 then
                        "..., "

                      else
                        ""
                    , List.length fullGame.gameState.moves
                    , idx
                    )

        head =
            List.take 4 moves
    in
    ( prefix ++ movesToString head ++ ellipsis, count, index )


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
    List.map ED.oneMoveToPrettyString moves
        |> String.join ", "


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
        game =
            model.game

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
                [ if game.isLive then
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
                        (renderPublicGameRow model.game.gameid name game.isLive)
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


alignRightStyle : Html.Attribute msg
alignRightStyle =
    style "text-align" "right"


smallTextStyle : Html.Attribute msg
smallTextStyle =
    style "font-size" "80%"


type alias Format =
    List DateFormat.Token


dateAndTimeFormat : Format
dateAndTimeFormat =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " "
    , DateFormat.monthNameFull
    , DateFormat.text " "
    , DateFormat.yearNumber
    , DateFormat.text ", "
    , DateFormat.hourNumber
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.amPmLowercase
    ]


formatPosix : Zone -> Format -> Posix -> String
formatPosix zone format posix =
    DateFormat.format format zone posix


formatUtc : Format -> Posix -> String
formatUtc format posix =
    formatPosix Time.utc format posix


dateAndTimeString : Zone -> Posix -> String
dateAndTimeString zone posix =
    formatPosix zone dateAndTimeFormat posix


sFormat : Format
sFormat =
    [ DateFormat.text ":"
    , DateFormat.secondFixed
    ]


msFormat : Format
msFormat =
    [ DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


hmsFormat : Format
hmsFormat =
    [ DateFormat.hourFixed
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


militaryFormat : Format
militaryFormat =
    [ DateFormat.hourMilitaryFixed
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


hmsString : Bool -> Posix -> String
hmsString isMilitary posix =
    let
        millis =
            Time.posixToMillis posix

        days =
            millis // (24 * 60 * 60 * 1000)

        secs =
            (toFloat millis / 1000 |> round) - (days * 24 * 60 * 60)

        format =
            if isMilitary then
                militaryFormat

            else if days > 0 then
                hmsFormat

            else if secs < 60 then
                sFormat

            else if secs < 60 * 60 then
                msFormat

            else
                hmsFormat

        dayString =
            if days > 0 then
                String.fromInt days ++ ":"

            else
                ""
    in
    dayString
        ++ (formatUtc format <| Time.millisToPosix (1000 * secs))


roundPosix : Posix -> Posix
roundPosix posix =
    Time.posixToMillis posix
        |> (\m -> 1000 * round (toFloat m / 1000))
        |> Time.millisToPosix


movesPage : Int -> Model -> Html Msg
movesPage bsize model =
    let
        settings =
            model.settings

        name =
            settings.name

        game =
            model.game

        gameState =
            game.gameState

        moves =
            List.reverse gameState.moves

        gameTime =
            case List.head moves of
                Just move ->
                    Just move.time

                Nothing ->
                    Nothing

        ( totalTime, offsetMoves ) =
            case gameTime of
                Nothing ->
                    ( Types.posixZero, moves )

                Just time ->
                    let
                        millis =
                            Time.posixToMillis (roundPosix time)

                        moves2 =
                            moves
                                |> List.map
                                    (\move ->
                                        { move
                                            | time =
                                                (Time.posixToMillis (roundPosix move.time) - millis)
                                                    |> Time.millisToPosix
                                        }
                                    )

                        moves3 =
                            moves2
                                |> List.foldl
                                    (\move ( res, last ) ->
                                        let
                                            atime =
                                                move.time |> Time.posixToMillis
                                        in
                                        ( { move | time = Time.millisToPosix (atime - last) } :: res
                                        , atime
                                        )
                                    )
                                    ( [], 0 )
                                |> Tuple.first
                                |> List.reverse
                    in
                    ( case List.head (List.reverse moves2) of
                        Just p ->
                            p.time
                                |> Time.posixToMillis
                                |> (\a ->
                                        1000
                                            * round (toFloat a / 1000)
                                            |> Time.millisToPosix
                                   )

                        Nothing ->
                            Types.posixZero
                    , moves3
                    )

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

        gameTimeSpan =
            case gameTime of
                Nothing ->
                    text ""

                Just time ->
                    span []
                        [ b "Total time: "
                        , text <| hmsString True totalTime
                        , br
                        , text <| dateAndTimeString model.zone time
                        ]
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Moves" ]
            , p [] [ playButton ]
            , if game.isLocal then
                p []
                    [ gameTimeSpan ]

              else
                p []
                    [ b "White: "
                    , text white
                    , text ", "
                    , b "Black: "
                    , text black
                    , span []
                        [ br
                        , gameTimeSpan
                        ]
                    ]
            , table
                [ class "prettytable"
                , style "color" "black"
                ]
              <|
                [ tr []
                    [ th chars.nbsp
                    , th chars.nbsp
                    , th "White"
                    , th "Black"
                    , th chars.nbsp
                    ]
                ]
                    ++ List.indexedMap movesRow
                        (offsetMoves |> pairList)
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
        , td
            [ alignRightStyle
            , smallTextStyle
            ]
            [ case maybeWhite of
                Nothing ->
                    text chars.nbsp

                Just { time } ->
                    if index == 0 && time == Types.posixZero then
                        text chars.nbsp

                    else
                        text <| hmsString False time
            ]
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
        , td
            [ alignRightStyle
            , smallTextStyle
            ]
            [ case maybeBlack of
                Nothing ->
                    text chars.nbsp

                Just { time } ->
                    text <| hmsString False time
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
    let
        game =
            model.game
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Statistics" ]
            , p [] [ playButton ]
            , if game.isLocal then
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
    , mdash = codestr 0x2014
    }


toMdashes : String -> String
toMdashes string =
    String.replace "--" chars.mdash string



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
            model.game.playerid
    in
    put pk.model <| Just value


putChat : String -> ChatSettings -> Cmd Msg
putChat gamename settings =
    (Just <| ElmChat.settingsEncoder settings)
        |> put (chatKey gamename)


getChat : String -> Cmd Msg
getChat gamename =
    getLabeled pk.chat (chatKey gamename)


putGame : Game -> Cmd Msg
putGame game =
    (Just <| ED.encodeNamedGame game)
        |> put (gameKey game.gamename)


getGame : String -> Cmd Msg
getGame gamename =
    getLabeled pk.game (gameKey gamename)


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend <| Debug.log "LocalStorage" (LocalStorage.get key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (Debug.log "LocalStorage" <|
            LocalStorage.getLabeled label key
        )


listKeys : String -> Cmd Msg
listKeys prefix =
    localStorageSend (LocalStorage.listKeys prefix)


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


clearStorage : Cmd Msg
clearStorage =
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
        , NotificationHandler notificationHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , chat = "chat"
    , game = "game"
    }


gamePrefix : String
gamePrefix =
    pk.game ++ "."


gameKey : String -> String
gameKey gamename =
    gamePrefix ++ gamename


gameKeyToName : String -> Maybe String
gameKeyToName key =
    let
        len =
            String.length gamePrefix
    in
    if String.left len key /= gamePrefix then
        Nothing

    else
        Just <| String.dropLeft len key


chatPrefix : String
chatPrefix =
    pk.chat ++ "."


chatKey : String -> String
chatKey gamename =
    chatPrefix ++ gamename


chatKeyToName : String -> Maybe String
chatKeyToName key =
    let
        len =
            String.length chatPrefix
    in
    if String.left len key /= chatPrefix then
        Nothing

    else
        Just <| String.dropLeft len key
