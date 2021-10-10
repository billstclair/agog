port module Agog.Server.Server exposing (main)

import Agog.EncodeDecode as ED
import Agog.Interface as Interface
import Agog.Types as Types
    exposing
        ( GameState
        , Message(..)
        , Participant(..)
        , Player
        , PlayerNames
        , PublicType(..)
        , ServerState
        , SubscriptionSet
        )
import Set exposing (Set)
import Time exposing (Posix)
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , Socket
        , UserFunctions
        , program
        , verbose
        )
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , ErrorKind(..)
        , GameId
        , InputPort
        , OutputPort
        )


type alias Model =
    WebSocketFramework.Server.Model ServerModel Message GameState Participant


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


tos : Int -> String
tos x =
    String.fromInt x


errorWrapper : Error Message -> Message
errorWrapper { kind, description, message } =
    case kind of
        JsonParseError ->
            let
                err =
                    case message of
                        Err msg ->
                            msg

                        Ok msg ->
                            Debug.toString msg
            in
            ErrorRsp
                { request = description
                , text = "JSON parser error: " ++ err
                }

        _ ->
            ErrorRsp
                { request = ""
                , text = Debug.toString message
                }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoder
    , decoder = ED.messageDecoder
    , errorWrapper = Just errorWrapper
    }


{-| Two weeks
-}
deathRowDuration : Int
deathRowDuration =
    14 * 24 * 60 * 60 * 1000


messageSender : ServerMessageSender ServerModel Message GameState Participant
messageSender mdl socket state request response =
    let
        time =
            WebSocketFramework.Server.getTime mdl

        state2 =
            let
                gs =
                    case state.state of
                        Nothing ->
                            Interface.emptyGameState <| PlayerNames "" ""

                        Just gs2 ->
                            gs2

                private =
                    gs.private
            in
            if private.startTime /= Nothing then
                state

            else
                let
                    verb =
                        WebSocketFramework.Server.verbose mdl

                    verbose =
                        if verb then
                            Debug.log "verbose" verb

                        else
                            verb
                in
                { state
                    | state =
                        Just
                            { gs
                                | private =
                                    { private
                                        | startTime = Just time
                                        , verbose = Just verbose
                                    }
                            }
                }

        model =
            if WebSocketFramework.Server.getDeathRowDuration mdl == deathRowDuration then
                mdl

            else
                WebSocketFramework.Server.setDeathRowDuration mdl deathRowDuration

        ( state3, cmd3 ) =
            case request of
                PublicGamesReq { subscribe, forName } ->
                    ( handlePublicGamesSubscription subscribe forName socket state2
                    , Cmd.none
                    )

                StatisticsReq { subscribe } ->
                    ( handleStatisticsSubscription subscribe socket state2
                    , Cmd.none
                    )

                _ ->
                    case response of
                        LeaveRsp { gameid } ->
                            let
                                ( model2, cmd2 ) =
                                    gamesDeleter model [ gameid ] state2
                            in
                            ( WebSocketFramework.Server.getState model2, cmd2 )

                        _ ->
                            ( state2, Cmd.none )

        ( state4, cmd4 ) =
            computeStatisticsSubscriberSends time state3

        sender =
            case request of
                UpdateReq _ ->
                    sendToOne

                PublicGamesReq { subscribe, forName } ->
                    sendToOne

                _ ->
                    case response of
                        NewRsp { gameid } ->
                            sendNewRsp model state4

                        JoinRsp { gameid } ->
                            sendJoinRsp model state4

                        AnotherGameRsp record ->
                            \_ _ ->
                                Cmd.batch
                                    [ sendToOne response socket
                                    , sendToOthers model
                                        (AnotherGameRsp
                                            { record
                                                | player =
                                                    Types.otherPlayer record.player
                                            }
                                        )
                                        socket
                                    ]

                        _ ->
                            sendToAll model
    in
    ( WebSocketFramework.Server.setState model state4
    , Cmd.batch [ cmd3, cmd4, sender response socket ]
    )


getStatisticsTimes : Maybe Int -> ServerState -> ( ServerState, Maybe Int, Maybe Int )
getStatisticsTimes newUpdateTime state =
    case state.state of
        Nothing ->
            -- Can't happen
            ( state, Nothing, Nothing )

        Just gs ->
            let
                private =
                    gs.private

                newPrivate =
                    case newUpdateTime of
                        Nothing ->
                            private

                        _ ->
                            { private | updateTime = newUpdateTime }

                newState =
                    case newUpdateTime of
                        Nothing ->
                            state

                        _ ->
                            { state
                                | state =
                                    Just { gs | private = newPrivate }
                            }
            in
            ( newState, newPrivate.startTime, newPrivate.updateTime )


computeStatisticsSubscriberSends : Int -> ServerState -> ( ServerState, Cmd Msg )
computeStatisticsSubscriberSends time state =
    if not <| Interface.getStatisticsChanged state then
        ( state, Cmd.none )

    else
        let
            ( state2, startTime, updateTime ) =
                getStatisticsTimes (Just time) state

            message =
                StatisticsRsp
                    { statistics = state.statistics
                    , startTime = startTime
                    , updateTime = updateTime
                    }
        in
        ( Interface.setStatisticsChanged False state2
        , (List.map (sendToOne message) <| getStatisticsSubscribers state)
            |> Cmd.batch
        )


getStatisticsSubscribers : ServerState -> List Socket
getStatisticsSubscribers state =
    case state.state of
        Nothing ->
            []

        Just gameState ->
            gameState.private.statisticsSubscribers
                |> Set.toList


handleStatisticsSubscription : Bool -> String -> ServerState -> ServerState
handleStatisticsSubscription subscribe socket state =
    let
        gs =
            case state.state of
                Nothing ->
                    Interface.emptyGameState <| PlayerNames "" ""

                Just gameState ->
                    gameState

        private =
            gs.private

        subscribers =
            private.statisticsSubscribers
    in
    { state
        | state =
            Just
                { gs
                    | private =
                        { private
                            | statisticsSubscribers =
                                if subscribe then
                                    Set.insert socket subscribers

                                else
                                    Set.filter ((/=) socket) subscribers
                        }
                }
    }


handlePublicGamesSubscription : Bool -> String -> Socket -> ServerState -> ServerState
handlePublicGamesSubscription subscribe forName socket state =
    let
        gs =
            case state.state of
                Nothing ->
                    Interface.emptyGameState <| PlayerNames "" ""

                Just gameState ->
                    gameState

        private =
            gs.private

        subscribers =
            Set.filter (\( sock, _ ) -> socket /= sock) private.subscribers
    in
    { state
        | state =
            Just
                { gs
                    | private =
                        { private
                            | subscribers =
                                if subscribe then
                                    Set.insert ( socket, forName ) subscribers

                                else
                                    subscribers
                        }
                }
    }


sendToOne : Message -> Socket -> Cmd Msg
sendToOne response socket =
    WebSocketFramework.Server.sendToOne ED.messageEncoder response outputPort socket


sendToAll : Model -> Message -> Socket -> Cmd Msg
sendToAll model response socket =
    case Types.messageToGameid response of
        Nothing ->
            sendToOne response socket

        Just gameid ->
            WebSocketFramework.Server.sendToAll gameid
                model
                ED.messageEncoder
                response


sendToOthers : Model -> Message -> Socket -> Cmd Msg
sendToOthers model response socket =
    case Types.messageToGameid response of
        Nothing ->
            Cmd.none

        Just gameid ->
            WebSocketFramework.Server.sendToOthers gameid
                socket
                model
                ED.messageEncoder
                response


sendNewRsp : Model -> ServerState -> Message -> Socket -> Cmd Msg
sendNewRsp model state response socket =
    -- Need to send new public game to subscribers.
    let
        notifications =
            case state.state of
                Nothing ->
                    []

                Just gs ->
                    case response of
                        NewRsp { gameid, player, name, publicType, gameState } ->
                            if publicType == NotPublic then
                                []

                            else
                                let
                                    publicGame =
                                        { gameid = gameid
                                        , creator = name
                                        , player = player
                                        , forName =
                                            case publicType of
                                                PublicFor for ->
                                                    Just for

                                                _ ->
                                                    Nothing
                                        }

                                    publicGameAndPlayers =
                                        { publicGame = publicGame
                                        , players = gameState.players
                                        , watchers = 0
                                        }

                                    notification =
                                        sendToOne
                                            (PublicGamesUpdateRsp
                                                { added = [ publicGameAndPlayers ]
                                                , removed = []
                                                }
                                            )
                                in
                                gs.private.subscribers
                                    |> Set.toList
                                    |> List.filterMap
                                        (\( sock, forName ) ->
                                            case publicType of
                                                EntirelyPublic ->
                                                    Just <| notification sock

                                                PublicFor for ->
                                                    if forName == for then
                                                        Just <| notification sock

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                        )

                        _ ->
                            []
    in
    Cmd.batch <|
        sendToOne response socket
            :: notifications


removedGameNotifications : GameId -> ServerState -> Cmd Msg
removedGameNotifications gameid state =
    case state.state of
        Nothing ->
            Cmd.none

        Just gs ->
            let
                notification =
                    sendToOne <|
                        PublicGamesUpdateRsp
                            { added = []
                            , removed = [ gameid ]
                            }
            in
            gs.private.subscribers
                |> Set.toList
                |> List.map
                    (\( sock, _ ) ->
                        notification sock
                    )
                |> Cmd.batch


sendJoinRsp : Model -> ServerState -> Message -> Socket -> Cmd Msg
sendJoinRsp model state response socket =
    let
        notifications =
            case response of
                JoinRsp { gameid } ->
                    removedGameNotifications gameid state

                _ ->
                    Cmd.none
    in
    [ notifications
    , case response of
        JoinRsp record ->
            [ sendToOne response socket
            , sendToOthers model
                (JoinRsp { record | playerid = Nothing })
                socket
            ]
                |> Cmd.batch

        _ ->
            sendToAll model response socket
    ]
        |> Cmd.batch


gamesDeleter : Model -> List GameId -> ServerState -> ( Model, Cmd Msg )
gamesDeleter model gameids state =
    case state.state of
        Nothing ->
            ( model, Cmd.none )

        Just gs ->
            let
                private =
                    gs.private

                subscribers =
                    private.subscribers

                loop : GameId -> ( SubscriptionSet, Cmd Msg ) -> ( SubscriptionSet, Cmd Msg )
                loop gameid ( subscribers2, notifications ) =
                    let
                        inner : Socket -> ( SubscriptionSet, Cmd Msg ) -> ( SubscriptionSet, Cmd Msg )
                        inner socket ( subscribers3, notifications2 ) =
                            ( Set.filter (\( sock, _ ) -> sock /= socket)
                                subscribers3
                            , [ removedGameNotifications gameid state
                              , notifications2
                              ]
                                |> Cmd.batch
                            )

                        sockets =
                            WebSocketFramework.Server.otherSockets gameid
                                ""
                                model
                    in
                    List.foldl inner ( subscribers2, notifications ) sockets

                ( subscribers4, notifications3 ) =
                    List.foldl loop ( subscribers, Cmd.none ) gameids

                state2 =
                    { state
                        | state =
                            Just
                                { gs
                                    | private =
                                        { private
                                            | subscribers = subscribers4
                                        }
                                }
                    }
            in
            ( WebSocketFramework.Server.setState model state2
            , notifications3
            )


userFunctions : UserFunctions ServerModel Message GameState Participant
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = Interface.messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just Types.messageToGameid
    , messageToPlayerid = Just Types.messageToPlayerid
    , autoDeleteGame = Just (\gameid serverState -> False)
    , gamesDeleter = Just gamesDeleter
    , playersDeleter = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


{-| Debugging version
-}
messageProcessor : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessor state message =
    Interface.messageProcessor (Debug.log "messageProcessor" state)
        (Debug.log "  message" message)
        |> Debug.log "  output"


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
