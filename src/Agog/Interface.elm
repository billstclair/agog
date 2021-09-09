--------------------------------------------------------------------
--
-- Interface.elm
-- AGOG server interface.
-- Runs on local machine for local play, and server for networked play.
-- Copyright (c) 2019-2021x Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Agog.Interface exposing
    ( emptyGameState
    , forNameMatches
    , messageProcessor
    )

import Agog.Board as Board
import Agog.EncodeDecode as ED
import Agog.NewBoard as NewBoard
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
        , Piece
        , PieceType(..)
        , Player(..)
        , PlayerNames
        , PublicType(..)
        , RowCol
        , Score
        , ServerState
        , UndoState
        , UndoWhichJumps(..)
        , Winner(..)
        )
import Debug
import Dict exposing (Dict)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.EncodeDecode as WFED
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , ReqRsp(..)
        , ServerState
        )


emptyGameState : PlayerNames -> GameState
emptyGameState players =
    { board = Board.empty
    , newBoard = NewBoard.initial
    , moves = []
    , players = players
    , whoseTurn = WhitePlayer
    , selected = Nothing
    , jumperLocations = []
    , legalMoves = Moves []
    , undoStates = []
    , jumps = []
    , score = Types.zeroScore
    , winner = NoWinner
    , path = []
    , testMode = Nothing
    , private = Types.emptyPrivateGameState
    }


errorRes : Message -> Types.ServerState -> String -> ( Types.ServerState, Maybe Message )
errorRes message state text =
    ( state
    , Just <|
        ErrorRsp
            { request = WFED.encodeMessage ED.messageEncoder message
            , text = text
            }
    )


forNameMatches : String -> Maybe String -> Bool
forNameMatches name1 name2 =
    (name1 |> String.toLower)
        == (Maybe.withDefault "" name2 |> String.toLower)


lookupGame : Message -> PlayerId -> Types.ServerState -> Result ( Types.ServerState, Maybe Message ) ( GameId, GameState, Player )
lookupGame message playerid state =
    let
        err text =
            Err <| errorRes message state text
    in
    case ServerInterface.getPlayer playerid state of
        Nothing ->
            err "Unknown playerid"

        Just { gameid, player } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    err "Unknown gameid"

                Just gameState ->
                    Ok ( gameid, gameState, player )


messageProcessor : Types.ServerState -> Message -> ( Types.ServerState, Maybe Message )
messageProcessor state message =
    case message of
        NewReq { name, player, publicType, restoreState } ->
            if name == "" then
                errorRes message state "Blank name not allowed."

            else
                let
                    players =
                        case player of
                            WhitePlayer ->
                                { white = name
                                , black = ""
                                }

                            BlackPlayer ->
                                { white = ""
                                , black = name
                                }

                    gameState =
                        case restoreState of
                            Nothing ->
                                emptyGameState players

                            Just gs ->
                                { gs
                                    | players = players
                                }

                    ( gameid, state2 ) =
                        ServerInterface.newGameid state

                    ( playerid, state3 ) =
                        ServerInterface.newPlayerid state2

                    playerInfo =
                        { gameid = gameid, player = player }

                    state4 =
                        ServerInterface.addGame gameid gameState state3

                    state5 =
                        ServerInterface.addPlayer playerid playerInfo state4

                    forName =
                        case publicType of
                            PublicFor fn ->
                                Just fn

                            _ ->
                                Nothing

                    state6 =
                        if publicType == NotPublic then
                            state5

                        else
                            let
                                publicGame =
                                    { gameid = gameid
                                    , creator = name
                                    , player = player
                                    , forName = forName
                                    }
                                        |> ED.publicGameToFramework
                            in
                            { state5
                                | publicGames =
                                    ServerInterface.appendPublicGames
                                        publicGame
                                        state5.publicGames
                            }
                in
                ( state6
                , Just <|
                    NewRsp
                        { gameid = gameid
                        , playerid = playerid
                        , player = player
                        , name = name
                        , publicType = publicType
                        , gameState = gameState
                        }
                )

        JoinReq { gameid, name } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    errorRes message state "Unknown gameid"

                Just gameState ->
                    let
                        players =
                            gameState.players

                        { white, black } =
                            players
                    in
                    if white /= "" && black /= "" then
                        errorRes message state "Game already has two players"

                    else if name == "" || name == white || name == black then
                        errorRes message
                            state
                            ("Blank or existing name: " ++ name)

                    else
                        let
                            ( players2, player ) =
                                if white == "" then
                                    ( { players | white = name }, WhitePlayer )

                                else
                                    ( { players | black = name }, BlackPlayer )

                            ( playerid, state2 ) =
                                ServerInterface.newPlayerid state

                            state3 =
                                ServerInterface.addPlayer playerid
                                    { gameid = gameid
                                    , player = player
                                    }
                                    state2

                            gameState2 =
                                { gameState | players = players2 }

                            state4 =
                                ServerInterface.updateGame gameid
                                    gameState2
                                    state3
                        in
                        ( { state4
                            | publicGames =
                                ServerInterface.removePublicGame
                                    gameid
                                    state4.publicGames
                          }
                        , Just <|
                            JoinRsp
                                { gameid = gameid
                                , playerid = Just playerid
                                , player = player
                                , gameState = gameState2
                                }
                        )

        LeaveReq { playerid } ->
            case ServerInterface.getPlayer playerid state of
                Nothing ->
                    errorRes message state "Unknown playerid"

                Just { gameid, player } ->
                    let
                        state2 =
                            ServerInterface.removeGame gameid state
                    in
                    ( state2
                    , Just <|
                        LeaveRsp
                            { gameid = gameid
                            , player = player
                            }
                    )

        UpdateReq { playerid } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, player ) ->
                    ( state
                    , Just <|
                        UpdateRsp
                            { gameid = gameid
                            , gameState = gameState
                            }
                    )

        PlayReq { playerid, placement } ->
            case lookupGame message playerid state of
                Err res ->
                    Debug.log "PlayReq Err" res

                Ok ( gameid, gameState, player ) ->
                    case Debug.log "PlayReq" placement of
                        ChooseNew newPlayer ->
                            case gameState.winner of
                                NoWinner ->
                                    errorRes message state "Game not over"

                                _ ->
                                    let
                                        players =
                                            if player == newPlayer then
                                                gameState.players

                                            else
                                                let
                                                    { white, black } =
                                                        gameState.players
                                                in
                                                { white = black
                                                , black = white
                                                }

                                        gs =
                                            emptyGameState players

                                        gs2 =
                                            { gs | score = gameState.score }

                                        state2 =
                                            if player == newPlayer then
                                                state

                                            else
                                                let
                                                    playerids =
                                                        ServerInterface.getGamePlayers
                                                            gameid
                                                            state

                                                    loop : PlayerId -> Types.ServerState -> Types.ServerState
                                                    loop id st =
                                                        let
                                                            pl =
                                                                if id == playerid then
                                                                    newPlayer

                                                                else
                                                                    Types.otherPlayer
                                                                        newPlayer
                                                        in
                                                        ServerInterface.updatePlayer
                                                            id
                                                            { gameid = gameid
                                                            , player = pl
                                                            }
                                                            st
                                                in
                                                List.foldl loop state playerids
                                    in
                                    ( ServerInterface.updateGame gameid gs2 state2
                                    , Just <|
                                        AnotherGameRsp
                                            { gameid = gameid
                                            , gameState = gs2
                                            , player = newPlayer
                                            }
                                    )

                        ChooseResign _ ->
                            case gameState.winner of
                                NoWinner ->
                                    let
                                        winner =
                                            case player of
                                                WhitePlayer ->
                                                    BlackWinner

                                                BlackPlayer ->
                                                    WhiteWinner

                                        gs =
                                            { gameState | winner = winner }
                                                |> updateScore
                                    in
                                    ( ServerInterface.updateGame gameid gs state
                                    , Just <|
                                        ResignRsp
                                            { gameid = gameid
                                            , gameState = gs
                                            , player = player
                                            }
                                    )

                                _ ->
                                    errorRes message state "Game already over"

                        ChoosePiece rowCol ->
                            let
                                board =
                                    gameState.newBoard

                                jumperLocations =
                                    gameState.jumperLocations
                            in
                            if gameState.undoStates /= [] then
                                errorRes message state "A jump sequence is in progress."

                            else
                                let
                                    piece =
                                        NewBoard.get rowCol board
                                in
                                if piece.pieceType == NoPiece then
                                    errorRes message state "No piece at chosen location."

                                else if not <| colorMatchesPlayer piece.color gameState.whoseTurn then
                                    errorRes message state "Chosen piece not of player's color."

                                else if
                                    (jumperLocations /= [])
                                        && (not <| List.member rowCol jumperLocations)
                                then
                                    errorRes message state "You must select a piece with a maximal jump sequence."

                                else
                                    let
                                        gs =
                                            { gameState
                                                | selected =
                                                    if gameState.selected == Just rowCol then
                                                        Nothing

                                                    else
                                                        Just rowCol
                                            }
                                                |> NewBoard.populateLegalMoves
                                    in
                                    ( ServerInterface.updateGame gameid gs state
                                    , Just <|
                                        PlayRsp
                                            { gameid = gameid
                                            , gameState = gs
                                            , decoration = NoDecoration
                                            }
                                    )

                        ChooseMove rowCol options ->
                            chooseMove state message gameid gameState player rowCol options

                        ChooseUndoJump undoWhichJumps ->
                            chooseUndoJump state message gameid gameState undoWhichJumps

        PublicGamesReq { subscribe, forName, gameid } ->
            -- subscribe is processed by the server code only
            let
                games =
                    Debug.log "publicGames" state.publicGames
                        |> List.filterMap ED.frameworkToPublicGame
                        |> List.filterMap
                            (\game ->
                                let
                                    matches =
                                        forNameMatches forName game.forName
                                            || (Maybe.withDefault "" gameid
                                                    == game.gameid
                                               )

                                    include =
                                        matches || game.forName == Nothing
                                in
                                if include then
                                    Just
                                        { game
                                            | forName =
                                                if matches then
                                                    game.forName

                                                else
                                                    Nothing
                                        }

                                else
                                    Nothing
                            )
            in
            ( state, Just <| PublicGamesRsp { games = games } )

        ChatReq { playerid, text } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, player ) ->
                    let
                        players =
                            gameState.players

                        name =
                            case player of
                                WhitePlayer ->
                                    players.white

                                BlackPlayer ->
                                    players.black
                    in
                    ( state
                    , Just <|
                        ChatRsp
                            { gameid = gameid
                            , name = name
                            , text = text
                            }
                    )

        _ ->
            errorRes message state "Received a non-request."


processChooseMoveOptions : List ChooseMoveOption -> RowCol -> Maybe ( RowCol, Piece ) -> GameState -> ( GameState, Maybe String )
processChooseMoveOptions options moveTo jumpOver gameState =
    let
        processOption option ( gs, err ) =
            let
                board =
                    gs.newBoard
            in
            case err of
                Just _ ->
                    ( gameState, err )

                Nothing ->
                    case option of
                        CorruptJumped ->
                            case jumpOver of
                                Nothing ->
                                    ( gameState, Just "Can't corrupt a jumped piece on a non-jump." )

                                Just ( jumpedPos, piece ) ->
                                    let
                                        { pieceType, color } =
                                            piece
                                    in
                                    if pieceType == Golem || pieceType == Hulk then
                                        let
                                            otherColor =
                                                Types.otherColor color
                                        in
                                        if colorMatchesPlayer color gameState.whoseTurn then
                                            ( gameState, Just "Can't corrupt a piece of your own color." )

                                        else if
                                            (pieceType == Golem)
                                                && (NewBoard.countColor otherColor board >= 23)
                                        then
                                            -- This can happen, but I'll be it won't
                                            -- Sorta like the two-move mate.
                                            ( gameState, Just "There are no pieces to use to make a corrupted hulk." )

                                        else
                                            let
                                                corruptedHulk =
                                                    { pieceType = CorruptedHulk
                                                    , color = Types.otherColor color
                                                    }

                                                newBoard =
                                                    NewBoard.set jumpedPos corruptedHulk board
                                            in
                                            ( { gs | newBoard = newBoard }, Nothing )

                                    else
                                        ( gameState, Just "Can't corrupt a Journeyman or a Corrupted Hulk" )

                        MakeHulk hulkPos ->
                            let
                                { pieceType, color } =
                                    NewBoard.get moveTo board

                                hulkPiece =
                                    NewBoard.get hulkPos board
                            in
                            if
                                ((color == WhiteColor) && (moveTo /= NewBoard.blackSanctum))
                                    || ((color == BlackColor) && (moveTo /= NewBoard.whiteSanctum))
                            then
                                ( gameState, Just "Can't make a hulk except by landing on the other player's sanctum." )

                            else if pieceType == Golem || pieceType == Hulk then
                                if not <| colorMatchesPlayer color gs.whoseTurn then
                                    ( gameState, Just "Can't convert another player's piece to a hulk." )

                                else if moveTo == hulkPos then
                                    ( gameState, Just "Can't convert the jumper to a hulk." )

                                else
                                    let
                                        hulk =
                                            { pieceType = Hulk
                                            , color = color
                                            }

                                        newBoard =
                                            NewBoard.clear moveTo board
                                                |> NewBoard.set hulkPos hulk
                                    in
                                    ( { gs | newBoard = newBoard }, Nothing )

                            else
                                ( gameState, Just "Can't make a Hulk from a Journeyman or Corrupted Hulk." )
    in
    List.foldl processOption ( gameState, Nothing ) options



-- TODO: Call processChooseOptions


chooseMove : Types.ServerState -> Message -> String -> GameState -> Player -> RowCol -> List ChooseMoveOption -> ( Types.ServerState, Maybe Message )
chooseMove state message gameid gameState player rowCol options =
    let
        board =
            gameState.newBoard

        rowcol2 =
            Debug.log "chooseMove" rowCol
    in
    case gameState.selected of
        Nothing ->
            errorRes message state "No selected piece."

        Just selected ->
            let
                legalMoves =
                    gameState.legalMoves

                piece =
                    NewBoard.get selected board
            in
            case legalMoves of
                Moves rowCols ->
                    if not <| List.member rowCol rowCols then
                        errorRes message state "Not a legal move."

                    else
                        let
                            gs =
                                endOfTurn selected rowCol piece gameState
                        in
                        ( ServerInterface.updateGame gameid gs state
                        , Just <|
                            PlayRsp
                                { gameid = gameid
                                , gameState = gs
                                , decoration = NoDecoration
                                }
                        )

                Jumps sequences ->
                    let
                        remaining =
                            List.filter (isFirstJumpTo rowCol) sequences

                        newSequences =
                            List.map (List.drop 1) remaining
                    in
                    case remaining of
                        [] ->
                            errorRes message state "Not a legal jump."

                        firstSequence :: _ ->
                            case List.head newSequences of
                                Nothing ->
                                    -- Can't happen
                                    errorRes message state "No jump sequences."

                                Just [] ->
                                    -- End of jumps
                                    let
                                        doJump jump board2 =
                                            NewBoard.set jump.over Types.emptyPiece board2

                                        jumps =
                                            case List.head firstSequence of
                                                Nothing ->
                                                    -- can't happen
                                                    gameState.jumps

                                                Just jump ->
                                                    jump :: gameState.jumps

                                        newBoard =
                                            List.foldr doJump board jumps

                                        gs =
                                            endOfTurn selected
                                                rowCol
                                                piece
                                                { gameState | newBoard = newBoard }
                                    in
                                    ( ServerInterface.updateGame gameid gs state
                                    , Just <|
                                        PlayRsp
                                            { gameid = gameid
                                            , gameState = gs
                                            , decoration = NoDecoration
                                            }
                                    )

                                _ ->
                                    let
                                        gs =
                                            { gameState
                                                | newBoard =
                                                    NewBoard.set selected Types.emptyPiece board
                                                        |> NewBoard.set rowCol piece
                                                , selected = Just rowCol
                                                , legalMoves = Jumps newSequences
                                                , undoStates =
                                                    { board = board
                                                    , moves = gameState.moves
                                                    , legalMoves = legalMoves
                                                    , selected = gameState.selected
                                                    }
                                                        :: gameState.undoStates
                                                , jumps =
                                                    List.take 1 firstSequence
                                                        ++ gameState.jumps
                                            }
                                    in
                                    ( ServerInterface.updateGame gameid gs state
                                    , Just <|
                                        PlayRsp
                                            { gameid = gameid
                                            , gameState = gs
                                            , decoration = NoDecoration
                                            }
                                    )


endOfTurn : RowCol -> RowCol -> Piece -> GameState -> GameState
endOfTurn selected moved piece gameState =
    let
        whoseTurn =
            Types.otherPlayer gameState.whoseTurn

        whoseTurnColor =
            Types.playerColor whoseTurn

        newBoard =
            NewBoard.set selected Types.emptyPiece gameState.newBoard
                |> NewBoard.set moved piece

        jumperLocations =
            NewBoard.computeJumperLocations whoseTurnColor newBoard
    in
    { gameState
        | newBoard = newBoard
        , selected = Nothing
        , jumperLocations = jumperLocations
        , legalMoves = Moves []
        , whoseTurn = whoseTurn
        , undoStates = []
        , jumps = []

        -- TODO
        , moves = gameState.moves
        , winner = NoWinner
    }
        |> updateScore


chooseUndoJump : Types.ServerState -> Message -> String -> GameState -> UndoWhichJumps -> ( Types.ServerState, Maybe Message )
chooseUndoJump state message gameid gameState undoWhichJumps =
    case undoWhichJumps of
        UndoAllJumps ->
            let
                dropCnt =
                    List.length gameState.undoStates - 1

                gs =
                    { gameState
                        | undoStates =
                            List.drop dropCnt gameState.undoStates
                        , jumps =
                            List.drop dropCnt gameState.jumps
                    }
            in
            chooseUndoJump state message gameid gs UndoOneJump

        UndoOneJump ->
            case gameState.undoStates of
                [] ->
                    errorRes message state "There is nothing to undo."

                undoState :: undoStates ->
                    let
                        gs =
                            { gameState
                                | newBoard = undoState.board
                                , moves = undoState.moves
                                , selected = undoState.selected
                                , legalMoves = undoState.legalMoves
                                , undoStates = undoStates
                                , jumps = List.drop 1 gameState.jumps
                            }
                    in
                    ( ServerInterface.updateGame gameid gs state
                    , Just <|
                        PlayRsp
                            { gameid = gameid
                            , gameState = gs
                            , decoration = NoDecoration
                            }
                    )


isFirstJumpTo : RowCol -> JumpSequence -> Bool
isFirstJumpTo rowCol sequence =
    case List.head sequence of
        Nothing ->
            False

        Just oneJump ->
            oneJump.to == rowCol


colorMatchesPlayer : Color -> Player -> Bool
colorMatchesPlayer color player =
    if color == WhiteColor then
        player == WhitePlayer

    else
        player == BlackPlayer


doPlay : Decoration -> GameId -> GameState -> Types.ServerState -> ( Types.ServerState, Maybe Message )
doPlay decoration gameid gameState state =
    let
        whoseTurn =
            gameState.whoseTurn

        private =
            gameState.private

        board =
            gameState.board

        moves =
            gameState.moves

        ( ( newDecoration, newBoard, newWhoseTurn ), ( newWinner, newPath, newMoves ) ) =
            ( ( decoration, board, whoseTurn ), ( NoWinner, [], moves ) )

        gs =
            { gameState
                | board = newBoard
                , moves = newMoves
                , whoseTurn = newWhoseTurn
                , winner = newWinner
                , path = newPath
                , private = { private | decoration = newDecoration }
            }
                |> updateScore

        state2 =
            ServerInterface.updateGame gameid gs state
    in
    case newWinner of
        NoWinner ->
            ( state2
            , Just <|
                PlayRsp
                    { gameid = gameid
                    , gameState = gs
                    , decoration = newDecoration
                    }
            )

        _ ->
            ( state2
            , Just <|
                GameOverRsp
                    { gameid = gameid
                    , gameState = gs
                    }
            )


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    Board.colToString colidx ++ Board.rowToString rowidx


updateScore : GameState -> GameState
updateScore gameState =
    let
        score =
            gameState.score

        names =
            gameState.players

        winnerName =
            case gameState.winner of
                NoWinner ->
                    ""

                WhiteWinner ->
                    names.white

                BlackWinner ->
                    names.black
    in
    if gameState.winner == NoWinner then
        gameState

    else
        let
            oneScore =
                case Dict.get winnerName score of
                    Nothing ->
                        Types.zeroOneScore

                    Just one ->
                        one
        in
        { gameState
            | score =
                Dict.insert winnerName
                    { oneScore
                        | games = oneScore.games + 1
                        , score = oneScore.score + Board.score gameState.board
                    }
                    score
        }
