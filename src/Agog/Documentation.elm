--------------------------------------------------------------------
--
-- Documentation.elm
-- In-game documentation for AGOG
-- Runs on local machine for local play, and server for networked play.
-- Copyright (c) 2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Agog.Documentation exposing (instructions, playButtonHtml, rules, rulesDiv)

import Html exposing (Html, button, div, h2, h3, text)
import Html.Attributes exposing (align, style)
import Html.Events exposing (onClick)
import Markdown


br : Html msg
br =
    Html.br [] []


rulesDiv : Bool -> List (Html msg) -> Html msg
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


playButtonHtml : msg -> Html msg
playButtonHtml setHomePageMsg =
    rulesDiv True
        [ button
            [ onClick setHomePageMsg
            , style "font-size" "110%"
            ]
            [ text "Play" ]
        ]


maybeFooterParagraph : Maybe (Html msg) -> Html msg
maybeFooterParagraph maybeFooter =
    case maybeFooter of
        Nothing ->
            text ""

        Just footerParagraph ->
            rulesDiv True
                [ footerParagraph ]


rules : msg -> Maybe (Html msg) -> Html msg
rules setHomePageMsg maybeFooter =
    let
        playButton =
            playButtonHtml setHomePageMsg
    in
    rulesDiv False
        [ rulesDiv False
            [ h2 [ align "center" ]
                [ text "Rules" ]
            , playButton
            , h3 [ align "center" ]
                [ text "STORY" ]
            , Markdown.toHtml [] """ 
_In a secret complex deep in the mountains, the Ancient Guild of Golemancy plies its trade, creating life where before was only base matter. Two journeyman golemancers feverishly prepare their master works, for there is but one position available among the Golemancery Elect. Both know that if they could but obtain their rival’s research, they would surely be able to create the greater work. The night before their creations are to be judged, each plots to infiltrate the other’s sanctum of knowledge, using their legion of early prototypes to subtly outmaneuver the other._
                                  
_In __A•G•O•G__, players attempt to be first reach their opponent’s sanctum, or failing that, to take their opponent captive and in so doing keep them from completing their masterpiece. If one of your golems manages to enter their sanctum, they may bring you back some sundry inventions with which to better equip your remaining creations, but your eyes alone will know what to look for within their volumes of research. Will you have what it takes to outwit your rival and take your place among the Masters of Golemancy?_
"""
            ]
        , playButton
        , rulesDiv False
            [ h3 [ align "center" ]
                [ text "SETUP" ]
            , Markdown.toHtml [] """
__A•G•O•G__ is a game of subtle strategy for two players. Played on a standard 8x8 checkers board, players sit at opposite dark-colored corner squares. The square closest to each player is their "sanctum." One player’s units are white and the other’s are black.
                                  
Each player starts with 20 golems, shaped like checkers pieces, and 1 journeyman golemancer (journeyman for short), comprised of a stack of 3 checkers: white-black-white for White; black-white-black for Black. The journeyman starts on the player’s sanctum with the golems filling the 5 rows of squares in front of them, leaving the three middle rows unoccupied between the two players’ ranks.
                                  
White goes first. On their turn, the acting player must move one unit or make a capture with one unit. Players alternate turns until one of them is victorious.
"""
            ]
        , playButton
        , rulesDiv False
            [ h3 [ align "center" ]
                [ text "UNITS" ]
            , Markdown.toHtml [] """
All unit movement in __A•G•O•G__ is orthogonal relative to the squares; diagonal relative to the players. Units can only move forward (towards the enemy sanctum), but can capture enemy units forward or backward.
                                  
Units capture by jumping over an enemy unit and landing on an unoccupied square beyond them. Units cannot jump over more than one enemy at a time, and cannot move or jump off the edge of the board. If a unit making a capture lands on a square from which they can make another capture, they are compelled to do so and continue the capturing sequence until they land on a square from which no further captures are available. Captured units are immediately removed from play. All jumped units remain on the board until the sequence is complete, and no unit can be jumped twice in the sequence.
                                  
If a capture is available on the acting player’s turn, they are compelled to take it instead of moving a unit, and if multiple captures are available, they are compelled to take the capturing sequence that will capture the most enemy units. For example, if the acting player could either capture two enemy golems or capture the enemy journeyman alone, they are compelled to capture the two golems even though capturing the journeyman would win them the game.
                                  
__The Golem:__ The basic unit of __A•G•O•G__. Golems can move forward one square at a time and can capture only adjacent enemy units, landing on the square immediately beyond them.
                                  
__The Hulk:__ If a golem ends the turn placed on the enemy sanctum, its player may choose either to place it atop any of their other golems or remove it from play. If the player chooses the former, the two stacked golems become a new unit: the hulk.
                                  
Augmented with stolen inventions from the enemy sanctum, the hulk is the most powerful unit in the game. They can move forward any distance in a straight line, and can capture at any distance in a straight line, landing on any unoccupied square beyond the captured unit (provided they do do not jump over more than one unit at a time and that they land such that they can capture the most possible enemy units in the sequence).
                                  
As with a golem, if a hulk ends the turn placed on the enemy sanctum, its player may either create a new hulk with the top golem of the stack and remove the bottom from play, or may remove both stacked golems from play. If they have no other golems in play when one of their units ends the turn on the enemy sanctum, that unit cannot promote to a hulk and must be removed from play.
                                  
__The Journeyman:__ The most important unit. If captured, their player loses the game, whereas if a journeyman ends the turn on the enemy sanctum, their player wins the game.
                                  
The journeymen move and capture the same as golems. However, when jumping over an enemy unit, they may choose to corrupt the jumped unit instead of capturing it, changing it into a “corrupted hulk”. To do so, the acting player places one of their golems that has been removed from play atop the jumped unit. This new corrupted hulk is now under the control of the player that corrupted it. If an enemy hulk is jumped by the acting player’s journeyman, it can be corrupted by removing the top stacked enemy golem and replacing it with one of the corrupting player’s that has been removed from play. If the acting player does not currently have any golems that have been removed from play, they cannot corrupt units with the journeyman and must capture instead.
                                  
Corrupted hulks behave the same as regular hulks of their color, including being able to promote to regular hulks if they end the turn on the enemy sanctum (in this scenario, the acting player discards the bottom stacked enemy golem from play). However, a corrupted hulk cannot be corrupted again and must be captured if jumped by the enemy journeyman.
"""
            ]
        , playButton
        , rulesDiv False
            [ h3 [ align "center" ]
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
            [ h3 [ align "center" ]
                [ text "SCOREKEEPING" ]
            , Markdown.toHtml [] """
If desired, players may keep a game record while playing. Each pair of turns for White and Black is denoted with a numbered move, with White’s turn recorded on the left and Black’s on the right. The board is labelled with coordinates the same as a chess board, with the squares to White’s right labelled alphabetically "a" through "h" and the squares to White’s left labelled numerically "1" through "8". The units and actions are labelled as follows:
                                  
* The journeyman is denoted as "J".
* Standard hulks are denoted as "H".
* Corrupted hulks are denoted as "C".
* Golems require no unit label, and their movement is indicated by the coordinates alone.
                                  
When a golem moves, it is often only necessary to record the coordinates of the square they moved to. If two golems could move to the same square, record the distinguishing row or column from which the golem moved before the coordinates. For example, an isolated golem moving from a6 to b6 would be recorded as "b6", but if there was a another golem of the same color on b5, it would be necessary to record "ab6" to clarify which golem was moving.
                                  
When another unit moves, record the unit and the coordinates of the square they moved to. As with golems, it is not necessary to record the row or column from which they moved unless two or more of the same unit could have moved to that square. For example, if corrupted hulk is on a6 and another is on b5, the corrupted hulk moving from b5 to b6 would be recorded as "C5b6".
                                  
When a unit makes a capture, record "x" between the unit label and the coordinates of the square they jumped to (when capturing with a golem, always record its starting row or column before the x). To record a journeyman creating a corrupted hulk, record "+" instead. For example, a journeyman making a capturing sequence from a1 to a3 to c3, capturing the first unit and creating a corrupted hulk from the second, would be recorded as "Jxa3+c3".

When a unit ends its move on the enemy sanctum and the acting player chooses to create a hulk, record the coordinates of the promoted unit and "=H". If the acting player chooses to remove the unit from play instead, no additional record is needed after the final coordinates. For example, a corrupted hulk moving from a3 to a1 and then promoting a golem on h1 would be recorded "Ca1 h1=H".

If White wins, record "1-0". If Black wins, record "0-1".
"""
            ]
        , playButton
        , rulesDiv False
            [ h3 [ align "center" ]
                [ text "HANDICAPPING" ]
            , Markdown.toHtml [] """
If players of differing skill levels wish to play an even game, the stronger player can be given a handicap by removing 1 or more of their golems from the board before play starts. The handicapped player always plays Black, and the golems must be removed such that the remaining forces are symmetrically arranged and are removed from the squares closest to the center of the board. For example, for a handicap of 1 remove the golem on f6; for a handicap of 2, remove the golems on e6 and f5; for a handicap of 3 remove the golems on e6, f5, and f6, etc. If the weaker player wins the majority of their games at a particular handicap, reduce the handicap by 1.
"""
            ]
        , playButton
        , maybeFooterParagraph maybeFooter
        ]


instructions : msg -> Maybe (Html msg) -> Html msg
instructions setHomePageMsg maybeFooter =
    let
        playButton =
            playButtonHtml setHomePageMsg
    in
    rulesDiv False
        [ rulesDiv False
            [ h2 [ align "center" ]
                [ text "Instructions" ]
            , playButton
            , rulesDiv False
                [ h3 [ align "center" ]
                    [ text "GENERAL" ]
                , Markdown.toHtml [] """
The green text below the __A•G•O•G__ board is instructions about what you are expected to do (or wait for). If there was a problem, an error message will appear in red between the board and the green line.

At night, you may prefer to display the screen in "Dark Mode". Check that box to do that. Uncheck to return to the default appearance.

You may choose to rotate the board by 180 degrees, if you're playing black, (or black has the move in local mode). Do so by choosing the "player down" radio button instead of "white down" for "Rotate".

If notifications are available in your browser (and they currently are NOT available in mobile browsers), there will be a "Notifications" check-box. When you check it for the first time, your browser will ask you whether to allow notifications. If you confirm, then notifications will be shown when your web browser tab is not current, and the other player does something that you should know about (e.g. it's now your turn). If the checkbox is unchecked and disabled, that means that you said not to allow notifications the last time you were asked. You must use your browser's settings to re-enable this.

If you check the "Sound" checkbox, you will hear a sound when pieces move. The sound for an intermediate jump in a sequence is different from the move or last jump sound. In iOS, playing this sound will turn off music; choose accordingly.

Your game state is remembered in your browser's "Local Storage" database. If you refresh the screen with a game in progress, it will attempt to reconnect to the server and resume the game.

Sometimes, when I'm uncareful about an update, your game may get wedged. If it does that, "Resign" from an existing game, check the "Local" check-box, click the "Clear Storage!" button, twice, and hopefully you'll be back in business (after unchecking "Local").
"""
                ]
            , playButton
            , rulesDiv False
                [ h3 [ align "center" ]
                    [ text "SERVER CONNECTION" ]
                , Markdown.toHtml [] """
This implementation of __A•G•O•G__ is a networked, two-player game. It communicates through a server, running on agog.ninja.

Before you can play games, you need to connect through that server to your opponent. Do this by entering "Your Name" and clicking "Start Game". This will display a "Game ID". Copy that, and text it to your opponent. The other player fills in "Your Name", pastes the Game ID, and clicks "Join". The initiating player is white initially. Colors swap with each subsequent game.

Alternatively, the initiating player can check the "Public" box before clicking "Start Game". This makes the game appear on the "Public" page (available via the link at the bottom of each page). If you fill in "for name" before clicking "Start Game", then only other players with the given "Name" will see it.

To join a public game, enter "Your Name", go to the "Public" page, and click on the underlined GameId.
"""
                ]
            , playButton
            , rulesDiv False
                [ h3 [ align "center" ]
                    [ text "GAME PLAY" ]
                , Markdown.toHtml [] """
When it's your turn, click on the piece you want to move, then click on the empty square you want to move it to. When you have pieces that can jump, they will be highlighted, and you will only be able to move one of them. When you have jumps, the jump destinations will be highlighted, until you've made all the jumps you can.

If you move a golem, hulk, or corrupted hulk into the other player's sanctum, you will be asked whether you want to convert it to a hulk. You may do that by clicking on one of your golems. You may skip the conversion by clicking on an empty square.

If your journeyman jumps a golem or hulk, you will be asked whether to "Corrupt Jumped Piece". Click the "Yes" button to do that or the "No" button to remove the jumped piece from play.

You may resign on your turn by clicking the "Resign" button.

When the game is over, either player may click the "New Game" button to start another game, with the white/black assignments reversed.
"""
                ]
            , playButton
            , rulesDiv False
                [ h3 [ align "center" ]
                    [ text "MOVE RECORDING" ]
                , Markdown.toHtml [] """
The previous four moves will be shown in the "Moves" line. They are encoded with ALL the information, more verbose than the standard move encoding described in the "SCOREKEEPING" section of the "Rules" page. If the move begins with an "n", then it is NOT unique, meaning that another piece could have moved to the same destination.

A move is encoded with a letter for the piece, the from location, a dash, and the to location. White pieces get upper-case letters and black pieces get lower-case letters. For example, an opening move of a white golem from d3 to d4 is encoded as nGd3-d4.

A jump is encoded with a letter for the piece, a from location, an x or +, for jump and remove or jump and convert to corrupted hulk, and a to location. Multiple jumps are denoted by more pairs of x or + and destionation location. For example, if the black journeyman on e6 jumps white golems on e5, e3, d2, and b2, converting the one on e5 to a corrupted hulk, the move will be represented as je6+e4xe2xc2xa2.

The final move of the game ends with a win indicator, this is the "%" character, followed by W or B for white or black winning, followed by C, S, I, or R, for win by Capture, Sanctum, Immobilization, or Resignation.

Click on the underlined "Moves" heading to go to a page which displays the game's moves in standard two-column format.
"""
                ]
            , playButton
            , rulesDiv False
                [ h3 [ align "center" ]
                    [ text "LOCAL MODE" ]
                , Markdown.toHtml [] """
Sometimes you want to play a game locally, without going through the server (except to fetch the HTML and JavaScript for the game itself). You may do this by checking the "Local" checkbox between games. You will then control both players.

Local mode has another feature, "Test Mode". Enable that by checking the "Test Mode" check box. This provides "Erase Board!" and "Initial Setup" buttons, a "Remove clicked" checkbox, which, when checked, causes a click on a piece to remove it from the board, a "Test piece" popup, which allows you to choose a piece type, and "White" & "Black" radio buttons, which choose the piece color. When you click on an empty square on the board, that piece will appear there. Unchecking "Test mode" returns to game play.
"""
                , playButton
                , rulesDiv False
                    [ h3 [ align "center" ]
                        [ text "STATISTICS" ]
                    , Markdown.toHtml [] """
The __A•G•O•G__ server keeps track of some statistics about games. These are shown on the "Statistics" page, available from the "Statistics" link at the bottom of the page. It displays a table containing counts for the following:

1. Finished Games
1. Finished Game Total Moves
1. White Won
1. Black Won
1. Active Games
1. Total Sessions
1. Total Public Sessions
1. Active Sessions

"Finished Games" is the number of games that have been completed. "Finished Game Total Moves" is the sum of the number of moves in each of the finished games. "White Won" and "Black Won" are the number of times that white or black won a finished game. These will be missing if the server was just started and no games have been completed yet.

If "Finished Games" is greater than 0, then a "White Wins" percentage and an "Average moves/game" ratio are computed and displayed below the table.

"Active Games" is the number of games that are currently being played.

"Total Sessions" is the total number of sessions that have been initiated, whether a second player ever joined or not.

"Total Public Sessions" is the number of the sessions that were created as public games.

"Active Sessions" is the number of currently active sessions. It will be less than "Active Games" when some of the sessions are between games.

The statistics page currently shows only statistics since the last time the server was started.
"""
                    ]
                ]
            , playButton
            ]
        , maybeFooterParagraph maybeFooter
        ]
