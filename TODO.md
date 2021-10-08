# AGOG TODO

1. Periodic "Update" commands sent over wire. In case we miss a message.
1. "Request Undo" button. If opponent approves, undoes the move.
1. Save moves from all games in session
   Done. Need a UI for viewing the archives.
1. "-- New Game --" copies all game state except connection.
1. ErrorRsp should explicitly encode the xxxReq it came from,
   and include a real Error field, so that code can respond intelligently.
   Include backward compatibility, a StringError alternative for Error.
1. Game replay, play/pause/rewind/step/goto move
1. Game review is shared in a network session
1. Multiple simultaneous games: alerts for activity in off-screen games.
   Click goes to off-screen game.
1. Initial position on new network game, from Test Mode and/or a saved game
1. New network session can reproduce entire session, just current gamestate, or nothing.
1. Handicapping
1. S3 persistence
1. Watchers, can chat, but don't play.
1. Automatic dark mode. User sets times. Default to 7am-8pm light

## Maybe

1. Custom color schemes
