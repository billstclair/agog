# AGOG TODO

1. Multiple simultaneous games: alerts for activity in off-screen games.
   Click goes to off-screen game.
1. S3 persistence
1. Watchers, can chat, but don't play.
1. Periodic "Update" commands sent over wire. In case we miss a message.
1. Store version in app and web site. Periodically compare and notify if
   updated code available.
1. "-- New Session --" copies all session state except connection.
1. ErrorRsp should explicitly encode the xxxReq it came from,
   and include a real Error field, so that code can respond intelligently.
   Include backward compatibility, a StringError alternative for Error.
1. Initial position on new network game, from Test Mode and/or a saved game
1. New network session can reproduce entire session, just current gamestate, or nothing.
1. Handicapping
1. Game review can be shared in a network session
1. Automatic dark mode. User sets times. Default to 7am-8pm light

## Maybe

1. Custom color schemes
