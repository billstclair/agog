# AGOG TODO

1. Public sessions stay in the public games list until they end.
   Spectators may join from there.
   Separate table for active games.
   Chat check-box for initial public game. Button to turn it on and off
   while playing.
   Indication of how many spectators there are.
1. Background session notification needs work. It should have its own
   `select` item to choose a session with activity since you were
   last there.
1. Public games are not subscribed if you start up on that page
   (same for Moves page?)
1. Multiple simultaneous games: alerts for activity in off-screen games.
   Click brings off-screen game to the screen.
1. S3 persistence
1. Periodic "Update" commands sent over wire. In case we miss a message.
1. Store version in app and web site. Periodically compare and notify if
   updated code available.
1. "-- New Session --" should be able to copy all session state except connection.
1. URLs for the pages, so you can send someone directly to one of them.
   Make them do the right thing from the Markdown in Agog.Documentation.
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
