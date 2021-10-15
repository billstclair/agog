# AGOG TODO

1. S3 persistence
1. If you're watching a game in a background session, and it has ended
   since you reloaded the game, the message is confusing.
   Messages for games that are not on-screen should go somewhere else,
   or be dropped on the floor.
   It would be nice to remember that the background game is not active,
   but I think it will have to just remain unconnected for now.
   This only happens when the server gets restarted.
1. Chat check-box for initial public game. Button to turn it on and off
   while playing.
1. Put the archive in the GameState, not the client-only Game.
   Restore the public state of a game, when the server needs reminding,
   so that it will return to the public games list for watchers.
   Not the GameState. Somewhere else, but persisted on the server.
1. Background session notification needs work. It should have its own
   `select` item to choose a session with activity since you were
   last there.
   * Proper OS notifications, which bring the session responsible to
     the front when clicked.
1. Periodic "Update" commands sent over wire. In case we miss a message.
   This bug is actually due to web sockets connections getting confused.
   Hard to debug, since I don't know how to reproduce it.
1. Store version in app and web site. Periodically compare and notify if
   updated code available.
1. "-- New Session --" should be able to copy all session state except connection.
1. URLs for the pages, so you can send someone directly to one of them.
   Make them do the right thing from the Markdown in Agog.Documentation.
1. ErrorRsp should explicitly encode the xxxReq it came from,
   and include a real Error field, so that code can respond intelligently.
   Include backward compatibility, a StringError alternative for Error.
1. Initial position on new network game, from Test Mode and/or a saved game
1. New network session can reproduce entire session, just current
   gamestate, or nothing.
1. Handicapping
1. Game review can be shared in a network session
1. Automatic dark mode. User sets times. Default to 7am-8pm light

## Maybe

1. Custom color schemes
