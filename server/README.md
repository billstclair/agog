This directory is for uploading to the web server machine for the AGOG server.

The server runs in `Node.js`.

You build the Elm part of the server from the top-level `agog` directory:

* cd .../agog
* bin/build-server

Installing the server JavaScript libraries:

One time:

* `cd ...agog/server   # this directory`
* `npm install`

To start the server:

* `npm run start`

Normally, the server is silent except for notifying you of the port it's listening on. If you want to see verbose messages, invoke it with:

* `VERBOSE=yes npm run start`

If you want to poke around in the node environment, you can set the REPL environment variable:

* `REPL=yes npm run start`

`exit()` will exit back to the command line from the REPL. If you don't start a repl, then type Ctrl-C to exit.

If you do not want to automatically start the web server, set the NOLISTEN environment variable. You can then start listening by typing `listen()` in the REPL, or `listen(port)`, if you want to listen on something other than the defaulted PORT.

If your web server automatically upgrades to HTTPS, or you prefer to leave off the ":8081" from the Server URL, you'll need to proxy to get to the non-encrypted websocket server. Do this by installing Apache `mod_proxy_wstunnel`:

    $ sudo a2enmod proxy_wstunnel
    $ sudo service apache2 restart

Then add to either your Apache virtual host configuration or to an `.htaccess` file, the following:

    ProxyPass "/my-server"  "ws://localhost:8081/"
    
`/my-server` has to match the contents of `site/server.txt`, from which the client loads the server default.

If you're running the server on your local machine, you can aim your browser at:

    http://localhost:8081
    
to get a very simple test client (`Agog.Server.Client`) that sends the strings you type over the wire and prints what it receives back.

During development, when you're running both the Agog webapp and the server on your local machine, you should connect to:

    ws://localhost:8081
    
unless you use PORT as described below to change the port, or are running it on a remote server.

If you want to run your server on a port other than 8081, you can set the `PORT` environment variable:

* `PORT=8800 npm run start`

## [Grokking](https://en.wikipedia.org/wiki/Grok) the code

Agog is based on [billstclair/elm-websocket-framework](https://package.elm-lang.org/packages/billstclair/elm-websocket-framework/latest/) and [billstclair/elm-websocket-framework-server](https://package.elm-lang.org/packages/billstclair/elm-websocket-framework-server/latest/). You'll need to understand at least the API for those before you can grok Agog's client/server communication.

Client and server communicate in JSON over a WebSocket connection. You can see the JSON by checking the "Show protocol" check box below the board.

`bin/build-server` creates the files `server/server.js` from [`src/Agog/Server/Server.elm`](https://github.com/billstclair/agog/blob/main/src/Agog/Server/Server.elm) and `server/client.js` from [`src/Agog/Server/Client.elm`](https://github.com/billstclair/agog/blob/main/src/Agog/Server/Client.elm). `server.js` is where most of the server code lives. You can see it on the Agog server at [http://agog.ninja:8084/server.js](http://agog.ninja:8084/server.js). `npm run` looks at [`server/package.json`](https://github.com/billstclair/agog/blob/main/server/package.json) to discover that [`server/index.js`](https://github.com/billstclair/agog/blob/main/server/index.js) is the main code for the server. `index.js` loads the WebSocket server code in [`server/WebSocketServer.js`](https://github.com/billstclair/agog/blob/main/server/WebSocketServer.js) and [`server/lib/WebSocketServer.js`](https://github.com/billstclair/agog/blob/main/server/lib/WebSocketServer.js), starts the elm code at `Elm.Agog.Server.Server.init` in `server/server.js`, then starts listening for HTTP (and WebSocket) connections.

[`src/Agog/Server/Server.elm`](https://github.com/billstclair/agog/blob/main/src/Agog/Server/Server.elm) is a headless Elm app via `WebSocketFramework.Server.program`, which is a simple wrapper of `Platform.worker` in the [`Platform`](https://package.elm-lang.org/packages/elm/core/latest/Platform) module in `elm/core`.

