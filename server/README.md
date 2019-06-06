This directory is for uploading to the web server machine for the Zephyrnot server.

The server runs in `Node.js`.

You build the Elm part of the server from the top-level `zephyrnot` directory:

* cd .../zephyrnot
* bin/build-server

Installing the server JavaScript libraries:

One time:

* `cd ...zephyrnot/server   # this directory`
* `npm install`

To start the server:

* `npm run start`

Normally, the server is silent except for notifying you of the port it's listening on. If you want to see verbose messages, invoke it with:

* `VERBOSE=yes npm run start`

If your web server automatically upgrades to HTTPS, or you prefer to leave off the ":8081" from the Server URL, you'll need to proxy to get to the non-encrypted websocket server. Do this by installing Apache `mod_proxy_wstunnel`:

    $ sudo a2enmod proxy_wstunnel
    $ sudo service apache2 restart

Then add to either your Apache virtual host configuration or to an `.htaccess` file, the following:

    ProxyPass "/my-server"  "ws://localhost:8081/"
    
`/my-server` has to match the contents of `site/server.txt`, from which the client loads the server default.

If you're running the server on your local machine, you can aim your browser at:

    http://localhost:8081
    
to get a very simple test client (`Zephyrnot.Server.Client`) that sends the strings you type over the wire and prints what it receives back.

During development, when you're running both the Zephyrnot webapp and the server on your local machine, you should connect to:

    ws://localhost:8081
    
unless you use PORT as described below to change the port, or are running it on a remove server.

If you want to run your server on a port other than 8081, you can set the `PORT` environment variable:

* `PORT=8800 npm run start`
