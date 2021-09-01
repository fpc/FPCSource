# Websocket examples

This directory contains the examples for the websockets functionality of
FCL-Web

## server

Small standalone chat server. It accepts JSON messages and uses from/to/msg fields in
the json objects to dispatch messages. If no to: is present, a message is
distributed to all connected clients.

## client
A small command-line chat client to connect to the server.
It will ask for a name, and then starts a loop in which you can enter
one-line messages that will be sent to the server.

## Upgrade
A demonstration of the HTTP server "upgrade" mechanism; 
This chat server not only acts as the server example, but also serves files on the same port.

A sample client can also be found in the pas2js sources, which is a sample
client program that works in the browser. (see the demo/websocket folder)

## Running the examples
All programs print help when invoked with the -h command-line parameter.

To test the examples, run the server in a terminal window:

```sh
wsserver -p 8080
```

(will start listening on  port 8080)

In another terminal, run

```sh
wsclient -u ws://localhost:8080/
```

You can run this program multiple times in different terminal windows.