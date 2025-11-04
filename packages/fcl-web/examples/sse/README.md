# Server-Sent events demo

The testsse demo shows how to use HTTP server-sent events. The testsse.lpr
project can function as a client (receiver) or as a source (server) of
events.

## Server demo
To start the demo as a server, run
```shell
testsse -s
```
optionally, you can set a port with option `-p`:
```shell
testsse -s -p 3000
```

You can test the raw output with wget (or curl) like so:

```shell
wget --header="Accept: text/event-stream" "http://localhost:8080/events" -q -O -
```
The server understands the 'Last-Event-Id' header as specified in the spec,
and will adapt the starting event accordingly:
```shell
wget --header="Accept: text/event-stream" --header="Last-Event-Id: 5" "http://localhost:8080/events" -q -O -
```

## Client demo
To test the client, start the server (as described above) in one terminal
window, and in another terminal window, start the client:
To start the demo as a server, run
```shell
testsse -c
```
optionally, you can set the server port with option `-p`:
```shell
testsse -c -p 3000
```
optionally, you can set the last received ID with option `-l`:
```shell
testsse -c -l 5
```
The server will then start with event 6.

You can also try to capture the event stream of another server than the
testsse server. Specify the `u` or `url` option:
```shell
testsse -c -u http://example.com/some-events-resource/
```

