# fcl-quic

`fcl-quic` is a Free Pascal package for the **QUIC** network protocol.
With it, a Pascal program can be a QUIC client, a QUIC server, or both.

If you want to know how the package works inside, read [docs/Architecture.md](docs/Architecture.md).

## What is QUIC?

QUIC is a protocol to send data between two computers over the internet.
It does the same job as TCP, but it is newer and it works in a different way:

- QUIC sends its data in **UDP datagrams**. A datagram is one small packet of data.
- QUIC always uses **encryption**. It uses TLS 1.3, the same security as HTTPS.
- One QUIC connection can hold many **streams**. A stream is one flow of bytes, like a file or a message.
  When one stream waits for lost data, the other streams can continue.
- QUIC finds lost packets and sends them again.
- QUIC has **congestion control**:  It slows down when the network is busy. 

HTTP/3, the newest version of the web protocol, works on top of QUIC.

QUIC is described in three RFCs:

- RFC 9000: the QUIC transport protocol.
- RFC 9001: how QUIC uses TLS.
- RFC 9002: how QUIC finds lost packets and controls congestion.

## What does this package give you?

- A **client** (`TQuicClient`) that connects to a QUIC server.
- A **server** (`TQuicServer`) that accepts connections from QUIC clients.
- **Streams** that work like a normal Pascal `TStream`.
  You can use `Read`, `Write`, `ReadBuffer` and `WriteBuffer` on them.
- A low-level connection class (`TQuicConnection`) that does no network work at all.
  You give it data, and it gives you data back. You decide how to send it.
- Session resumption and 0-RTT. A client that connected before can connect again faster,
  and it can send data before the handshake is finished.
  **Warning:** an attacker can record 0-RTT data and send it again to another server process,
  or to the same server after a restart. Use 0-RTT only for requests that are safe to repeat.
- Protection for busy servers. A server has limits on the number of connections (`MaxConnections`)
  and on the number of handshakes that are not finished (`MaxHandshakes`).
  When many handshakes are busy (`RetryThreshold`), a new client must first answer a Retry packet.
  This proves that the client really owns its IP address.

These features are **not** in implemented (yet) in this package:

- Connection migration: a connection cannot move to a new IP address.
- Path MTU discovery: all datagrams are at most 1200 bytes.
- ECN and the DATAGRAM extension.
- WinCE: there is no secure random source on WinCE, so creating a client or a server raises an error.

If you need HTTP/3, use the package `fcl-http3`. It is built on top of `fcl-quic`.

## What do you need?

- The package uses the unit `fpsockets`, which older versions do not have.
- **OpenSSL 3.5 or later.** Older OpenSSL versions do not have the QUIC functions that this package needs.
  Many Linux systems still have OpenSSL 3.0, so you may need to install a newer version yourself.
- The packages `fcl-base`, `fcl-hash`, `fcl-net`, `openssl` and `rtl-objpas`. 

## Units

You normally need only two units:

- `FpQuic.OpenSSL`: loads OpenSSL and holds the TLS settings (`TQuicTLSConfig`).
- `FpQuic.Endpoint`: the client, the server, connections and streams.

The unit names already contain a dot. They are the same in a normal build and in a namespaced (dotted) build of Free Pascal.

The full list of units is in [docs/Architecture.md](docs/Architecture.md).

## Step 1: load OpenSSL

Call `QuicInitOpenSSL` once, before you create a client or a server.
It returns `False` when it cannot find OpenSSL 3.5 or later.

```pascal
uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, FpQuic.OpenSSL, FpQuic.Endpoint;

begin
  // Empty names: use the OpenSSL libraries of the system.
  if not QuicInitOpenSSL('', '') then
    begin
    WriteLn('OpenSSL 3.5 or later is required');
    Halt(1);
    end;
```

If OpenSSL 3.5 is not the default OpenSSL of your system, give the full file names:

```pascal
QuicInitOpenSSL('/opt/openssl-3.5/lib/libssl.so.3', '/opt/openssl-3.5/lib/libcrypto.so.3');
```

On Unix, put `cthreads` first in the `uses` clause. The package uses threads.

## Step 2: write a client

This client opens a stream, sends a text and reads the answer.

```pascal
var
  TLS: TQuicTLSConfig;
  Client: TQuicClient;
  Connection: TQuicEndpointConnection;
  Stream: TQuicStreamAdapter;
  Msg: String;
  Buffer: array[0..4095] of AnsiChar;
  Count: Integer;

begin
  // ... load OpenSSL (step 1) ...
  TLS := Default(TQuicTLSConfig);
  TLS.Alpn := ['echo'];            // the name of the protocol; the server must use the same name
  TLS.ServerName := 'localhost';   // the name in the certificate of the server
  TLS.VerifyPeer := True;          // check the certificate of the server (the default is False!)
  TLS.CAFile := 'ca.pem';          // the certificate that signed the server certificate
  Client := TQuicClient.Create(TLS, QuicDefaultEndpointParams);
  try
    Connection := Client.Connect('127.0.0.1', 4433, 5000);   // wait at most 5000 ms
    if Connection = nil then
      begin
      WriteLn('Connection failed');
      Exit;
      end;
    Stream := Connection.OpenStream;
    try
      Msg := 'Hello';
      Stream.WriteBuffer(Msg[1], Length(Msg));
      Stream.Finish;               // tell the server that we will send no more data
      repeat
        Count := Stream.Read(Buffer[0], SizeOf(Buffer));   // 0 means: the stream has ended
        Write(Copy(Buffer, 0, Count));
      until Count = 0;
      WriteLn;
    finally
      Stream.Free;
    end;
    Connection.Close(0, 'done');
  finally
    Client.Free;                   // this also frees Connection
  end;
end.
```

`VerifyPeer` is `False` by default. Then the client does not check the certificate of the server,
and anybody between the client and the server can read and change the data.
Always set `VerifyPeer` and `ServerName` when the client talks to a real server.

## Step 3: write a server

A server needs a certificate and a private key, as PEM files.
This server answers each stream with the same data that it received (an "echo" server).
To keep the example short, it handles one connection at a time.
The example program `examples/quicechoserver.pp` starts one thread for each connection.

```pascal
var
  TLS: TQuicTLSConfig;
  Server: TQuicServer;
  Connection: TQuicEndpointConnection;
  Stream: TQuicStreamAdapter;
  Buffer: array[0..65535] of Byte;
  Count: Integer;

begin
  // ... load OpenSSL (step 1) ...
  TLS := Default(TQuicTLSConfig);
  TLS.Alpn := ['echo'];
  TLS.CertificateFile := 'server.crt';
  TLS.PrivateKeyFile := 'server.key';
  Server := TQuicServer.Create(TLS, QuicDefaultEndpointParams);
  try
    Server.Listen('0.0.0.0', 4433);      // '::' listens on IPv6 and IPv4
    repeat
      Connection := Server.Accept(1000); // nil when no client came within 1000 ms
      if Connection = nil then
        Continue;
      try
        Stream := Connection.AcceptStream(10000);
        if Stream <> nil then
          try
            repeat
              Count := Stream.Read(Buffer[0], SizeOf(Buffer));
              if Count > 0 then
                Stream.WriteBuffer(Buffer[0], Count);
            until Count = 0;
            Stream.Finish;
          finally
            Stream.Free;
          end;
        Connection.WaitClosed(5000);     // give the client time to read the answer and close
      finally
        Connection.Free;
      end;
    until False;
  finally
    Server.Free;
  end;
end.
```

## Rules for freeing objects

- You must free each stream that you get from `OpenStream` or `AcceptStream`.
- A server connection from `Accept` belongs to you. Free it **before** you free the server.
- A client connection from `Connect` belongs to the client. `Client.Free` frees it.
- One `TQuicClient` has one connection. Create more clients for more connections.

## Errors

- `Stream.Read` returns 0 when the other side has finished the stream.
- `Stream.Read` and `Stream.Write` raise `EQuicStreamError` when:
  - the other side reset the stream (`IsReset` is then `True`, and `ResetCode` holds the error code),
  - the connection was closed,
  - the time limit (`Stream.Timeout`, in milliseconds) has passed,
  - you write to a stream that cannot send.
- `Client.Connect` returns `nil` when the connection fails.

## Example programs

The directory `examples/` has complete programs:

| Program | What it does |
|---|---|
| `quicechoserver.pp` | Echo server. Each connection gets its own thread. |
| `quicechoclient.pp` | Sends a message to the echo server and prints the answer. |
| `quicfileserver.pp` | Serves the files of one directory. Clients can also upload files. |
| `quicfileclient.pp` | Downloads (`get`) or uploads (`put`) one file. |
| `quicsansio.pp` | A client and a server `TQuicConnection` that talk in memory, without a network. |

Try the echo programs like this (the test certificate is in `tests/data/`):

```sh
fpc -Fusrc examples/quicechoserver.pp
fpc -Fusrc examples/quicechoclient.pp
examples/quicechoserver tests/data/server.crt tests/data/server.key 4433
examples/quicechoclient 127.0.0.1 4433 "Hello QUIC" tests/data/server.crt
```

The last two arguments of each program can give the file names of `libssl` and `libcrypto`.

## Tests

The directory `tests/` has a test program that uses fpcunit:

```sh
fpc -Fusrc -Futests tests/testquic.pp
QUIC_OPENSSL_DIR=/opt/openssl-3.5/lib tests/testquic --all
```

`QUIC_OPENSSL_DIR` is the directory with `libssl.so.3` and `libcrypto.so.3` of OpenSSL 3.5.
You can leave it out when OpenSSL 3.5 is the default OpenSSL of your system.

`tests/interop/opensslinterop.pp` checks that this package can talk to the QUIC code of OpenSSL itself.

## More information

- [docs/Architecture.md](docs/Architecture.md): how the package is built inside.
- The source files in `src/`: each public class, method and property has a short comment.
