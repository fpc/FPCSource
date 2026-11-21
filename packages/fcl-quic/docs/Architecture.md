# The architecture of fcl-quic

This document explains how the `fcl-quic` package is built inside.
To learn how to *use* the package, read the [README](../README.md) first.

## 1. Three layers

The package has three layers. Each layer uses only the layer below it.

```
  +-----------------------------------------------------------+
  |  Your program                                             |
  +-----------------------------------------------------------+
        |  Read / Write on a TStream
        v
  +-----------------------------------------------------------+
  |  Layer 3: the endpoint            (unit FpQuic.Endpoint)  |
  |  TQuicClient, TQuicServer, TQuicEndpointConnection,       |
  |  TQuicStreamAdapter                                       |
  |  - owns the UDP socket and a worker thread                |
  |  - reads the clock                                        |
  +-----------------------------------------------------------+
        |  datagrams in, datagrams out, the current time
        v
  +-----------------------------------------------------------+
  |  Layer 2: the connection        (unit FpQuic.Connection)  |
  |  TQuicConnection                                          |
  |  - the QUIC protocol itself                               |
  |  - no sockets, no threads, no clock                       |
  +-----------------------------------------------------------+
        |  uses
        v
  +-----------------------------------------------------------+
  |  Layer 1: building blocks                                 |
  |  buffers, frames, packets, transport parameters,          |
  |  encryption, TLS, streams, loss recovery                  |
  +-----------------------------------------------------------+
        |  TLS handshake and ciphers
        v
     OpenSSL 3.5
```

- **Layer 1** has small, separate parts. Each part does one job, for example "write a number in the QUIC format"
  or "encrypt one packet".
- **Layer 2** is the QUIC protocol. It puts the parts of layer 1 together.
- **Layer 3** connects layer 2 to the real network and to the real clock. It also makes everything blocking,
  so that your program can simply call `Read` and wait.

## 2. The connection does no input or output

The most important idea in this package is this:
**`TQuicConnection` never touches the network, and it never reads the clock.**
This design is often called "sans-I/O" ("sans" means "without").

The connection is like a machine with three buttons:

| You call | What happens |
|---|---|
| `ReceiveDatagram(Data, Now)` | You give the connection a datagram that arrived from the network. |
| `NextDatagram(Now, Data)` | The connection gives you a datagram to send. It returns `False` when it has nothing to send. |
| `HandleTimeout(Now)` | You tell the connection that its timer has expired. `NextTimeout` tells you when that happens. |

`Now` is the current time in **microseconds**, as a `QWord`. You choose where the time comes from.

This design has two good results:

- **Tests are simple and exact.** A test can connect two `TQuicConnection` objects in memory.
  It can lose, copy or change the order of packets on purpose. It can move the time forward by exactly
  one second. The same test gives the same result every time.
- **You can use your own network code.** If you already have an event loop, you can feed it datagrams
  yourself. The example `examples/quicsansio.pp` shows this.

The rest of the connection API works with streams:
`OpenStream`, `AcceptStream`, `WriteStream`, `ReadStream`, `ResetStream`, `StopSending` and `Close`.
These calls only change the state of the connection. The data goes out at the next `NextDatagram`.

## 3. The units

The units are listed from the bottom to the top. A unit uses only units that are higher in this list.

| Unit | What it does |
|---|---|
| `FpQuic.Consts` | Numbers from the RFCs: frame types, error codes, the size limits. The exception class `EQuicError`. |
| `FpQuic.Buffers` | `TQuicReader` and `TQuicWriter` read and write bytes. QUIC writes numbers in a special short format, called a "variable-length integer". `TQuicRangeSet` remembers ranges of numbers, for example which packets have arrived. |
| `FpQuic.Frames` | A packet holds one or more **frames**. Each frame is one message, for example "here are bytes 100 to 199 of stream 4" or "I received packets 1 to 10". This unit turns frames into bytes and bytes into frames. |
| `FpQuic.Packets` | Reads and writes the **header** of a packet: its type, its connection IDs and its packet number. |
| `FpQuic.TransportParams` | Both sides send their limits during the handshake, for example "you may open 100 streams". These limits are called **transport parameters**. |
| `FpQuic.Crypto` | Makes encryption keys from secrets (HKDF). Encrypts and decrypts packets (`TQuicKeys`). Hides the packet number (header protection). |
| `FpQuic.TLS` | Two interfaces: `IQuicTLS` (the TLS handshake) and `IQuicTLSEvents` (what the TLS handshake reports back). |
| `FpQuic.OpenSSL` | Implements the TLS interface and the ciphers with OpenSSL 3.5. Loads OpenSSL (`QuicInitOpenSSL`). Holds the TLS settings (`TQuicTLSConfig`). |
| `FpQuic.Streams` | One stream (`TQuicStream`): data to send, data received, and flow control. Received data can arrive in the wrong order; `TQuicReassembly` puts it back in order. `TQuicStreamMap` holds all streams of one connection. |
| `FpQuic.Recovery` | Finds lost packets (`TQuicLossDetector`), measures the round-trip time (`TQuicRttEstimator`), builds acknowledgements (`TQuicAckTracker`) and controls congestion (`TQuicNewReno`). |
| `FpQuic.Connection` | `TQuicConnection`: the protocol. See section 2. |
| `FpQuic.Udp` | Sends many datagrams with one system call, on Linux and Windows. |
| `FpQuic.Endpoint` | The client, the server, the worker thread and the blocking streams. See section 6. |

## 4. The life of a datagram

### Receiving

1. The worker thread of the endpoint receives a datagram from the UDP socket.
2. The endpoint finds the right connection:
   - A client has only one connection, so every datagram goes to it.
   - A server reads the **connection ID** in the packet header, and uses this ID to find the connection.
     It creates a new connection when the datagram starts a new connection (an "Initial" packet).
3. The endpoint calls `ReceiveDatagram` on that connection.
4. One datagram can hold more than one packet. The connection splits the datagram into packets.
5. For each packet, the connection:
   - removes the header protection, so it can read the packet number,
   - decrypts the packet with the keys of the right encryption level,
   - reads the frames, one by one, and handles them.
6. Stream data goes to the right stream. If the data arrives in the wrong order, the stream keeps it
   until the missing part arrives.
7. The connection tells the endpoint about new data, through the interface `IQuicConnectionEvents`.
   The endpoint wakes up the threads that wait in `Read`.

### Sending

1. Your program calls `Write` on a stream. The data is stored in the send buffer of the stream.
2. The endpoint calls `NextDatagram` until it returns `False`.
3. For each datagram, the connection collects frames: acknowledgements, control frames and stream data.
   It asks congestion control how much it may send.
4. It encrypts each packet, protects the header, and puts the packets together in one datagram.
5. The loss detector remembers each packet that was sent.
6. The endpoint sends the datagrams on the UDP socket. When it can, it sends many datagrams with one system call.

### When a packet is lost

- The other side sends **ACK** frames. An ACK frame lists the packets that arrived.
- When later packets arrive but an older one does not, the loss detector decides that the older packet is lost.
  The connection then puts the frames of that packet back in the queue, and they go out in a new packet.
  (QUIC never sends the same packet twice. It sends the same *frames* in a new packet.)
- When no ACK arrives at all for some time, a timer expires (the "probe timeout").
  The connection then sends a probe packet to get an answer.
- Each lost packet makes congestion control send less data for a while. This is the NewReno method from RFC 9002.

## 5. The handshake and the keys

QUIC does not use TLS records. It uses only the TLS **handshake** messages, and it encrypts its own packets.

- The connection gives TLS the handshake bytes that arrive in CRYPTO frames (`IQuicTLS.ProvideData`).
- TLS answers through `IQuicTLSEvents`:
  - `TLSSendCrypto`: send these handshake bytes to the other side.
  - `TLSSecret`: here is a new secret. The connection makes new packet keys from it.
  - `TLSPeerTransportParams`: here are the limits of the other side.
  - `TLSAlert`: the handshake failed.

A connection uses up to four sets of keys, one after the other:

| Encryption level | Used for | Where the keys come from |
|---|---|---|
| Initial | The first packets | Computed from the connection ID. Everybody can compute them. |
| 0-RTT | Early data from a client that connected before | The saved session |
| Handshake | The rest of the handshake | TLS |
| 1-RTT (Application) | All normal data | TLS |

Each level except 0-RTT has its own **packet number space** (`TQuicSpace`), with its own packet numbers,
its own ACKs and its own CRYPTO data. The Initial keys are thrown away as soon as the Handshake keys work,
and the Handshake keys are thrown away when the handshake is confirmed.

Later, either side can change the 1-RTT keys. This is called a **key update**. The package supports it in both directions.
A key must not encrypt too many packets (RFC 9001 section 6.6). The connection therefore changes its keys by itself
when it has used half of this limit (`QuicConfidentialityLimit`). If it reaches the limit anyway, it closes with the
error AEAD_LIMIT_REACHED. It also closes with this error when too many received packets cannot be decrypted
(`QuicIntegrityLimit`).

## 6. The endpoint: threads and waiting

A `TQuicClient` or a `TQuicServer` is an **endpoint**. An endpoint has:

- one UDP socket,
- one worker thread,
- one lock (a critical section),
- one signal (an RTL event) to wake up waiting threads,
- a list of connections.

The **worker thread** does this, again and again:

1. Wait at most 20 ms for a datagram on the socket.
2. Give each datagram to the right connection.
3. Call `HandleTimeout` on each connection whose timer has expired.
4. Send everything the connections want to send.
5. Set the signal, so that waiting threads wake up and check again.

Your threads call `Read`, `Write`, `AcceptStream` or `Accept`. These calls:

1. take the lock,
2. call the connection (for example `ReadStream`),
3. send what the connection wants to send,
4. release the lock,
5. return when they are done, or wait for the signal and try again.

All work on a connection happens while the lock is held. So only one thread at a time changes a connection,
and `TQuicConnection` itself needs no locks.

Several threads can use different streams of the same connection at the same time.

`Write` does not wait until the data has arrived. It waits only when more than 1 MB of data is still waiting to be sent
on that stream.

## 7. Interfaces and ownership

The package uses interfaces where a second implementation is useful, for example a fake TLS object in a test:

| Interface | Implemented by | Freed by |
|---|---|---|
| `IQuicTLS` | `TQuicOpenSSLTLS` | The code that created the connection. It must live longer than the connection. |
| `IQuicTLSEvents` | `TQuicConnection` | |
| `IQuicCipherFactory` | `TQuicOpenSSLCipherFactory` | The code that created the connection. |
| `IQuicCongestionControl` | `TQuicNewReno` | The connection. |
| `IQuicConnectionEvents` | `TQuicEndpointConnection`, or your own class | The code that created it. |

All interfaces are **CORBA interfaces** (`{$INTERFACES CORBA}`).
CORBA interfaces have no reference counting. An object is never freed automatically when the last interface
to it goes away. The code that creates an object must also free it.

## 8. Errors

- When the other side breaks the protocol, the code raises `EQuicError` with a QUIC error code.
  `TQuicConnection` catches it in `ReceiveDatagram` and closes the connection.
  It sends a CONNECTION_CLOSE frame with that error code. The exception never reaches your program.
- A packet that cannot be decrypted is ignored. This is what the RFC asks for.
- Other exceptions in `ReceiveDatagram`, for example from your session cache handler, also close the connection,
  with the error INTERNAL_ERROR.
- OpenSSL calls the package through C callbacks. An exception must not travel through the C code of OpenSSL.
  So each callback catches the exception and keeps it. When OpenSSL returns, the package raises it again.
- When an exception leaves the work of the worker thread, the endpoint closes all its connections at once
  (`TQuicConnection.Abort`). The waiting threads wake up and see the closed connection. The worker thread keeps running.
- The endpoint raises `EQuicStreamError` in your thread when a stream cannot be read or written.
  The [README](../README.md) lists the cases.

## 9. What the package uses from Free Pascal

| Need | Comes from |
|---|---|
| SHA-256 and SHA-384 HMAC (for HKDF) | `fcl-hash` |
| Random bytes for connection IDs | `fcl-hash` (`fphashutils`) |
| Loading OpenSSL, the TLS handshake, the ciphers | `openssl` |
| UDP sockets | `fcl-net` (`fpsockets`) |
| Threads, locks and events | the RTL |

A few OpenSSL 3.5 functions are not in the `openssl` unit yet. `FpQuic.OpenSSL` loads them itself
from the OpenSSL library.

## 10. Tests

The tests in `tests/` follow the layers:

- **Layer 1:** each building block has its own tests. Many tests use the example values printed in the RFCs,
  so the results are checked against the standard.
- **Layer 2:** `tests/utquicnetwork.pp` is a fake network in memory. It connects a client and a server
  `TQuicConnection` with a fake clock. It can lose, copy and reorder packets. About 50 scenarios run on it:
  the handshake, lost packets, flow control, key updates, closing, 0-RTT and more.
- **Layer 3:** the endpoint tests use real UDP sockets on the local computer (`127.0.0.1`).

Every line of code in `src/` is run by at least one test.
