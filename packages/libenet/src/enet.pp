{$mode objfpc}
unit enet;


{
  ENet - Reliable UDP networking library

  FreePascal DLL header: enet.pp
  Copyright (c) 2015 Dmitry D. Chernov aka Black Doomer

  Original file: enet.h
  Copyright (c) 2002-2014 Lee Salzman

  Version 1 for 1.3.12: 25.02.2015

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

interface

uses
  enetplatform, enettypes, enetprotocol, enetlist, enetcallbacks;

const
  ENET_VERSION_MAJOR = 1;
  ENET_VERSION_MINOR = 3;
  ENET_VERSION_PATCH = 12;

  ENET_HOST_ANY       = 0;
  ENET_HOST_BROADCAST : LongWord = $FFFFFFFF;
  ENET_PORT_ANY       = 0;

  ENET_BUFFER_MAXIMUM = 1 + 2 * ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS;

  ENET_HOST_RECEIVE_BUFFER_SIZE          = 256 * 1024;
  ENET_HOST_SEND_BUFFER_SIZE             = 256 * 1024;
  ENET_HOST_BANDWIDTH_THROTTLE_INTERVAL  = 1000;
  ENET_HOST_DEFAULT_MTU                  = 1400;
  ENET_HOST_DEFAULT_MAXIMUM_PACKET_SIZE  = 32 * 1024 * 1024;
  ENET_HOST_DEFAULT_MAXIMUM_WAITING_DATA = 32 * 1024 * 1024;

  ENET_PEER_DEFAULT_ROUND_TRIP_TIME      = 500;
  ENET_PEER_DEFAULT_PACKET_THROTTLE      = 32;
  ENET_PEER_PACKET_THROTTLE_SCALE        = 32;
  ENET_PEER_PACKET_THROTTLE_COUNTER      = 7;
  ENET_PEER_PACKET_THROTTLE_ACCELERATION = 2;
  ENET_PEER_PACKET_THROTTLE_DECELERATION = 2;
  ENET_PEER_PACKET_THROTTLE_INTERVAL     = 5000;
  ENET_PEER_PACKET_LOSS_SCALE            = 1 shl 16;
  ENET_PEER_PACKET_LOSS_INTERVAL         = 10000;
  ENET_PEER_WINDOW_SIZE_SCALE            = 64 * 1024;
  ENET_PEER_TIMEOUT_LIMIT                = 32;
  ENET_PEER_TIMEOUT_MINIMUM              = 5000;
  ENET_PEER_TIMEOUT_MAXIMUM              = 30000;
  ENET_PEER_PING_INTERVAL                = 500;
  ENET_PEER_UNSEQUENCED_WINDOWS          = 64;
  ENET_PEER_UNSEQUENCED_WINDOW_SIZE      = 1024;
  ENET_PEER_FREE_UNSEQUENCED_WINDOWS     = 32;
  ENET_PEER_RELIABLE_WINDOWS             = 16;
  ENET_PEER_RELIABLE_WINDOW_SIZE         = $1000;
  ENET_PEER_FREE_RELIABLE_WINDOWS        = 8;

type
  //enums
  ENetSocketType      = ( ENET_SOCKET_TYPE_STREAM     = 1,
                          ENET_SOCKET_TYPE_DATAGRAM   = 2 );
  ENetSocketWait      = ( ENET_SOCKET_WAIT_NONE       = 0,
                          ENET_SOCKET_WAIT_SEND       = 1,
                          ENET_SOCKET_WAIT_RECEIVE    = 2,
                          ENET_SOCKET_WAIT_INTERRUPT  = 4 );
  ENetSocketOption    = ( ENET_SOCKOPT_NONBLOCK       = 1,
                          ENET_SOCKOPT_BROADCAST      = 2,
                          ENET_SOCKOPT_RCVBUF         = 3,
                          ENET_SOCKOPT_SNDBUF         = 4,
                          ENET_SOCKOPT_REUSEADDR      = 5,
                          ENET_SOCKOPT_RCVTIMEO       = 6,
                          ENET_SOCKOPT_SNDTIMEO       = 7,
                          ENET_SOCKOPT_ERROR          = 8,
                          ENET_SOCKOPT_NODELAY        = 9 );
  ENetSocketShutdown  = ( ENET_SOCKET_SHUTDOWN_READ,
                          ENET_SOCKET_SHUTDOWN_WRITE,
                          ENET_SOCKET_SHUTDOWN_READ_WRITE );

Const
  ENET_PACKET_FLAG_RELIABLE            = 1;
  ENET_PACKET_FLAG_UNSEQUENCED         = 2;
  ENET_PACKET_FLAG_NO_ALLOCATE         = 4;
  ENET_PACKET_FLAG_UNRELIABLE_FRAGMENT = 8;
  ENET_PACKET_FLAG_SENT                = 256; 

Type
  ENetPeerState       = ( ENET_PEER_STATE_DISCONNECTED,
                          ENET_PEER_STATE_CONNECTING,
                          ENET_PEER_STATE_ACKNOWLEDGING_CONNECT,
                          ENET_PEER_STATE_CONNECTION_PENDING,
                          ENET_PEER_STATE_CONNECTION_SUCCEEDED,
                          ENET_PEER_STATE_CONNECTED,
                          ENET_PEER_STATE_DISCONNECT_LATER,
                          ENET_PEER_STATE_DISCONNECTING,
                          ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT,
                          ENET_PEER_STATE_ZOMBIE                    );

  ENetEventType       = ( ENET_EVENT_TYPE_NONE,
                          ENET_EVENT_TYPE_CONNECT,
                          ENET_EVENT_TYPE_DISCONNECT,
                          ENET_EVENT_TYPE_RECEIVE     );

  //definitions
  ENetVersion = enet_uint32;

  //structs pointers
  pENetAddress         = ^ENetAddress;
  pENetPacket          = ^ENetPacket;
  pENetAcknowledgement = ^ENetAcknowledgement;
  pENetOutgoingCommand = ^ENetOutgoingCommand;
  pENetIncomingCommand = ^ENetIncomingCommand;
  pENetChannel         = ^ENetChannel;
  pENetPeer            = ^ENetPeer;
  pENetCompressor      = ^ENetCompressor;
  pENetHost            = ^ENetHost;
  pENetEvent           = ^ENetEvent;

  //callbacks
  ENetPacketFreeCallback = procedure( packet: pENetPacket ); cdecl;
  ENetChecksumCallback = function( const buffers: pENetBuffer; bufferCount: enet_size_t ): enet_uint32; cdecl;
  ENetInterceptCallback = function( host: pENetHost; event: pENetEvent ): enet_int; cdecl;

{$PACKRECORDS C}

  //structs
  ENetAddress = record
    host : enet_uint32;
    port : enet_uint16;
  end;
  ENetPacket = record
    referenceCount : enet_size_t;
    flags          : enet_uint32;
    data           : penet_uint8;
    dataLength     : enet_size_t;
    freeCallback   : ENetPacketFreeCallback;
    userData       : Pointer;
  end;
  ENetAcknowledgement = record
    acknowledgementList : ENetListNode;
    sentTime            : enet_uint32;
    command             : TENetProtocol;
  end;
  ENetOutgoingCommand = record
    outgoingCommandList      : ENetListNode;
    reliableSequenceNumber   : enet_uint16;
    unreliableSequenceNumber : enet_uint16;
    sentTime                 : enet_uint32;
    roundTripTimeout         : enet_uint32;
    roundTripTimeoutLimit    : enet_uint32;
    fragmentOffset           : enet_uint32;
    fragmentLength           : enet_uint16;
    sendAttempts             : enet_uint16;
    command                  : TENetProtocol;
    packet                   : pENetPacket;
  end;
  ENetIncomingCommand = record
    incomingCommandList      : ENetListNode;
    reliableSequenceNumber   : enet_uint16;
    unreliableSequenceNumber : enet_uint16;
    command                  : TENetProtocol;
    fragmentCount            : enet_uint32;
    fragmentsRemaining       : enet_uint32;
    fragments                : penet_uint32;
    packet                   : pENetPacket;
  end;
  ENetChannel = record
    outgoingReliableSequenceNumber   : enet_uint16;
    outgoingUnreliableSequenceNumber : enet_uint16;
    usedReliableWindows              : enet_uint16;
    reliableWindows                  : array[ 0..ENET_PEER_RELIABLE_WINDOWS-1 ] of enet_uint16;
    incomingReliableSequenceNumber   : enet_uint16;
    incomingUnreliableSequenceNumber : enet_uint16;
    incomingReliableCommands         : TENetList;
    incomingUnreliableCommands       : TENetList;
  end;
  ENetPeer = record
    dispatchList                   : ENetListNode;
    host                           : pENetHost;
    outgoingPeerID                 : enet_uint16;
    incomingPeerID                 : enet_uint16;
    connectID                      : enet_uint32;
    outgoingSessionID              : enet_uint8;
    incomingSessionID              : enet_uint8;
    address                        : ENetAddress;
    data                           : Pointer;
    state                          : ENetPeerState;
    channels                       : pENetChannel;
    channelCount                   : enet_size_t;
    incomingBandwidth              : enet_uint32;
    outgoingBandwidth              : enet_uint32;
    incomingBandwidthThrottleEpoch : enet_uint32;
    outgoingBandwidthThrottleEpoch : enet_uint32;
    incomingDataTotal              : enet_uint32;
    outgoingDataTotal              : enet_uint32;
    lastSendTime                   : enet_uint32;
    lastReceiveTime                : enet_uint32;
    nextTimeout                    : enet_uint32;
    earliestTimeout                : enet_uint32;
    packetLossEpoch                : enet_uint32;
    packetsSent                    : enet_uint32;
    packetsLost                    : enet_uint32;
    packetLoss                     : enet_uint32;
    packetLossVariance             : enet_uint32;
    packetThrottle                 : enet_uint32;
    packetThrottleLimit            : enet_uint32;
    packetThrottleCounter          : enet_uint32;
    packetThrottleEpoch            : enet_uint32;
    packetThrottleAcceleration     : enet_uint32;
    packetThrottleDeceleration     : enet_uint32;
    packetThrottleInterval         : enet_uint32;
    pingInterval                   : enet_uint32;
    timeoutLimit                   : enet_uint32;
    timeoutMinimum                 : enet_uint32;
    timeoutMaximum                 : enet_uint32;
    lastRoundTripTime              : enet_uint32;
    lowestRoundTripTime            : enet_uint32;
    lastRoundTripTimeVariance      : enet_uint32;
    highestRoundTripTimeVariance   : enet_uint32;
    roundTripTime                  : enet_uint32;
    roundTripTimeVariance          : enet_uint32;
    mtu                            : enet_uint32;
    windowSize                     : enet_uint32;
    reliableDataInTransit          : enet_uint32;
    outgoingReliableSequenceNumber : enet_uint16;
    acknowledgements               : TENetList;
    sentReliableCommands           : TENetList;
    sentUnreliableCommands         : TENetList;
    outgoingReliableCommands       : TENetList;
    outgoingUnreliableCommands     : TENetList;
    dispatchedCommands             : TENetList;
    needsDispatch                  : enet_int;
    incomingUnsequencedGroup       : enet_uint16;
    outgoingUnsequencedGroup       : enet_uint16;
    unsequencedWindow              : array[ 0..(ENET_PEER_UNSEQUENCED_WINDOW_SIZE div 32)-1 ] of enet_uint32;
    eventData                      : enet_uint32;
    totalWaitingData               : enet_size_t;
  end;
  ENetCompressor = record
    context    : Pointer;
    compress   : function( context: Pointer; const inBuffers: pENetBuffer; inBufferCount, inLimit: enet_size_t; outData: penet_uint8; outLimit: enet_size_t ): enet_size_t; cdecl;
    decompress : function( context: Pointer; const inData: penet_uint8; inLimit: enet_size_t; outData: penet_uint8; outLimit: enet_size_t ): enet_size_t; cdecl;
    destroy    : procedure( context: Pointer ); cdecl;
  end;
  ENetHost = record
    socket                     : ENetSocket;
    address                    : ENetAddress;
    incomingBandwidth          : enet_uint32;
    outgoingBandwidth          : enet_uint32;
    bandwidthThrottleEpoch     : enet_uint32;
    mtu                        : enet_uint32;
    randomSeed                 : enet_uint32;
    recalculateBandwidthLimits : enet_int;
    peers                      : pENetPeer;
    peerCount                  : enet_size_t;
    channelLimit               : enet_size_t;
    serviceTime                : enet_uint32;
    dispatchQueue              : TENetList;
    continueSending            : enet_int;
    packetSize                 : enet_size_t;
    headerFlags                : enet_uint16;
    commands                   : array[ 0..ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS-1 ] of TENetProtocol;
    commandCount               : enet_size_t;
    buffers                    : array[ 0..ENET_BUFFER_MAXIMUM-1 ] of ENetBuffer;
    bufferCount                : enet_size_t;
    checksum                   : ENetChecksumCallback;
    compressor                 : ENetCompressor;
    packetData                 : array[ 0..1, 0..ENET_PROTOCOL_MAXIMUM_MTU-1 ] of enet_uint8;
    receivedAddress            : ENetAddress;
    receivedData               : penet_uint8;
    receivedDataLength         : enet_size_t;
    totalSentData              : enet_uint32;
    totalSentPackets           : enet_uint32;
    totalReceivedData          : enet_uint32;
    totalReceivedPackets       : enet_uint32;
    intercept                  : ENetInterceptCallback;
    connectedPeers             : enet_size_t;
    bandwidthLimitedPeers      : enet_size_t;
    duplicatePeers             : enet_size_t;
    maximumPacketSize          : enet_size_t;
    maximumWaitingData         : enet_size_t;
  end;
  ENetEvent = record
    kind      : ENetEventType; //originally "type", which conflicts
    peer      : pENetPeer;
    channelID : enet_uint8;
    data      : enet_uint32;
    packet    : pENetPacket;
  end;

{$PACKRECORDS DEFAULT}

//inline macros
function ENET_VERSION_CREATE( const major, minor, patch: LongInt ): ENetVersion; inline;
function ENET_VERSION_GET_MAJOR( const version: ENetVersion ): LongInt; inline;
function ENET_VERSION_GET_MINOR( const version: ENetVersion ): LongInt; inline;
function ENET_VERSION_GET_PATCH( const version: ENetVersion ): LongInt; inline;
function ENET_VERSION(): ENetVersion; inline;

//external
{$MACRO ON}
{$DEFINE libraryENet := cdecl; external 'enet'}

function enet_initialize(): enet_int; libraryENet;
function enet_initialize_with_callbacks( version: ENetVersion; const inits: pENetCallbacks ): enet_int; libraryENet;
procedure enet_deinitialize(); libraryENet;
function enet_linked_version(): ENetVersion; libraryENet;

function enet_time_get(): enet_uint32; libraryENet;
procedure enet_time_set( newTimeBase: enet_uint32 ); libraryENet;

function enet_socket_create( kind: ENetSocketType ): ENetSocket; libraryENet;
function enet_socket_bind( socket: ENetSocket; const address: pENetAddress ): enet_int; libraryENet;
function enet_socket_get_address( socket: ENetSocket; address: pENetAddress ): enet_int; libraryENet;
function enet_socket_listen( socket: ENetSocket; backlog: enet_int ): enet_int; libraryENet;
function enet_socket_accept( socket: ENetSocket; address: pENetAddress ): ENetSocket; libraryENet;
function enet_socket_connect( socket: ENetSocket; const address: pENetAddress ): enet_int; libraryENet;
function enet_socket_send( socket: ENetSocket; const address: pENetAddress; const buffers: pENetBuffer; bufferCount: enet_size_t ): enet_int; libraryENet;
function enet_socket_receive( socket: ENetSocket; address: pENetAddress; buffers: pENetBuffer; bufferCount: enet_size_t ): enet_int; libraryENet;
function enet_socket_wait( socket: ENetSocket; condition: penet_uint32; timeout: enet_uint32 ): enet_int; libraryENet;
function enet_socket_set_option( socket: ENetSocket; option: ENetSocketOption; value: enet_int ): enet_int; libraryENet;
function enet_socket_get_option( socket: ENetSocket; option: ENetSocketOption; value: penet_int ): enet_int; libraryENet;
function enet_socket_shutdown( socket: ENetSocket; how: ENetSocketShutdown ): enet_int; libraryENet;
procedure enet_socket_destroy( socket: ENetSocket ); libraryENet;
function enet_socketset_select( maxSocket: ENetSocket; readSet: pENetSocketSet; writeSet: pENetSocketSet; timeout: enet_uint32 ): enet_int; libraryENet;

function enet_address_set_host( address: pENetAddress; const hostName: PChar ): enet_int; libraryENet;
function enet_address_get_host_ip( const address: pENetAddress; hostName: PChar; nameLength: enet_size_t ): enet_int; libraryENet;
function enet_address_get_host( const address: pENetAddress; hostName: PChar; nameLength: enet_size_t ): enet_int; libraryENet;

function enet_packet_create( const data: Pointer; dataLength: enet_size_t; flags: enet_uint32 ): pENetPacket; libraryENet;
procedure enet_packet_destroy( packet: pENetPacket ); libraryENet;
function enet_packet_resize( packet: pENetPacket; dataLength: enet_size_t ): enet_int; libraryENet;
function enet_crc32( const buffers: pENetBuffer; bufferCount: enet_size_t ): enet_uint32; libraryENet;

function enet_host_create( const address: pENetAddress; peerCount, channelLimit: enet_size_t; incomingBandwidth, outgoingBandwidth: enet_uint32 ): pENetHost; libraryENet;
procedure enet_host_destroy( host: pENetHost ); libraryENet;
function enet_host_connect( host: pENetHost; const address: pENetAddress; channelCount: enet_size_t; data: enet_uint32 ): pENetPeer; libraryENet;
function enet_host_check_events( host: pENetHost; event: pENetEvent ): enet_int; libraryENet;
function enet_host_service( host: pENetHost; event: pENetEvent; timeout: enet_uint32 ): enet_int; libraryENet;
procedure enet_host_flush( host: pENetHost ); libraryENet;
procedure enet_host_widecast( host: pENetHost; channelID: enet_uint8; packet: pENetPacket ); libraryENet name 'enet_host_broadcast'; //renamed due to names conflict
procedure enet_host_compress( host: pENetHost; const compressor: pENetCompressor ); libraryENet;
function enet_host_compress_with_range_coder( host: pENetHost ): enet_int; libraryENet;
procedure enet_host_channel_limit( host: pENetHost; channelLimit: enet_size_t ); libraryENet;
procedure enet_host_bandwidth_limit( host: pENetHost; incomingBandwidth, outgoingBandwidth: enet_uint32 ); libraryENet;

function enet_peer_send( peer: pENetPeer; channelID: enet_uint8; packet: pENetPacket ): enet_int; libraryENet;
function enet_peer_receive( peer: pENetPeer; channelID: penet_uint8 ): pENetPacket; libraryENet;
procedure enet_peer_ping( peer: pENetPeer ); libraryENet;
procedure enet_peer_ping_frequency( peer: pENetPeer; pingInterval: enet_uint32 ); libraryENet name 'enet_peer_ping_interval'; //renamed due to names conflict
procedure enet_peer_timeout( peer: pENetPeer; timeoutLimit, timeoutMinimum, timeoutMaximum: enet_uint32 ); libraryENet;
procedure enet_peer_reset( peer: pENetPeer ); libraryENet;
procedure enet_peer_disconnect( peer: pENetPeer; data: enet_uint32 ); libraryENet;
procedure enet_peer_disconnect_now( peer: pENetPeer; data: enet_uint32 ); libraryENet;
procedure enet_peer_disconnect_later( peer: pENetPeer; data: enet_uint32 ); libraryENet;
procedure enet_peer_throttle_configure( peer: pENetPeer; interval, acceleration, deceleration: enet_uint32 ); libraryENet;

function enet_range_coder_create(): Pointer; libraryENet;
procedure enet_range_coder_destroy( context: Pointer ); libraryENet;
function enet_range_coder_compress( context: Pointer; const inBuffers: pENetBuffer; inBufferCount, inLiit: enet_size_t; outData: penet_uint8; outLimit: enet_size_t ): enet_size_t; libraryENet;
function enet_range_coder_decompress( context: Pointer; const inData: penet_uint8; inLimit: enet_size_t; outData: penet_uint8; outLimit: enet_size_t ): enet_size_t; libraryENet;

implementation

function ENET_VERSION_CREATE( const major, minor, patch: LongInt ): ENetVersion; inline;
   begin Result := (major shl 16) or (minor shl 8) or patch;
     end;

function ENET_VERSION_GET_MAJOR( const version: ENetVersion ): LongInt; inline;
   begin Result := (version shr 16) and $FF;
     end;

function ENET_VERSION_GET_MINOR( const version: ENetVersion ): LongInt; inline;
   begin Result := (version shr 8) and $FF;
     end;

function ENET_VERSION_GET_PATCH( const version: ENetVersion ): LongInt; inline;
   begin Result := version and $FF;
     end;

function ENET_VERSION(): ENetVersion; inline;
   begin Result := ENET_VERSION_CREATE( ENET_VERSION_MAJOR, ENET_VERSION_MINOR, ENET_VERSION_PATCH );
     end;

end.

