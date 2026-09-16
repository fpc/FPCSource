{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 - by the Free Pascal development team

    Abstract websocket protocol implementation - objects only

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit FpWebSocket;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Net.Sockets, System.Net.Ssockets;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sockets, ssockets;
{$ENDIF FPC_DOTTEDUNITS}

Const
  SSecWebSocketGUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  SSecWebsocketProtocol = 'Sec-WebSocket-Protocol';
  SSecWebsocketVersion = 'Sec-WebSocket-Version';
  SSecWebsocketExtensions = 'Sec-WebSocket-Extensions';
  SSecWebsocketKey = 'Sec-WebSocket-Key';
  SSecWebsocketAccept = 'Sec-WebSocket-Accept';

  MinFrameSize = 4;

  DefaultWebSocketVersion = 13;

  // Opcodes
  FlagReserved     = $F;
  FlagContinuation = $0;
  FlagText         = $1;
  FlagBinary       = $2;
  FlagClose        = $8;
  FlagPing         = $9;
  FlagPong         = $A;

  // For SVR etc.
  FlagTwoBytes   = 126;
  FlagEightBytes = 127;
  FlagFinalFrame : Byte = $80;
  FlagMasked     : Byte = $80;
  FlagLengthMask : Byte = $7F;

  FlagRES1 = $40;
  FlagRES2 = $20;
  FlagRES3 = $10;

  CLOSE_NORMAL_CLOSURE              = 1000;
  CLOSE_GOING_AWAY                  = 1001;
  CLOSE_PROTOCOL_ERROR              = 1002;
  CLOSE_UNSUPORTED_DATA             = 1003;
  CLOSE_RESERVER                    = 1004;
  CLOSE_NO_STATUS_RCVD              = 1005;
  CLOSE_ABNORMAL_CLOSURE            = 1006;
  CLOSE_INVALID_FRAME_PAYLOAD_DATA  = 1007;
  CLOSE_POLICY_VIOLATION            = 1008;
  CLOSE_MESSAGE_TOO_BIG             = 1009;
  CLOSE_MANDRATORY_EXT              = 1010;
  CLOSE_INTERNAL_SERVER_ERROR       = 1011;
  CLOSE_TLS_HANDSHAKE               = 1015;

  { These limits apply to incoming data. Set the corresponding connection
    property to 0 to disable the policy limit. The native TBytes/SizeInt
    limit is always enforced. }
  DefaultMaxFramePayloadSize   = 16 * 1024 * 1024;
  DefaultMaxMessagePayloadSize = 64 * 1024 * 1024;

type
  EWebSocket = Class(Exception);
  EWSHandShake = class(EWebSocket);
  EWSReadInterrupted = class(EWebSocket);

  { Typed transport failures let a message pump distinguish an orderly EOF,
    an interrupted read and a terminal socket error. }
  EWSTransportError = class(EWebSocket)
  private
    FErrorCode: LongInt;
  public
    constructor Create(const aMessage: String; aErrorCode: LongInt); reintroduce;
    property ErrorCode: LongInt read FErrorCode;
  end;
  EWSConnectionClosed = class(EWSTransportError);
  EWSReadError = class(EWSTransportError);
  EWSWriteError = class(EWSTransportError);

  { RFC protocol errors carry the close status which should be returned to
    the peer before the connection is discarded. }
  EWSProtocolError = class(EWebSocket)
  private
    FCloseCode: Word;
  public
    constructor Create(const aMessage: String; aCloseCode: Word); reintroduce;
    property CloseCode: Word read FCloseCode;
  end;

  TFrameType = (ftContinuation,ftText,ftBinary,ftClose,ftPing,ftPong,ftFutureOpcodes);

  TFrameTypes = Set of TFrameType;

  TFrameSequence = (fsFirst,fsContinuation,fsLast);
  TFrameSequences = Set of TFrameSequence;

  TIncomingResult = (irNone,    // No data waiting
                     irWaiting, // Data waiting
                     irOK,      // Data was waiting and handled
                     irClose    // The connection reached a terminal read/close condition
                     );

  { TFrameTypeHelper }

  TFrameTypeHelper = Type helper for TFrametype
  private
    function GetAsFlag: Byte;
    procedure SetAsFlag(AValue: Byte);
  Public
    Property asFlag : Byte Read GetAsFlag Write SetAsFlag;
  end;


  { TWSHeaders }

  TWSHeaders = class
  private
    FRawHeaders: TStrings;
    FResource: String;
  Protected
    Function GetS(aIdx : Integer) : String;
    procedure SetS(AIndex: Integer; const AValue: string);
    Function GetH(const aName : string) : String;
    procedure SetH(const aName, aValue: string);
  Public
    Const
      WSHeaderNames : Array[0..8] of string
                    = ('Host','Origin','Connection','Upgrade',SSecWebSocketProtocol,
                       SSecWebSocketVersion,SSecWebSocketExtensions,SSecWebSocketKey,
                       SSecWebSocketAccept);
  public
    constructor Create(const aResource : String; const AHeaderList: TStrings); virtual;
    Destructor Destroy; override;
    Property RawHeaders : TStrings Read FRawHeaders;
    property Resource : String Read FResource Write FResource;
    property Host: string Index 0 read GetS Write SetS;
    property Origin: string Index 1 read GetS Write SetS;
    property Connection: string Index 2 read GetS Write SetS;
    property Upgrade: string Index 3 read GetS Write SetS;
    property Protocol: string Index 4 read GetS Write SetS;
    property Version: string Index 5 read GetS Write SetS;
    property Extensions : String Index 6 read GetS Write SetS;
    property Key: string Index 7 read GetS Write SetS;
  end;

  { TWSHandShakeRequest }

  TWSHandShakeRequest = Class(TWSHeaders)
  private
    FPort: Word;
  Public
    Constructor Create(const aResource : string; const aExtraHeaders : TStrings); override;
    class function GenerateKey: String; static;
    Procedure ToStrings(aHeaders : TStrings);
    Property Port : Word Read FPort Write FPort;
  End;


  { TWSHandShakeResponse }

  TWSHandShakeResponse = Class (TWSHeaders)
  private
    FHTTPVersion: String;
    FStatusCode: Integer;
    FStatusText: String;
  Public
    Constructor Create(const aResource : string; const aExtraHeaders : TStrings); override;
    Procedure ToStrings(aHandShake : TWSHandshakeRequest; aResponse : TStrings; AddStatusLine : Boolean);
    Property HTTPVersion : String Read FHTTPVersion Write FHTTPVersion;
    Property StatusCode : Integer Read FStatusCode Write FStatusCode;
    Property StatusText : String Read FStatusText Write FSTatusText;
    property Accept : String Index 8 read GetS Write SetS;
  End;

{$INTERFACES CORBA}

  { IWSTransport }

  IWSTransport = Interface
    // Check if transport can read data
    Function CanRead(aTimeOut: Integer) : Boolean;
    // Read length of buffer bytes. Raise exception if no data read
    Procedure ReadBuffer (aBytes : TBytes);
    // Read at most aCount bytes into buffer. Return number of bytes actually read, set length of buffer to actually read
    function ReadBytes (var aBytes : TBytes; aCount : Integer) : Integer;
    // Write at most aCount bytes.
    function WriteBytes (aBytes : TBytes; aCount : Integer) : Integer;
    // Write complete buffer. Raise exception if not all bytes were written.
    Procedure WriteBuffer (aBytes : TBytes);
    function ReadLn : String;
    function PeerIP: string;
    function PeerPort: word;
  end;

  { TWSSocketHelper }

  TWSSocketHelper = Class (TObject,IWSTransport)
  Private
    FSocket : TSocketStream;
    FReadState : LongInt;
    FReadsCancelled : LongInt;
    FWriteLock : TRTLCriticalSection;
    Procedure BeginRead;
    Procedure EndRead;
    function ReadSocket(var aBuffer; aCount : LongInt) : LongInt;
    Procedure ReadSocketBuffer(var aBuffer; aCount : LongInt);
    function WriteSocket(const aBuffer; aCount : LongInt) : LongInt;
  Public
    Constructor Create (aSocket : TSocketStream);
    Destructor Destroy; override;
    Procedure InterruptRead;
    Procedure CancelReads;
    Function CanRead(aTimeOut: Integer) : Boolean;
    function PeerIP: string; virtual;
    function PeerPort: word; virtual;
    function ReadLn : String; virtual;
    function ReadBytes (var aBytes : TBytes; aCount : Integer) : Integer; virtual;
    Procedure ReadBuffer (aBytes : TBytes); virtual;
    function WriteBytes (aBytes : TBytes; aCount : Integer) : Integer; virtual;
    Procedure WriteBuffer (aBytes : TBytes);
    Property Socket : TSocketStream Read FSocket;
  end;

  TWSTransport = class(TObject, IWSTransport)
  Private
    FHelper : TWSSocketHelper;
    FStream : TSocketStream;
    function GetSocket: TSocketStream;
  Public
    Constructor Create(aStream : TSocketStream);
    Destructor Destroy; override;
    Procedure CloseSocket;
    Procedure InterruptRead;
    Procedure CancelReads;
    Property Helper : TWSSocketHelper Read FHelper Implements IWSTransport;
    Property Socket : TSocketStream Read GetSocket;
  end;


  { TWSFramePayload }

  TWSFramePayload = record
    DataLength: QWord;
    // Data is unmasked
    Data: TBytes;
    MaskKey: dword;
    Masked: Boolean;
    Procedure ReadData(var Content : TBytes; aTransport : IWSTransport);
    Procedure Read(buffer: TBytes; aTransport : IWSTransport); overload;
    Procedure Read(buffer: TBytes; aTransport : IWSTransport;
      aMaxPayloadSize: QWord); overload;
    class procedure DoMask(var aData: TBytes; Key: DWORD); static;
    class procedure CopyMasked(SrcData: TBytes; var DestData: TBytes; Key: DWORD; aOffset: Integer); static;
    class function CopyMasked(SrcData: TBytes; Key: DWORD) : TBytes; static;
  end;

  { TWSFrame }
  TWSFrame = Class
  private
    FFrameType: TFrameType;
    FFinalFrame: Boolean;
    FRSV: Byte;
    FPayload : TWSFramePayload;
    FReason: WORD;
  protected
    function Read(aTransport: IWSTransport): boolean; overload;
    function Read(aTransport: IWSTransport;
      aMaxPayloadSize: QWord): boolean; overload;
    function GetAsBytes : TBytes; virtual;
  Public
    // Read a message from transport. Returns Nil if the connection was closed when reading.
    class function CreateFromStream(aTransport : IWSTransport): TWSFrame; overload;
    class function CreateFromStream(aTransport : IWSTransport;
      aMaxPayloadSize: QWord): TWSFrame; overload;
  public
    constructor Create(aType: TFrameType; aIsFinal: Boolean; APayload: TBytes; aMask : Integer = 0); overload; virtual;
    constructor Create(Const aMessage : UTF8String; aMask : Integer = 0); overload; virtual;
    constructor Create(aType: TFrameType; aIsFinal: Boolean = True; aMask: Integer = 0); overload; virtual;
    property Reserved : Byte read FRSV write FRSV;
    property FinalFrame: Boolean read FFinalFrame write FFinalFrame;
    property Payload : TWSFramePayload Read FPayload Write FPayLoad;
    property FrameType: TFrameType read FFrameType;
    property Reason: WORD read FReason;
    Property AsBytes : TBytes Read GetAsBytes;
  end;
  TWSFrameClass = Class of TWSFrame;

  { TWSMessage }

  TWSMessage = record
  private
    function GetAsString: UTF8String;
    function GetAsUnicodeString: UnicodeString;
  Public
    PayLoad : TBytes;
    Sequences : TFrameSequences;
    IsText : Boolean;
    // Use these only when IsText is true (PayLoad contains valid UTF-8).
    // You may use them when IsText is false, but only if you know there is valid UTF-8 in payload.
    // Return Payload as a UTF8 string
    Property AsString : UTF8String Read GetAsString;
    // Return Payload as a UTF8 string
    Property AsUTF8String : UTF8String Read GetAsString;
    // Return Payload (assumed to contain valid UTF8) as a UTF16 string
    Property AsUnicodeString : UnicodeString Read GetAsUnicodeString;
  end;


  TWSMessageEvent = procedure(Sender: TObject; const aMessage : TWSMessage) of object;
  TWSControlEvent = procedure(Sender: TObject; aType : TFrameType; const aData: TBytes) of object;

  TCloseState = (csNone,csSent,csReceived,csClosed);
  TCloseStates = Set of TCloseState;

  TWSOption = (woPongExplicit,      // Send Pong explicitly, not implicitly.
               woCloseExplicit,     // SeDo Close explicitly, not implicitly.
               woIndividualFrames,  // Send frames one by one, do not concatenate.
               woSkipUpgradeCheck,  // Skip handshake "Upgrade:" HTTP header check.
               woSkipVersionCheck,  // Skip handshake "Sec-WebSocket-Version' HTTP header check.
               woSendErrClosesConn  // Don't raise an exception when writing to a broken connection
              );
  TWSOptions = set of TWSOption;


  { TWSConnection }

  TWSConnection = class
  Private
    class var _ConnectionCount : {$IFDEF CPU64}QWord{$ELSE}Cardinal{$ENDIF};
  private
    FAutoDisconnect: Boolean;
    FConnectionID: String;
    FFreeUserData: Boolean;
    FOnDisconnect: TNotifyEvent;
    FOutgoingFrameMask: Integer;
    FOwner: TComponent;
    FUserData: TObject;
    FWebSocketVersion: Integer;
    FInitialOpcode : TFrameType;
    FMessageContent : TBytes;
    FHandshakeRequest: TWSHandShakeRequest;
    FOnMessageReceived: TWSMessageEvent;
    FOnControl: TWSControlEvent;
    FCloseState : TCloseState;
    FOptions: TWSOptions;
    FMaxFramePayloadSize: QWord;
    FMaxMessagePayloadSize: QWord;
    FWriteLock: TRTLCriticalSection;
    Function GetPeerIP : String;
    Function GetPeerPort : word;
    function GetCloseState: TCloseState;
    function MessageSizeAllowed(aCurrentSize, aAdditionalSize: SizeInt): Boolean;
    procedure PrepareProtocolClose(out aSendClose: Boolean);
    function WriteProtocolClose(aReason: Word): Boolean;
    function WriteFrame(aFrame: TWSFrame;
      out aErrorMessage: String): Boolean;
    function WriteControlFrame(aFrameType: TFrameType; const aData: TBytes;
      out aErrorMessage: String): Boolean;
  protected
    procedure AllocateConnectionID; virtual;
    Procedure SetCloseState(aValue : TCloseState); virtual;
    Procedure DoDisconnect; virtual; abstract;
    // Read message from connection. Return False if connection was closed.
    function DoReadMessage: Boolean;
    procedure DispatchEvent(aInitialType : TFrameType; aFrame: TWSFrame; aMessageContent: TBytes);
    Procedure SetHandShakeRequest(aRequest : TWSHandShakeRequest);
    Function HandleIncoming(aFrame: TWSFrame) : Boolean; virtual;
    function GetHandshakeCompleted: Boolean; virtual; abstract;
    Function GetTransport : IWSTransport; virtual; abstract;
    property Owner : TComponent Read FOwner;
    function IsValidUTF8(aValue: TBytes): boolean;
  Public
    Type
      TConnectionIDAllocator = Procedure(out aID : String) of object;
    class var IDAllocator : TConnectionIDAllocator;
  Public
    Constructor Create(aOwner : TComponent; aOptions : TWSOptions); virtual;
    destructor Destroy; override;
    // Extract close data
    Class Function GetCloseData(aBytes : TBytes; Out aReason : String) : Word;
    // Send close with message data
    procedure Close(aData : TBytes = Nil); overload;
    procedure Close(aMessage : UTF8String); overload;
    procedure Close(aMessage : UTF8String; aReason: word); overload;
    // Check incoming message
    function CheckIncoming(aTimeout: Integer; DoRead : Boolean = True): TIncomingResult;
    // read & process incoming message. Return nil if connection was close.
    function ReadMessage: Boolean;
    // Disconnect
    Procedure Disconnect; inline;
    // Descendents can override this to provide custom frames
    Function FrameClass : TWSFrameClass; virtual;
    // Send raw frame. No checking is done !
    procedure Send(aFrame : TWSFrame); virtual;
    // Send message
    procedure Send(const AMessage: UTF8string);
    // Send binary data
    procedure Send(const ABytes: TBytes);
    // Send control frame. ftPing,ftPong,ftClose
    procedure Send(aFrameType: TFrameType; aData : TBytes = Nil);
    // Disconnect when status is set to csClosed;
    Property AutoDisconnect : Boolean Read FAutoDisconnect Write FAutoDisconnect;
    // Close frame handling
    Property CloseState : TCloseState Read GetCloseState;
    // Connection ID, allocated during create
    Property ConnectionID : String Read FConnectionID;
    // If set to true, the owner data is freed when the connection is freed.
    Property FreeUserData : Boolean Read FFreeUserData Write FFreeUserData;
    // Request headers during handshake
    property HandshakeRequest: TWSHandShakeRequest read FHandshakeRequest;
    // Has handshake been completed ?
    property HandshakeCompleted: Boolean read GetHandshakeCompleted;
    // Options passed by server
    Property Options : TWSOptions Read FOptions;
    // Mask to use when sending frames. Set to nonzero value to send masked frames.
    Property OutgoingFrameMask : Integer Read FOutgoingFrameMask Write FOutgoingFrameMask;
    // Maximum accepted payload of one incoming frame. 0 disables the policy limit.
    property MaxFramePayloadSize: QWord read FMaxFramePayloadSize write FMaxFramePayloadSize;
    // Maximum accepted assembled incoming data message. 0 disables the policy limit.
    property MaxMessagePayloadSize: QWord read FMaxMessagePayloadSize write FMaxMessagePayloadSize;
    // Peer IP address
    property PeerIP: string read GetPeerIP;
    // Peer IP port
    property PeerPort: word read GetPeerPort;
    // Transport in use by this connection
    property Transport: IWSTransport read GetTransport;
    // User data to associate with this connection.
    Property UserData : TObject Read FUserData Write FUserData;
    // Socket version to check for
    Property WebSocketVersion : Integer Read FWebSocketVersion Write FWebSocketVersion;
    // Called when text/binary data was received
    property OnMessageReceived: TWSMessageEvent read FOnMessageReceived write FOnMessageReceived;
    // Called when Ping, Pong, Close control messages come in.
    property OnControl: TWSControlEvent read FOnControl write FOnControl;
    // Called when disconnect is called.
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

  { TWSClientTransport }

  TWSClientTransport = Class(TWSTransport)
  end;

  { TWSClientConnection }

  TWSClientConnection = Class(TWSConnection)
  private
    FTransport : TWSClientTransport;
    FHandshakeResponse: TWSHandShakeResponse;
  Protected
    function GetTransport : IWSTransport ; override;
  public
    Constructor Create(aOwner: TComponent; aTransport : TWSClientTransport; aOptions : TWSOptions); reintroduce; overload;
    Destructor Destroy; override;
    //
    function GetHandshakeCompleted: Boolean; override;
    // Owned by connection
    Property ClientTransport : TWSClientTransport Read FTransport;
    // Non-owning compatibility alias; the assigning code retains ownership.
    Property HandShakeResponse : TWSHandShakeResponse Read FHandshakeResponse Write FHandshakeResponse;
  End;

  { TWSServerTransport }

  TWSServerTransport = class(TWSTransport)
  end;

  { TWSServerConnection }
  TWSConnectionHandshakeEvent =  function(aRequest : TWSHandShakeRequest; aResponse : TWSHandShakeResponse): boolean of object;

  TWSServerConnection = Class(TWSConnection)
  Private
    FExtraHeaders: TStrings;
    FHandshakeResponseSent: Boolean;
    FOnHandShake: TWSConnectionHandshakeEvent;
    FTransport : TWSServerTransport;
  Protected
    Procedure DoDisconnect; override;
    function GetTransport: IWSTransport; override;
    function DoPrepareHandshakeResponse(aRequest : TWSHandShakeRequest; aResponse : TWSHandShakeResponse): boolean; virtual;
    function GetHandshakeCompleted: Boolean; override;
  public
    // Transport is owned by connection
    constructor Create(aOwner : TComponent; aTransport : TWSServerTransport; aOptions : TWSOptions); overload;
    // disconnect
    destructor Destroy; override;
    // Do full circle.
    Procedure PerformHandshake; virtual;
    // Given a request, send response
    function DoHandshake(const aRequest : TWSHandShakeRequest): Boolean; virtual;
    // Has handshake been exchanged?
    property HandshakeResponseSent: Boolean read FHandshakeResponseSent;
    // Extra handshake headers
    Property ExtraHeaders : TStrings Read FExtraHeaders;
    // Owned by connection
    property ServerTransport : TWSServerTransport Read FTransport;
    // Called when exchanging handshake
    Property OnHandshake : TWSConnectionHandshakeEvent Read FOnHandShake write FOnHandshake;
  end;

Type

  { TBytesHelper }

  TBytesHelper = Type helper for TBytes
    // No swapping of bytes
    Function ToDword(aOffset : Integer = 0) : DWORD;
    Function ToInt32(aOffset : Integer = 0) : LongInt;
    Function ToWord(aOffset : Integer = 0) : Word;
    Function ToQWord(aOffset : Integer = 0) : QWord;
    Procedure FromDword(const aData : DWORD; aOffset : Integer = 0);
    Procedure FromInt32(const aData : Longint; aOffset : Integer = 0);
    Procedure FromWord(const aData : Word; aOffset : Integer = 0);
    Procedure FromQWord(const aData : QWord; aOffset : Integer = 0);
    procedure Reverse(var Dest: TBytes; Offset: Integer; Size: Integer);
    Function Reverse(Offset: Integer; Size: Integer) : TBytes;
    Procedure Append(aData : TBytes);
  end;


Resourcestring
  SErrNotSimpleOperation = 'Frame type %d is not a simple operation.';
  SErrCloseAlreadySent = 'Close message already sent, cannot send more data.';
  SErrHandshakeInComplete = 'Operation cannot be performed while the handshake is not completed';
  SErrConnectionActive = 'Operation cannot be performed while the websocket connection is active';
  SErrConnectionInActive = 'Operation cannot be performed while the websocket connection is not active';
  SErrServerActive = 'Operation cannot be performed while the websocket connection is active';
  SErrInvalidSizeFlag = 'Invalid size flag: %d';
  SErrInvalidFrameType = 'Invalid frame type flag: %d';
  SErrWriteReturnedError = 'Write operation returned error: (%d) %s';
  SErrWriteClosed = 'Write operation returned no data; the connection is closed';
  SErrReadReturnedError = 'Read operation returned error: (%d) %s';
  SErrReadClosed = 'The peer closed the WebSocket connection';
  SErrInvalidReadCount = 'Transport returned invalid read count %d for a %d-byte request';
  SErrInvalidWriteCount = 'Transport returned invalid write count %d for a %d-byte request';
  SErrNegativeBufferSize = 'Negative WebSocket buffer size: %d';
  SErrHandshakeLineTooLong = 'WebSocket handshake line exceeds the transport limit';
  SErrHandshakeHeadersTooLarge = 'WebSocket handshake headers exceed the transport limit';
  SErrNonCanonicalLength = 'Non-canonical WebSocket payload length encoding';
  SErrInvalidPayloadLength = 'Invalid WebSocket payload length';
  SErrFramePayloadTooLarge = 'WebSocket frame payload is too large (%s bytes)';
  SErrControlPayloadTooLarge = 'WebSocket control frame payload exceeds 125 bytes';
  SErrPayloadLengthMismatch = 'WebSocket payload length does not match its data buffer';
  SErrInvalidFrameMask = 'Invalid WebSocket frame masking for this endpoint';
  SErrConnectionClosing = 'Cannot send WebSocket data after the close handshake has started';
  SErrReadInterrupted = 'WebSocket read interrupted';
  SErrConcurrentRead = 'Concurrent reads on one WebSocket transport are not supported';

function DecodeBytesBase64(const s: string; Strict: boolean = false) : TBytes;
function EncodeBytesBase64(const aBytes : TBytes) : String;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.StrUtils, System.Hash.Sha1, System.Hash.Base64;
{$ELSE FPC_DOTTEDUNITS}
uses strutils, sha1, base64;
{$ENDIF FPC_DOTTEDUNITS}

Const
  WSReadIdle = 0;
  WSReadActive = 1;
  WSReadInterrupting = 2;
  WSMaxTransportLineBytes = 64 * 1024;
  WSMaxServerHandshakeLineBytes = 8192;
  WSMaxServerHandshakeLines = 256;
  WSMaxServerHandshakeBytes = 64 * 1024;

{ EWSTransportError }

constructor EWSTransportError.Create(const aMessage: String;
  aErrorCode: LongInt);
begin
  FErrorCode:=aErrorCode;
  inherited Create(aMessage);
end;

{ EWSProtocolError }

constructor EWSProtocolError.Create(const aMessage: String; aCloseCode: Word);
begin
  FCloseCode:=aCloseCode;
  inherited Create(aMessage);
end;

function WSLastSocketError: LongInt; inline;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.SocketError;
end;

function WSValidCloseCode(aCode: Word): Boolean; inline;
begin
  Result:=((aCode>=CLOSE_NORMAL_CLOSURE) and
           (aCode<=1014) and
           (aCode<>CLOSE_RESERVER) and
           (aCode<>CLOSE_NO_STATUS_RCVD) and
           (aCode<>CLOSE_ABNORMAL_CLOSURE)) or
          ((aCode>=3000) and (aCode<=4999));
end;

procedure WSRaiseReadError;
var
  ErrorCode: LongInt;
begin
  { SocketError must be sampled before formatting or allocating anything. }
  ErrorCode:=WSLastSocketError;
  raise EWSReadError.Create(
    Format(SErrReadReturnedError,[ErrorCode,SysErrorMessage(ErrorCode)]),
    ErrorCode);
end;

procedure WSReadExact(aTransport: IWSTransport; var aBytes: TBytes;
  aCount: Integer);
var
  Chunk: TBytes;
  BytesRead: Integer;
  ReadPosition: Integer;
  Remaining: Integer;
begin
  if aCount<0 then
    raise EWebSocket.CreateFmt(SErrNegativeBufferSize,[aCount]);
  SetLength(aBytes,aCount);
  if aCount=0 then
    Exit;
  if not Assigned(aTransport) then
    raise EWSConnectionClosed.Create(SErrReadClosed,0);

  ReadPosition:=0;
  while ReadPosition<aCount do
    begin
    Remaining:=aCount-ReadPosition;
    SetLength(Chunk,Remaining);
    BytesRead:=aTransport.ReadBytes(Chunk,Remaining);
    if BytesRead=0 then
      raise EWSConnectionClosed.Create(SErrReadClosed,0);
    if BytesRead<0 then
      WSRaiseReadError;
    if (BytesRead>Remaining) or (Length(Chunk)<BytesRead) then
      raise EWSReadError.Create(
        Format(SErrInvalidReadCount,[BytesRead,Remaining]),0);
    Move(Chunk[0],aBytes[ReadPosition],BytesRead);
    Inc(ReadPosition,BytesRead);
    end;
end;

{$IFDEF MSWINDOWS}
Type
  TCancelIoExProc = function(aHandle : PtrUInt;
    aOverlapped : Pointer) : LongBool; stdcall;

function WSGetModuleHandleA(aModuleName : PAnsiChar) : PtrUInt; stdcall;
  external 'kernel32.dll' name 'GetModuleHandleA';
function WSGetProcAddress(aModule : PtrUInt;
  aProcName : PAnsiChar) : Pointer; stdcall;
  external 'kernel32.dll' name 'GetProcAddress';
{$ENDIF MSWINDOWS}

function WakeSocketRead(aSocket : TSocket) : Boolean;
{$IFDEF MSWINDOWS}
Var
  KernelModule : PtrUInt;
  CancelIO : TCancelIoExProc;
begin
  { Winsock shutdown disables later receives but does not release the receive
    already blocked on another thread. Do it first so that, after CancelIoEx
    releases the current call, neither OpenSSL nor the frame reader can block
    by retrying the same socket. This is raw socket state only: the descriptor
    stays open and no TLS object is touched or freed here. }
  {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.fpShutdown(
    aSocket,{$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.SHUT_RDWR);

  { Resolve CancelIoEx dynamically so merely linking fpwebsocket does not add
    a hard dependency on that entry point on older Windows versions. }
  KernelModule:=WSGetModuleHandleA('kernel32.dll');
  if KernelModule=0 then
    Exit(False);
  CancelIO:=TCancelIoExProc(WSGetProcAddress(KernelModule,'CancelIoEx'));
  if not Assigned(CancelIO) then
    Exit(False);
  Result:=CancelIO(PtrUInt(aSocket),Nil);
end;
{$ELSE MSWINDOWS}
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.fpShutdown(
    aSocket,{$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.SHUT_RDWR)=0;
end;
{$ENDIF MSWINDOWS}

{ TFrameTypeHelper }

function TFrameTypeHelper.GetAsFlag: Byte;

Const
  Flags : Array[TFrameType] of byte = (FlagContinuation,FlagText,FlagBinary,FlagClose,FlagPing,FlagPong,FlagReserved);

begin
  Result:=Flags[Self];
end;

procedure TFrameTypeHelper.SetAsFlag(AValue: Byte);
begin
  case aValue of
    FlagContinuation : Self:=ftContinuation;
    FlagText :         Self:=ftText;
    FlagBinary :       Self:=ftBinary;
    FlagClose :        Self:=ftClose;
    FlagPing :         Self:=ftPing;
    FlagPong :         Self:=ftPong;
  else
    Self:=ftFutureOpcodes;
  end;
end;


{ TWSServerTransport }


{ TWSHandShakeResponse }

constructor TWSHandShakeResponse.Create(const aResource: string; const aExtraHeaders: TStrings);
begin
  inherited Create(aResource, aExtraHeaders);
  HTTPVersion:='1.1';
  StatusCode:=101;
  StatusText:='Switching Protocols';
end;

procedure TWSHandShakeResponse.ToStrings(aHandShake: TWSHandshakeRequest; aResponse: TStrings; AddStatusLine: Boolean);

  Function CalcKey : String;

  Var
    B : TBytes;
    {%H-}hash : TSHA1Digest;
    K : string;
  begin
    // respond key
    b:=[];
    k:= Trim(aHandshake.Key) + SSecWebSocketGUID;
    hash:=SHA1String(k);
    SetLength(B,SizeOf(hash));
    Move(Hash,B[0],Length(B));
    Result:=EncodeBytesBase64(B);
  end;

begin
  // Fill needed headers
  Upgrade:='websocket';
  Connection:='Upgrade';
  // Chrome doesn't like it if you send an empty protocol header.
  if (Protocol='') and (aHandshake.Protocol<>'') then
    Protocol:=aHandshake.Protocol;
  if Version='' then
    Version:=IntToStr(DefaultWebSocketVersion);
  if Accept='' then
    Accept:=CalcKey;
  if AddStatusLine then
    aResponse.Add('HTTP/%s %d %s',[HTTPVersion,StatusCode,StatusText]);
  aResponse.AddStrings(RawHeaders);
end;

{ TWSTransport }

function TWSTransport.GetSocket: TSocketStream;
begin
  Result:=FHelper.Socket;
end;

constructor TWSTransport.Create(aStream : TSocketStream);
begin
  FStream:=aStream;
  FHelper:=TWSSocketHelper.Create(FStream);
end;

destructor TWSTransport.Destroy;
begin
  FreeAndNil(FHelper);
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TWSTransport.CloseSocket;
begin
  { CancelReads performs a bidirectional shutdown, which both wakes blocked
    I/O and makes subsequent I/O fail.  The owning TSocketStream closes the
    descriptor exactly once from TWSTransport.Destroy; closing the raw handle
    here as well could later close an unrelated descriptor after handle
    reuse. }
  if Assigned(FHelper) then
    FHelper.CancelReads;
end;

procedure TWSTransport.InterruptRead;
begin
  if Assigned(FHelper) then
    FHelper.InterruptRead;
end;

procedure TWSTransport.CancelReads;
begin
  if Assigned(FHelper) then
    FHelper.CancelReads;
end;

{ TWSTransport }

constructor TWSSocketHelper.Create(aSocket: TSocketStream);
begin
  FSocket:=aSocket;
  FReadState:=WSReadIdle;
  FReadsCancelled:=0;
  InitCriticalSection(FWriteLock);
{$if defined(FreeBSD) or defined(Linux)}
  FSocket.ReadFlags:=MSG_NOSIGNAL;
  FSocket.WriteFlags:=MSG_NOSIGNAL;
{$endif}
end;

destructor TWSSocketHelper.Destroy;
begin
  DoneCriticalSection(FWriteLock);
  inherited Destroy;
end;

procedure TWSSocketHelper.BeginRead;
var
  PreviousState: LongInt;
begin
  if InterlockedCompareExchange(FReadsCancelled,0,0)<>0 then
    Raise EWSReadInterrupted.Create(SErrReadInterrupted);
  PreviousState:=InterlockedCompareExchange(FReadState,WSReadActive,
    WSReadIdle);
  if PreviousState<>WSReadIdle then
    begin
    if (PreviousState=WSReadInterrupting) or
       (InterlockedCompareExchange(FReadsCancelled,0,0)<>0) then
      Raise EWSReadInterrupted.Create(SErrReadInterrupted);
    Raise EWebSocket.Create(SErrConcurrentRead);
    end;
  { Close the race in which terminal cancellation is published immediately
    after the first check but before this read claims FReadState. }
  if InterlockedCompareExchange(FReadsCancelled,0,0)<>0 then
    begin
    InterlockedCompareExchange(FReadState,WSReadIdle,WSReadActive);
    Raise EWSReadInterrupted.Create(SErrReadInterrupted);
    end;
end;

procedure TWSSocketHelper.EndRead;
Var
  PreviousState : LongInt;
begin
  { The state transition made by InterruptRead is itself the interruption
    publication. This single atomic exchange closes the former window between
    claiming a read and publishing a separate interruption flag. }
  PreviousState:=InterlockedExchange(FReadState,WSReadIdle);
  if (PreviousState=WSReadInterrupting) or
     (InterlockedCompareExchange(FReadsCancelled,0,0)<>0) then
    Raise EWSReadInterrupted.Create(SErrReadInterrupted);
end;

function TWSSocketHelper.ReadSocket(var aBuffer; aCount: LongInt): LongInt;
var
  ErrorCode: LongInt;
  ReadResult: LongInt;
begin
  ErrorCode:=0;
  BeginRead;
  try
    ReadResult:=FSocket.Read(aBuffer,aCount);
    if ReadResult<0 then
      { Capture this before EndRead performs any synchronization operation. }
      ErrorCode:=WSLastSocketError;
  finally
    EndRead;
  end;
  if ReadResult<0 then
    raise EWSReadError.Create(
      Format(SErrReadReturnedError,
        [ErrorCode,SysErrorMessage(ErrorCode)]),ErrorCode);
  Result:=ReadResult;
end;

procedure TWSSocketHelper.ReadSocketBuffer(var aBuffer; aCount: LongInt);
var
  Buffer: TBytes;
  BytesRead: LongInt;
  ReadPosition: LongInt;
begin
  if aCount<0 then
    raise EWebSocket.CreateFmt(SErrNegativeBufferSize,[aCount]);
  if aCount=0 then
    Exit;
  SetLength(Buffer,aCount);
  ReadPosition:=0;
  while ReadPosition<aCount do
    begin
    BytesRead:=ReadSocket(Buffer[ReadPosition],aCount-ReadPosition);
    if BytesRead=0 then
      raise EWSConnectionClosed.Create(SErrReadClosed,0);
    if BytesRead>aCount-ReadPosition then
      raise EWSReadError.Create(
        Format(SErrInvalidReadCount,[BytesRead,aCount-ReadPosition]),0);
    Inc(ReadPosition,BytesRead);
    end;
  Move(Buffer[0],aBuffer,aCount);
end;

function TWSSocketHelper.WriteSocket(const aBuffer; aCount: LongInt): LongInt;
var
  ErrorCode: LongInt;
begin
  Result:=FSocket.Write(aBuffer,aCount);
  if Result<0 then
    begin
    { Capture this before releasing the surrounding write lock. }
    ErrorCode:=WSLastSocketError;
    raise EWSWriteError.Create(
      Format(SErrWriteReturnedError,
        [ErrorCode,SysErrorMessage(ErrorCode)]),ErrorCode);
    end;
end;

procedure TWSSocketHelper.InterruptRead;
Var
  PreviousState : LongInt;
begin
  { Claim only a transport whose reader is currently inside a socket read.
    A pump may serve several connections, so interrupting every registered
    socket would unnecessarily break healthy siblings. }
  PreviousState:=InterlockedCompareExchange(FReadState,WSReadInterrupting,
    WSReadActive);
  if (PreviousState<>WSReadActive) and
     (PreviousState<>WSReadInterrupting) then
    Exit;

  { Keep WSReadInterrupting set until EndRead observes it and raises. Repeated
    termination passes may retry the platform wake, but the connection can no
    longer return to the pump as healthy after its socket has been shut down. }
  WakeSocketRead(FSocket.Handle);
end;

procedure TWSSocketHelper.CancelReads;
begin
  { Unlike InterruptRead, this is a terminal per-session cancellation. It is
    used only after the owning session has been deregistered, and deliberately
    prevents a reader from entering a later exact-read chunk. }
  InterlockedExchange(FReadsCancelled,1);
  InterlockedExchange(FReadState,WSReadInterrupting);
  { Shutting down while idle makes the cancellation durable at the socket
    layer too. The descriptor remains owned by TWSTransport. }
  WakeSocketRead(FSocket.Handle);
end;

function TWSSocketHelper.CanRead(aTimeOut: Integer): Boolean;
begin
  Result:=FSocket.CanRead(aTimeout);
end;

function TWSSocketHelper.PeerIP: string;

  Function SocketAddrToString(ASocketAddr: TSockAddr): String;
  begin
    if ASocketAddr.sa_family = AF_INET then
      Result := NetAddrToStr(ASocketAddr.sin_addr)
    else // no ipv6 support yet
      Result := '';
  end;

begin
  Result:= SocketAddrToString(FSocket.RemoteAddress);
end;

function TWSSocketHelper.PeerPort: word;

  Function SocketAddrToPort(ASocketAddr: TSockAddr): word;
  begin
    if ASocketAddr.sa_family = AF_INET then
      Result := ASocketAddr.sin_port
    else // no ipv6 support yet
      Result := 0;
  end;

begin
  Result:=SocketAddrToPort(FSocket.RemoteAddress);
end;

function TWSSocketHelper.ReadLn: String;

Var
  C : Byte;
  BytesRead,
  aSize : integer;

begin
  // Preset
  Result:='';
  SetLength(Result,255);
  aSize:=0;
  C:=0;
  repeat
    BytesRead:=ReadSocket(C,1);
    if BytesRead=0 then
      raise EWSConnectionClosed.Create(SErrReadClosed,0);
    if BytesRead<>1 then
      raise EWSReadError.Create(
        Format(SErrInvalidReadCount,[BytesRead,1]),0);
    if C=10 then
      Break;
    if aSize>=WSMaxTransportLineBytes then
      raise EWSHandShake.Create(SErrHandshakeLineTooLong);
    Inc(aSize);
    if aSize>Length(Result) then
      SetLength(Result,Length(Result)+255);
    Result[aSize]:=AnsiChar(C);
  until False;
  if (aSize>0) and (Result[aSize]=#13) then
    Dec(aSize);
  SetLength(Result,aSize);
end;

function TWSSocketHelper.ReadBytes(var aBytes: TBytes; aCount: Integer): Integer;
begin
  if aCount<0 then
    raise EWebSocket.CreateFmt(SErrNegativeBufferSize,[aCount]);
  SetLength(aBytes,aCount);
  if aCount=0 then
    Exit(0);
  Result:=ReadSocket(aBytes[0],aCount);
  if Result>aCount then
    raise EWSReadError.Create(
      Format(SErrInvalidReadCount,[Result,aCount]),0);
  SetLength(aBytes,Result);
end;

procedure TWSSocketHelper.ReadBuffer(aBytes: TBytes);
begin
  if Length(ABytes)=0 then exit;
  ReadSocketBuffer(aBytes[0],Length(ABytes));
end;

function TWSSocketHelper.WriteBytes(aBytes: TBytes; aCount: Integer): Integer;
begin
  if (aCount<0) or (aCount>Length(aBytes)) then
    raise EWSWriteError.Create(
      Format(SErrInvalidWriteCount,[aCount,Length(aBytes)]),0);
  if aCount=0 then
    Exit(0);
  EnterCriticalSection(FWriteLock);
  try
    Result:=WriteSocket(aBytes[0],aCount);
    if Result>aCount then
      raise EWSWriteError.Create(
        Format(SErrInvalidWriteCount,[Result,aCount]),0);
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

procedure TWSSocketHelper.WriteBuffer(aBytes: TBytes);
var
  BytesWritten: LongInt;
  WriteCount: LongInt;
  WritePosition: SizeInt;
begin
  if Length(aBytes)=0 then
    Exit;
  EnterCriticalSection(FWriteLock);
  try
    WritePosition:=0;
    while WritePosition<Length(aBytes) do
      begin
      if Length(aBytes)-WritePosition>High(LongInt) then
        WriteCount:=High(LongInt)
      else
        WriteCount:=LongInt(Length(aBytes)-WritePosition);
      BytesWritten:=WriteSocket(aBytes[WritePosition],WriteCount);
      if BytesWritten=0 then
        raise EWSWriteError.Create(SErrWriteClosed,0);
      if BytesWritten>WriteCount then
        raise EWSWriteError.Create(
          Format(SErrInvalidWriteCount,
            [BytesWritten,WriteCount]),0);
      Inc(WritePosition,BytesWritten);
      end;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

{ TWSMessage }

function TWSMessage.GetAsString: UTF8String;
begin
  Result:=UTF8String(TEncoding.UTF8.GetString(Payload));
end;

function TWSMessage.GetAsUnicodeString: UnicodeString;
begin
  Result:=UTF8Decode(asUTF8String);
end;

{ TBytesHelper }

function TBytesHelper.Reverse(Offset: Integer; Size: Integer): TBytes;

begin
  Result:=[];
  Reverse(Result,Offset,Size);
end;

procedure TBytesHelper.Append(aData: TBytes);

Var
  sLen,dLen : SizeInt;

begin
  sLen:=Length(Self);
  dLen:=Length(aData);
  if dLen>0 then
    begin
    SetLength(Self,dLen+sLen);
    Move(aData[0],Self[sLen],dLen);
    end;
end;

procedure TBytesHelper.Reverse(var Dest: TBytes; Offset: Integer; Size: Integer);
var
  I: Integer;
begin
  SetLength(dest, Size);
  for I := 0 to Size - 1 do
    Dest[Size-1-I]:=Self[Offset+I];
end;

function TBytesHelper.ToInt32(aOffset: Integer = 0): LongInt;
begin
  Result:=0;
  Move(Self[aOffSet],Result,SizeOf(LongInt));
end;

function TBytesHelper.ToDword(aOffset: Integer): DWORD;
begin
  Result:=0;
  Move(Self[aOffSet],Result,SizeOf(DWORD));
end;

function TBytesHelper.ToWord(aOffset: Integer): Word;
begin
  Result:=0;
  Move(Self[aOffSet],Result,SizeOf(Word));
end;

function TBytesHelper.ToQWord(aOffset: Integer): QWord;
begin
  Result:=0;
  Move(Self[aOffSet],Result,SizeOf(QWord));
end;

procedure TBytesHelper.FromDword(const aData: DWORD; aOffset: Integer);
begin
  Move(aData, Self[aOffSet],SizeOf(DWORD));
end;


procedure TBytesHelper.FromInt32(const aData: Longint; aOffset: Integer);

begin
  Move(aData, Self[aOffSet],SizeOf(Longint));
end;

procedure TBytesHelper.FromWord(const aData: Word; aOffset: Integer = 0);
begin
  Move(aData, Self[aOffSet],SizeOf(Word));
end;

procedure TBytesHelper.FromQWord(const aData: QWord; aOffset: Integer);
begin
  Move(aData, Self[aOffSet],SizeOf(QWord));
end;

Function HToNx(Host: QWord) : QWord;

begin
{$ifdef FPC_BIG_ENDIAN}
  htonx:=host;
{$else}
  htonx:=SwapEndian(host);
{$endif}
end;

Function NToHx(Net: QWord) : QWord;

begin
{$ifdef FPC_BIG_ENDIAN}
  ntohx:=Net;
{$else}
  ntohx:=SwapEndian(Net);
{$endif}
end;

function EncodeBytesBase64(const aBytes : TBytes) : String;

var
  OutStream : TStringStream;
  Encoder   : TBase64EncodingStream;

begin
  if Length(aBytes)=0 then
    Exit('');
  Encoder:=Nil;
  OutStream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.Create(OutStream);
    if Length(aBytes)>0 then
      Encoder.WriteBuffer(aBytes[0],Length(aBytes));
    Encoder.Flush;
    Result:=OutStream.DataString;
  finally
    Encoder.Free;
    OutStream.free;
  end;
end;


function DecodeBytesBase64(const s: string; Strict: boolean = false) : TBytes;

Const
  StrictModes : Array[Boolean] of TBase64DecodingMode = (bdmMime,bdmStrict);

var
  missing : Integer;
  SD : String;
  Instream,
  Outstream : TBytesStream;
  Decoder   : TBase64DecodingStream;

begin
  Result:=[];
  if Length(s)=0 then
    Exit;
  SD:=S;
  Missing:=Length(Sd) mod 4;
  if Missing>0 then
    SD:=SD+StringOfChar('=',Missing);
  Outstream:=Nil;
  Decoder:=Nil;
  Instream:=TStringStream.Create(SD);
  try
    Outstream:=TBytesStream.Create(Nil);
    Decoder:=TBase64DecodingStream.Create(Instream,StrictModes[Strict]);
    Outstream.CopyFrom(Decoder,Decoder.Size);
    Result:=Outstream.Bytes;
  finally
    Decoder.Free;
    Outstream.Free;
    Instream.Free;
  end;
end;



{ TWSFramePayload }


procedure TWSFramePayload.ReadData(var Content: TBytes; aTransport: IWSTransport);

Const
  MaxBufSize = 32*1024;

Var
  Buf : TBytes;
  ReadPosition,
  ToRead : QWord;
  ReadCount : Longint;

begin
  Buf:=[];
  ToRead:=DataLength;
  ReadPosition:=0;
  while ToRead>0 do
    begin
    if ToRead>MaxBufSize then
      ReadCount:=MaxBufSize
    else
      ReadCount:=LongInt(ToRead);
    WSReadExact(aTransport,Buf,ReadCount);
    Move(Buf[0],Content[SizeInt(ReadPosition)],ReadCount);
    Inc(ReadPosition,QWord(ReadCount));
    Dec(ToRead,QWord(ReadCount));
    end;
end;

procedure TWSFramePayload.Read(buffer: TBytes; aTransport: IWSTransport);
begin
  Read(buffer,aTransport,0);
end;

procedure TWSFramePayload.Read(buffer: TBytes; aTransport: IWSTransport;
  aMaxPayloadSize: QWord);

Var
  LenFlag : Byte;
  PayLen16 : Word;
  EncodedLength,
  Content: TBytes;

begin
  if Length(Buffer)<2 then
    raise EWSProtocolError.Create(SErrInvalidPayloadLength,
      CLOSE_PROTOCOL_ERROR);
  content:=[];
  Data:=[];
  MaskKey:=0;
  Masked := ((buffer[1] and FlagMasked) <> 0);
  LenFlag := buffer[1] and FlagLengthMask;

  Case LenFlag of
  FlagTwoBytes:
    begin
    WSReadExact(aTransport,EncodedLength,2);
    Paylen16:=EncodedLength.ToWord(0);
    DataLength := ntohs(PayLen16);
    if DataLength<FlagTwoBytes then
      raise EWSProtocolError.Create(SErrNonCanonicalLength,
        CLOSE_PROTOCOL_ERROR);
    end;
  FlagEightBytes:
    begin
    WSReadExact(aTransport,EncodedLength,8);
    { RFC 6455 payload lengths are 63-bit unsigned integers. }
    if (EncodedLength[0] and $80)<>0 then
      raise EWSProtocolError.Create(SErrInvalidPayloadLength,
        CLOSE_PROTOCOL_ERROR);
    DataLength:=EncodedLength.ToQWord(0);
    DataLength:=ntohx(DataLength);
    if DataLength<(QWord(1) shl 16) then
      raise EWSProtocolError.Create(SErrNonCanonicalLength,
        CLOSE_PROTOCOL_ERROR);
    end
  else
    DataLength:=lenFlag;
  end;

  if (aMaxPayloadSize<>0) and (DataLength>aMaxPayloadSize) then
    raise EWSProtocolError.Create(
      Format(SErrFramePayloadTooLarge,[IntToStr(Int64(DataLength))]),
      CLOSE_MESSAGE_TOO_BIG);
  if DataLength>QWord(High(SizeInt)) then
    raise EWSProtocolError.Create(
      Format(SErrFramePayloadTooLarge,[IntToStr(Int64(DataLength))]),
      CLOSE_MESSAGE_TOO_BIG);

  if Masked then
    begin
    WSReadExact(aTransport,EncodedLength,4);
    MaskKey:=EncodedLength.ToDword(0);
    end;
  SetLength(content,SizeInt(DataLength));
  if (DataLength>0) then
    begin
    ReadData(Content,aTransport);
    if Masked then
      DoMask(Content, MaskKey);
    Data := content;
    end;
end;


{ TWSFrame }

constructor TWSFrame.Create(aType: TFrameType; aIsFinal: Boolean; APayload: TBytes; aMask: Integer=0);

begin
  Create(aType,aIsFinal,aMask);

  FPayload.Data := APayload;
  if Assigned(aPayload) then
    FPayload.DataLength := QWord(Length(aPayload));
end;

constructor TWSFrame.Create(aType: TFrameType; aIsFinal : Boolean = True; aMask: Integer=0);

begin
  FReason:=CLOSE_NORMAL_CLOSURE;
  FPayload:=Default(TWSFramePayload);
  FPayload.MaskKey:=aMask;
  FPayload.Masked:=aMask<>0;
  FFrameType := aType;
  FFinalFrame := AIsFinal;
end;

constructor TWSFrame.Create(const aMessage: UTF8String; aMask: Integer=0);

Var
  Data : TBytes;

begin
  Data:=TEncoding.UTF8.GetBytes(UnicodeString(AMessage));
  Create(ftText,True,Data,aMask);
end;

class function TWSFrame.CreateFromStream(aTransport : IWSTransport): TWSFrame;
begin
  Result:=CreateFromStream(aTransport,0);
end;

class function TWSFrame.CreateFromStream(aTransport : IWSTransport;
  aMaxPayloadSize: QWord): TWSFrame;
begin
  Result:=TWSFrame.Create;
  try
    if not Result.Read(aTransport,aMaxPayloadSize) then
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


function TWSFrame.Read(aTransport: IWSTransport): boolean;
begin
  Result:=Read(aTransport,0);
end;

function TWSFrame.Read(aTransport: IWSTransport;
  aMaxPayloadSize: QWord): boolean;
Var
  Buffer : Tbytes;
  B1,
  LenFlag : Byte;

begin
  Result:=False;
  Buffer:=Default(TBytes);
  try
    WSReadExact(aTransport,Buffer,2);
  except
    { EOF before a new frame is the normal terminal indication expected by
      CheckIncoming. EOF after a partial fixed field is also terminal and the
      incomplete frame is never exposed. }
    on E: EWSConnectionClosed do
      Exit;
  end;
  B1:=buffer[0];
  FFinalFrame:=(B1 and FlagFinalFrame) = FlagFinalFrame;
  FRSV:=(B1 and %01110000) shr 4;
  FFrameType.AsFlag:=(B1 and $F);
  if (FRSV<>0) or (FFrameType=ftFutureOpcodes) then
    raise EWSProtocolError.Create(
      Format(SErrInvalidFrameType,[B1 and $F]),CLOSE_PROTOCOL_ERROR);
  LenFlag:=Buffer[1] and FlagLengthMask;
  if FFrameType in [ftClose,ftPing,ftPong] then
    begin
    if not FFinalFrame then
      raise EWSProtocolError.Create(
        Format(SErrInvalidFrameType,[B1 and $F]),CLOSE_PROTOCOL_ERROR);
    if LenFlag>125 then
      raise EWSProtocolError.Create(SErrControlPayloadTooLarge,
        CLOSE_PROTOCOL_ERROR);
    end;
  FPayload.Read(Buffer,aTransport,aMaxPayloadSize);
  if FFrameType=ftClose then
    FReason:=CLOSE_NO_STATUS_RCVD
  else
    FReason:=CLOSE_NORMAL_CLOSURE;
  if FFrameType=ftClose then
    if FPayload.DataLength = 1 then
      raise EWSProtocolError.Create(SErrInvalidPayloadLength,
        CLOSE_PROTOCOL_ERROR)
    else
    if FPayload.DataLength>1 then
    begin
      FReason:=NToHs(FPayload.Data.ToWord(0));
      FPayload.DataLength := FPayload.DataLength - 2;
      if FPayload.DataLength > 0 then
        move(FPayload.Data[2],FPayload.Data[0],
          SizeInt(FPayload.DataLength));
      SetLength(FPayload.Data,SizeInt(FPayload.DataLength));
    end;
  Result:=True;
end;


function TWSFrame.GetAsBytes: TBytes;

var
  LenByte,firstByte: Byte;
  buffer, LengthBytes: TBytes;
  aOffSet, I : Integer;
  pLen16 : Word;
  pLen64 : QWord;

begin
  Result:=Nil;
  if FPayload.DataLength<>QWord(Length(FPayload.Data)) then
    raise EWSProtocolError.Create(SErrPayloadLengthMismatch,
      CLOSE_PROTOCOL_ERROR);
  if (FrameType in [ftClose,ftPing,ftPong]) and
     (FPayload.DataLength>125) then
    raise EWSProtocolError.Create(SErrControlPayloadTooLarge,
      CLOSE_PROTOCOL_ERROR);
  if FPayload.DataLength>QWord(High(SizeInt)-14) then
    raise EWSProtocolError.Create(
      Format(SErrFramePayloadTooLarge,
        [IntToStr(Int64(FPayload.DataLength))]),CLOSE_MESSAGE_TOO_BIG);
  firstByte := FrameType.AsFlag;
  if FinalFrame then
    firstByte := firstByte or FlagFinalFrame;
  if FPayload.DataLength < FlagTwoBytes then
  begin
    aOffSet:=2;
    LenByte := Byte(FPayload.DataLength);
    LengthBytes:=[];
  end
  else if Payload.DataLength < (1 shl 16) then
  begin
    aOffset:=4;
    LenByte := FlagTwoBytes;
    plen16:=Payload.DataLength;
    SetLength(LengthBytes, SizeOf(Word));
    LengthBytes.FromWord(HToNs(pLen16));
  end
  else
  begin
    aOffset:=10;
    LenByte:=FlagEightBytes;
    plen64:=Payload.DataLength;
    SetLength(LengthBytes, Sizeof(UInt64));
    LengthBytes.FromQWord(HToNx(Plen64));
  end;

  Buffer:=[];
  if FPayload.Masked then
    begin
    lenByte:=Lenbyte or FlagMasked;
    aoffSet:=aOffSet+4;
    end;
  SetLength(buffer,aOffset+SizeInt(FPayload.DataLength));
  buffer[0] := firstByte;
  buffer[1] := LenByte;
  for I := 0 to Length(LengthBytes)-1 do
    buffer[2 + I] := LengthBytes[I];
  if Payload.Masked then
    begin
    Buffer.FromInt32(Payload.MaskKey,aOffSet-4);
    TWSFramePayload.CopyMasked(Payload.Data,Buffer,Payload.MaskKey,aOffset);
    end
  else
    if Payload.DataLength > 0 then
      move(Payload.Data[0],buffer[aOffset],
        SizeInt(Payload.DataLength));
  Result := Buffer;
end;

class procedure TWSFramePayload.DoMask(var aData: TBytes; Key: DWORD);


begin
  CopyMasked(aData,aData,Key,0)
end;

class procedure TWSFramePayload.CopyMasked(SrcData: TBytes; var DestData: TBytes; Key: DWORD; aOffset: Integer);

var
  currentMaskIndex: SizeInt;
  byteKeys: TBytes;
  I: SizeInt;

begin
  CurrentMaskIndex := 0;
  byteKeys:=[];
  SetLength(byteKeys, SizeOf(Key));
  ByteKeys.FromDword(Key);
  for I := 0 to Length(SrcData) - 1 do
    begin
    DestData[I+aOffset] := SrcData[I] XOR byteKeys[currentMaskIndex];
    currentMaskIndex := (currentMaskIndex + 1) mod 4;
    end;
end;

class function TWSFramePayload.CopyMasked(SrcData: TBytes; Key: DWORD): TBytes;
begin
  Result:=[];
  SetLength(Result,Length(SrcData));
  CopyMasked(SrcData,Result,Key,0)
end;

{ TWSRequest }

function TWSHeaders.GetS(aIdx: Integer): String;
begin
  Result:=GetH(WSHeaderNames[aIdx]);
end;

procedure TWSHeaders.SetS(AIndex: Integer; const AValue: string);
begin
  SetH(WSHeaderNames[aIndex],aValue);
end;

function TWSHeaders.GetH(const aName: string): String;
begin
  Result:=Trim(FRawHeaders.Values[aName]);
end;

Procedure TWSHeaders.SetH(const aName,aValue: string);
begin
  FRawHeaders.Values[aName]:=' '+aValue;
end;

constructor TWSHeaders.Create(Const aResource : String; const AHeaderList: TStrings);

var
  I : Integer;
  N,V : String;

begin
  FResource:=aResource;
  FRawHeaders:=TStringList.Create;
  FRawHeaders.NameValueSeparator:=':';
  if Assigned(aHeaderList) then
    for I:=0 to aHeaderList.Count-1 do
      begin
      aHeaderList.GetNameValue(I,N,V);
      if (N<>'') and (V<>'') then
        FRawHeaders.Add(N+': '+Trim(V));
      end;
end;

destructor TWSHeaders.Destroy;
begin
  FreeAndNil(FRawHeaders);
  inherited;
end;

{ TWSConnection }

procedure TWSConnection.Send(aFrameType : TFrameType; aData : TBytes = Nil);

Var
  aFrame : TWSFrame;

begin
  if not (aFrameType in [ftClose,ftPing,ftPong]) then
    Raise EWebSocket.CreateFmt(SErrNotSimpleOperation,[Ord(aFrameType)]);
  aFrame:=FrameClass.Create(aFrameType,True,aData, OutgoingFrameMask);
  try
    Send(aFrame);
  finally
    aFrame.Free;
  end;
end;

procedure TWSConnection.SetHandShakeRequest(aRequest: TWSHandShakeRequest);
begin
  FreeAndNil(FHandshakeRequest);
  FHandShakeRequest:=aRequest;
end;

constructor TWSConnection.Create(aOwner : TComponent; aOptions: TWSOptions);
begin
  InitCriticalSection(FWriteLock);
  FOwner:=aOwner;
  Foptions:=aOptions;
  FWebSocketVersion:=DefaultWebSocketVersion;
  FInitialOpcode:=ftContinuation;
  FCloseState:=csNone;
  FMaxFramePayloadSize:=DefaultMaxFramePayloadSize;
  FMaxMessagePayloadSize:=DefaultMaxMessagePayloadSize;
  AllocateConnectionID;
end;

destructor TWSConnection.Destroy;
begin
  try
    FreeAndNil(FHandshakeRequest);
  finally
    try
      If FreeUserData then
        FreeAndNil(FUserData);
    finally
      DoneCriticalSection(FWriteLock);
      inherited Destroy;
    end;
  end;
end;

class function TWSConnection.GetCloseData(aBytes: TBytes; out aReason: String): Word;
begin
  Result:=0;
  aReason:='';
  if Length(aBytes)>1 then
    Result:=NToHs(aBytes.ToWord(0));
  if Length(aBytes)>2 then
    aReason:=TEncoding.UTF8.GetAnsiString(aBytes,2,Length(aBytes)-2);
end;

function TWSConnection.GetPeerIP: String;

Var
  S : IWSTransport;

begin
  S:=Transport;
  if Assigned(S) then
    Result:=S.PeerIP
  else
    Result:=''
end;

function TWSConnection.GetPeerPort: word;
Var
  S : IWSTransport;
begin
  S:=Transport;
  if Assigned(S) then
    Result:=S.PeerPort
  else
    Result:=0
end;

procedure TWSConnection.AllocateConnectionID;
begin
  if Assigned(IDAllocator) then
    IDAllocator(FConnectionID);
  if FConnectionID='' then
{$IFDEF CPU64}
    FConnectionID:=IntToStr(InterlockedIncrement64(_ConnectionCount));
{$ELSE}
    FConnectionID:=IntToStr(InterlockedIncrement(_ConnectionCount));
{$ENDIF}
end;

procedure TWSConnection.SetCloseState(aValue: TCloseState);
var
  MustDisconnect: Boolean;
begin
  EnterCriticalSection(FWriteLock);
  try
    FCloseState:=aValue;
    MustDisconnect:=(FCloseState=csClosed) and AutoDisconnect;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
  if MustDisconnect then
    Disconnect;
end;

function TWSConnection.GetCloseState: TCloseState;
begin
  EnterCriticalSection(FWriteLock);
  try
    Result:=FCloseState;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

function TWSConnection.MessageSizeAllowed(aCurrentSize,
  aAdditionalSize: SizeInt): Boolean;
var
  TotalSize: QWord;
begin
  if (aCurrentSize<0) or (aAdditionalSize<0) then
    Exit(False);
  TotalSize:=QWord(aCurrentSize)+QWord(aAdditionalSize);
  Result:=(TotalSize<=QWord(High(SizeInt))) and
    ((FMaxMessagePayloadSize=0) or
     (TotalSize<=FMaxMessagePayloadSize));
end;

procedure TWSConnection.PrepareProtocolClose(out aSendClose: Boolean);
begin
  EnterCriticalSection(FWriteLock);
  try
    case FCloseState of
      csNone:
        begin
        FCloseState:=csReceived;
        aSendClose:=True;
        end;
      csReceived:
        aSendClose:=False;
      csSent:
        begin
        FCloseState:=csClosed;
        aSendClose:=False;
        end;
      csClosed:
        aSendClose:=False;
    end;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

function TWSConnection.WriteProtocolClose(aReason: Word): Boolean;
var
  CloseData: TBytes;
  ErrorMessage: String;
  SendClose: Boolean;
begin
  PrepareProtocolClose(SendClose);
  if not SendClose then
    Exit(False);
  SetLength(CloseData,2);
  CloseData[0]:=(aReason and $FF00) shr 8;
  CloseData[1]:=aReason and $FF;
  { Protocol-generated close frames never dispatch a nested application
    callback.  The pump reports any terminal read/write failure once, after
    the connection has been retired. }
  Result:=WriteControlFrame(ftClose,CloseData,ErrorMessage);
end;

function TWSConnection.ReadMessage: Boolean;
begin
  Result:=DoReadMessage;
end;

procedure TWSConnection.DispatchEvent(aInitialType: TFrameType; aFrame: TWSFrame; aMessageContent: TBytes);

Var
  msg: TWSMessage;
  ControlHandler: TWSControlEvent;
  MessageHandler: TWSMessageEvent;

begin
  Case aInitialType of
  ftPing,
  ftPong,
  ftClose :
    begin
      ControlHandler:=FOnControl;
      If Assigned(ControlHandler) then
        ControlHandler(Self,aInitialType,aMessageContent);
    end;
  ftBinary,
  ftText :
    begin
      MessageHandler:=FOnMessageReceived;
      if Assigned(MessageHandler) then
      begin
        Msg:=Default(TWSMessage);
        Msg.IsText:=(aInitialType=ftText);
        if aFrame.FrameType in [ftBinary,ftText] then
          Msg.Sequences:=[fsFirst]
        else
          Msg.Sequences:=[fsContinuation];
        if aFrame.FinalFrame then
          Msg.Sequences:=Msg.Sequences+[fsLast];
        Msg.PayLoad:=aMessageContent;
        MessageHandler(Self, Msg);
      end;
    end;
  ftContinuation: ; // Cannot happen normally
  end;
end;

function TWSConnection.HandleIncoming(aFrame: TWSFrame) : Boolean;

  procedure ProtocolError(aCode: Word);
  begin
    { Commit the terminal state before sending. }
    Result:=False;
    WriteProtocolClose(aCode);
  end;

  function ReceiveClose: TCloseState;
  begin
    EnterCriticalSection(FWriteLock);
    try
      Result:=FCloseState;
      case FCloseState of
        csNone:
          FCloseState:=csReceived;
        csSent:
          FCloseState:=csClosed;
        csReceived,
        csClosed:
          ;
      end;
    finally
      LeaveCriticalSection(FWriteLock);
    end;
  end;

var
  CloseData,
  ErrorData: TBytes;
  WriteErrorMessage: String;
  InitialType: TFrameType;
  MessageContent: TBytes;
  PreviousCloseState: TCloseState;

begin
  Result:=True;
  // check Reserved bits
  if aFrame.Reserved<>0 then
    begin
    ProtocolError(CLOSE_PROTOCOL_ERROR);
    Exit;
    end;
  // check Reserved opcode
  if aFrame.FrameType=ftFutureOpcodes then
    begin
    ProtocolError(CLOSE_PROTOCOL_ERROR);
    Exit;
    end;
  { A control frame must be final and have at most 125 payload bytes. }
  if (aFrame.FrameType in [ftPing,ftPong,ftClose]) and
     ((not aFrame.FinalFrame) or (aFrame.Payload.DataLength>125)) then
    begin
    ProtocolError(CLOSE_PROTOCOL_ERROR);
    Exit;
    end;

  Case aFrame.FrameType of
    ftContinuation:
      begin
      if FInitialOpcode=ftContinuation then
        begin
        ProtocolError(CLOSE_PROTOCOL_ERROR);
        Exit;
        end;
      if not MessageSizeAllowed(Length(FMessageContent),
        Length(aFrame.Payload.Data)) then
        begin
        ProtocolError(CLOSE_MESSAGE_TOO_BIG);
        Exit;
        end;

      FMessageContent.Append(aFrame.Payload.Data);
      if aFrame.FinalFrame then
        begin
        InitialType:=FInitialOpcode;
        MessageContent:=FMessageContent;
        { All parser state is reset before application code is entered. }
        FInitialOpcode:=ftContinuation;
        FMessageContent:=[];
        if (InitialType=ftText) and
           (not IsValidUTF8(MessageContent)) then
          begin
          ProtocolError(CLOSE_INVALID_FRAME_PAYLOAD_DATA);
          Exit;
          end;
        DispatchEvent(InitialType,aFrame,MessageContent);
        end;
      end;

    ftPing:
      begin
      if not (woPongExplicit in Options) then
        if not WriteControlFrame(ftPong,aFrame.Payload.Data,
          WriteErrorMessage) then
          begin
          Result:=False;
          ErrorData:=TEncoding.UTF8.GetBytes(
            UnicodeString(WriteErrorMessage));
          DispatchEvent(ftClose,nil,ErrorData);
          Exit;
          end;
      { The callback is deliberately last: it may disconnect or free Self. }
      DispatchEvent(ftPing,aFrame,aFrame.Payload.Data);
      end;

    ftPong:
      { The callback is deliberately last: it may disconnect or free Self. }
      DispatchEvent(ftPong,aFrame,aFrame.Payload.Data);

    ftClose:
      begin
      if (aFrame.Payload.DataLength>123) or
         ((aFrame.Reason<>CLOSE_NO_STATUS_RCVD) and
          (not WSValidCloseCode(aFrame.Reason))) then
        begin
        ProtocolError(CLOSE_PROTOCOL_ERROR);
        Exit;
        end;
      if not IsValidUTF8(aFrame.Payload.Data) then
        begin
        ProtocolError(CLOSE_INVALID_FRAME_PAYLOAD_DATA);
        Exit;
        end;

      PreviousCloseState:=ReceiveClose;
      if PreviousCloseState in [csSent,csClosed] then
        Result:=False
      else if woCloseExplicit in Options then
        Result:=True
      else
        begin
        Result:=False;
        { Commit/send the close reply before entering application code. }
        if aFrame.Reason=CLOSE_NO_STATUS_RCVD then
          CloseData:=Nil
        else
          begin
          SetLength(CloseData,2);
          CloseData[0]:=(aFrame.Reason and $FF00) shr 8;
          CloseData[1]:=aFrame.Reason and $FF;
          end;
        if not WriteControlFrame(ftClose,CloseData,
          WriteErrorMessage) then
          begin
          ErrorData:=TEncoding.UTF8.GetBytes(
            UnicodeString(WriteErrorMessage));
          DispatchEvent(ftClose,nil,ErrorData);
          Exit;
          end;
        end;
      { Explicit-close users now receive the close event needed to respond. }
      DispatchEvent(ftClose,aFrame,aFrame.Payload.Data);
      end;

    ftBinary,ftText:
      begin
      if FInitialOpcode in [ftText,ftBinary] then
        begin
        ProtocolError(CLOSE_PROTOCOL_ERROR);
        Exit;
        end;
      if not MessageSizeAllowed(0,Length(aFrame.Payload.Data)) then
        begin
        ProtocolError(CLOSE_MESSAGE_TOO_BIG);
        Exit;
        end;

      FInitialOpcode:=aFrame.FrameType;
      FMessageContent:=aFrame.Payload.Data;
      if aFrame.FinalFrame then
        begin
        InitialType:=FInitialOpcode;
        MessageContent:=FMessageContent;
        { All parser state is reset before application code is entered. }
        FInitialOpcode:=ftContinuation;
        FMessageContent:=[];
        if (InitialType=ftText) and
           (not IsValidUTF8(MessageContent)) then
          begin
          ProtocolError(CLOSE_INVALID_FRAME_PAYLOAD_DATA);
          Exit;
          end;
        DispatchEvent(InitialType,aFrame,MessageContent);
        end;
      end;
  else
    ; // avoid Compiler warning
  End;
end;

function TWSConnection.IsValidUTF8(aValue: TBytes): boolean;
var
  i, len: SizeInt;
  n, j: Integer;
  c: ^byte;
begin
  Result := true;
  len := length(aValue);
  if len = 0 then
    exit;
  Result := False;
  i := 0;
  c := @AValue[0];
  while i < len do
  begin
    if (c^ <= $7f) then
      n := 0
    else if (c^ >= $c2) and (c^ <= $df) then
      n := 1
    else if (c^ = $e0) then
      n := 2
    else if (c^ >= $e1) and (c^ <= $ec) then
      n := 2
    else if (c^ = $ed) then
      n := 2
    else if (c^ >= $ee) and (c^ <= $ef) then
      n := 2
    else if (c^ = $f0) then
      n := 3
    else if (c^ >= $f1) and (c^ <= $f3) then
      n := 3
    else if (c^ = $f4) then
      n := 3
    else
      exit;

    j := 0;
    Inc(i);

    while j < n do
    begin
      if i >= len then
        exit;
      case c^ of
        $c2..$df, $e1..$ec, $ee..$ef, $f1..$f3:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $bf)) then
            exit;
        $e0:
          if not (((c + 1)^ >= $a0) and ((c + 1)^ <= $bf)) then
            exit;
        $ed:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $9f)) then
            exit;
        $f0:
          if not (((c + 1)^ >= $90) and ((c + 1)^ <= $bf)) then
            exit;
        $f4:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $8f)) then
            exit;
        $80..$bf:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $bf)) then
            exit;
      end;
      Inc(c);
      Inc(i);
      Inc(j);
    end;
    Inc(c);
  end;
  Result := True;
end;

function TWSConnection.FrameClass: TWSFrameClass;

begin
  Result:=TWSFrame;
end;

procedure TWSConnection.Send(const AMessage: UTF8string);

var
  aFrame: TWSFrame;

begin
  aFrame:=FrameClass.Create(aMessage, OutgoingFrameMask);
  try
    Send(aFrame);
  finally
    aFrame.Free;
  end;
end;

procedure TWSConnection.Send(const ABytes: TBytes);
var
  aFrame: TWSFrame;
begin
  aFrame:=FrameClass.Create(ftBinary,True,ABytes, OutgoingFrameMask);
  try
    Send(aFrame);
  finally
    aFrame.Free;
  end;
end;

procedure TWSConnection.Close(aMessage: UTF8String);
begin
  Close(aMessage, CLOSE_NORMAL_CLOSURE);
end;

procedure TWSConnection.Close(aMessage: UTF8String; aReason: word);
var
  aData: TBytes;
  aSize: SizeInt;
begin
  aData := [];
  // first two bytes is reason of close RFC 6455 section-5.5.1
  aData := TEncoding.UTF8.GetAnsiBytes(aMessage);
  aSize := Length(aData);
  if aSize>123 then
    raise EWSProtocolError.Create(SErrControlPayloadTooLarge,
      CLOSE_PROTOCOL_ERROR);
  SetLength(aData, aSize + 2);
  if aSize > 0 then
    move(aData[0], aData[2], aSize);
  aData[0] := (aReason and $FF00) shr 8;
  aData[1] := aReason and $FF;
  Close(aData);
end;

procedure TWSConnection.Disconnect;
begin
  DoDisconnect;
end;

procedure TWSConnection.Close(aData: TBytes);
var
  CloseReason: Word;
  ReasonData: TBytes;
begin
  if (Length(aData)=1) or (Length(aData)>125) then
    raise EWSProtocolError.Create(SErrInvalidPayloadLength,
      CLOSE_PROTOCOL_ERROR);
  if Length(aData)>=2 then
    begin
    CloseReason:=NToHs(aData.ToWord(0));
    if not WSValidCloseCode(CloseReason) then
      raise EWSProtocolError.Create(SErrInvalidPayloadLength,
        CLOSE_PROTOCOL_ERROR);
    if Length(aData)>2 then
      begin
      ReasonData:=Copy(aData,2,Length(aData)-2);
      if not IsValidUTF8(ReasonData) then
        raise EWSProtocolError.Create(SErrInvalidPayloadLength,
          CLOSE_INVALID_FRAME_PAYLOAD_DATA);
      end;
    end;
  Send(ftClose,aData);
end;

function TWSConnection.WriteFrame(aFrame: TWSFrame;
  out aErrorMessage: String): Boolean;
var
  Data: TBytes;
  CurrentTransport: IWSTransport;
begin
  Data:=aFrame.AsBytes;
  CurrentTransport:=Transport;
  aErrorMessage:='';
  Result:=True;
  EnterCriticalSection(FWriteLock);
  try
    case FCloseState of
      csNone:
        ;
      csReceived:
        if aFrame.FrameType<>ftClose then
          Raise EWebSocket.Create(SErrConnectionClosing);
      csSent,
      csClosed:
        Raise EWebSocket.Create(SErrCloseAlreadySent);
    end;
    try
      if not Assigned(CurrentTransport) then
        raise EWSWriteError.Create(SErrWriteClosed,0);
      { IWSTransport.WriteBuffer is the serialized write-all boundary.
        TWSSocketHelper keeps its transport lock across every short write. }
      CurrentTransport.WriteBuffer(Data);
    except
      on E: EWSTransportError do
        begin
        { Publish terminal state before the caller may dispatch an error. }
        FCloseState:=csClosed;
        if woSendErrClosesConn in Options then
          begin
          aErrorMessage:=E.Message;
          Result:=False;
          end
        else
          raise;
        end;
    end;
    if Result and (aFrame.FrameType=ftClose) then
      begin
      if FCloseState=csNone then
        FCloseState:=csSent
      else if FCloseState=csReceived then
        FCloseState:=csClosed;
      end;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
end;

function TWSConnection.WriteControlFrame(aFrameType: TFrameType;
  const aData: TBytes; out aErrorMessage: String): Boolean;
var
  Frame: TWSFrame;
begin
  Frame:=FrameClass.Create(aFrameType,True,aData,OutgoingFrameMask);
  try
    Result:=WriteFrame(Frame,aErrorMessage);
  finally
    Frame.Free;
  end;
end;

procedure TWSConnection.Send(aFrame: TWSFrame);
var
  ErrorData: TBytes;
  ErrorMessage: String;
begin
  if not WriteFrame(aFrame,ErrorMessage) then
    begin
    ErrorData:=TEncoding.UTF8.GetBytes(UnicodeString(ErrorMessage));
    { This callback is deliberately the final operation on Self. }
    DispatchEvent(ftClose,nil,ErrorData);
    end;
end;

function TWSConnection.DoReadMessage: Boolean;

Var
  F : TWSFrame;

begin
  Result:=False;
  If not Transport.CanRead(0) then
    Exit;
  try
    F:=Nil;
    try
      F:=FrameClass.CreateFromStream(Transport,FMaxFramePayloadSize);
      if Assigned(F) then
        begin
        if ((Self is TWSServerConnection) and (not F.Payload.Masked)) or
           ((Self is TWSClientConnection) and F.Payload.Masked) then
          raise EWSProtocolError.Create(SErrInvalidFrameMask,
            CLOSE_PROTOCOL_ERROR);
        Result:=HandleIncoming(F);
        end;
    finally
      F.Free;
    end;
  except
    { Preserve the long-standing CheckIncoming contract: an orderly EOF,
      including EOF in an incomplete frame, is a terminal irClose result. }
    on E: EWSConnectionClosed do
      Result:=False;
    on E: EWSProtocolError do
      begin
      Result:=False;
      WriteProtocolClose(E.CloseCode);
      end;
  end;
end;

function TWSConnection.CheckIncoming(aTimeout: Integer; DoRead: Boolean = True): TIncomingResult;
begin
  if not Transport.CanRead(aTimeOut) then
    Result:=irNone
  else if Not DoRead then
    Result:=irWaiting
  else if ReadMessage then
    Result:=irOK
  else
    Result:=irClose;
end;

constructor TWSClientConnection.Create(aOwner: TComponent; aTransport: TWSClientTransport; aOptions : TWSOptions);
begin
  Inherited Create(aOwner,aOptions);
  FTransport:=aTransport;
end;

destructor TWSClientConnection.Destroy;
begin
  FreeAndNil(FTransport);
  inherited Destroy;
end;

function TWSClientConnection.GetHandshakeCompleted: Boolean;
begin
  Result:=Assigned(FHandshakeResponse);
end;


function TWSClientConnection.GetTransport: IWSTransport;

begin
  Result:=FTransport;
end;

{ TWSHandShakeRequest }

Class Function TWSHandShakeRequest.GenerateKey : String;

Var
  I : Integer;
  B : TBytes;

begin
  B:=[];
  SetLength(B,16);
  For I:=0 to 15 do
    B[i]:=Random(256);
  Result:=EncodeBytesBase64(B);
end;


constructor TWSHandShakeRequest.Create(const aResource: string; const aExtraHeaders: TStrings);
begin
  Inherited Create(aResource,aExtraHeaders);
  Version:=IntToStr(DefaultWebSocketVersion);
end;

procedure TWSHandShakeRequest.ToStrings(aHeaders: TStrings);

  procedure Add(const AName, aValue, aDefault: String);

  Var
    V : String;
  begin
    V:=aValue;
    if V='' then
      V:=aDefault;
    if V<>'' then
      aHeaders.Add(aName+': '+V)
  end;


Var
  N,V : String;
  I  : Integer;

begin
  aHeaders.Clear;
  if Resource='' then
    Resource:='/';
  aHeaders.Add('GET ' + Resource + ' HTTP/1.1');
  V:=Host;
  if (V<>'') and (Port<>443) and (Port<>80) then
    V:=V+':'+IntToStr(Port);
  Add('Host',V,'');
  Add('Upgrade',Upgrade,'websocket');
  Add('Connection',Connection,'Upgrade');
  Add('Origin',Origin,Host);
  if Key='' then
    Key:=GenerateKey;
  Add('Sec-WebSocket-Key',Key,'');
  Add('Sec-WebSocket-Protocol',Protocol,'');
  Add('Sec-WebSocket-Version',Version,'');
  For I:=0 to RawHeaders.Count-1 do
    begin
    RawHeaders.GetNameValue(I,N,V);
    if (N<>'') and (V<>'') then
      if (aHeaders.Values[N]='') then
        Add(N,V,'')
    end;
end;

{ TWSServerConnection }

constructor TWSServerConnection.Create(aOwner : TComponent; aTransport : TWSServerTransport; aOptions : TWSOptions);
begin
  Inherited Create(aOwner,aOptions);
  FHandshakeResponseSent := False;
  FTransport:=aTransport;
  FExtraHeaders:=TStringList.Create;
  FExtraHeaders.NameValueSeparator:=':';
end;

destructor TWSServerConnection.Destroy;
begin
  if Assigned(FTransport) then
    DisConnect;
  FreeAndNil(FExtraHeaders);
  inherited;
end;

procedure TWSServerConnection.PerformHandshake;

Var
  Headers : TStrings;
  aResource,Status,aLine : String;
  HSR : TWSHandShakeRequest;
  HeaderBytes,
  LineBytes : SizeInt;
  HeaderLines : Integer;

begin
  Status:=Transport.ReadLn;
  if Length(Status)>WSMaxServerHandshakeLineBytes then
    Raise EWSHandShake.Create(SErrHandshakeLineTooLong);
  HeaderBytes:=Length(Status)+2;
  HeaderLines:=1;
  aResource:=ExtractWord(2,Status,[' ']);
  HSR:=Nil;
  Headers:=TStringList.Create;
  try
    Headers.NameValueSeparator:=':';
    repeat
      begin
      aLine:=Transport.ReadLn;
      LineBytes:=Length(aLine);
      if LineBytes>WSMaxServerHandshakeLineBytes then
        Raise EWSHandShake.Create(SErrHandshakeLineTooLong);
      Inc(HeaderLines);
      if (HeaderLines>WSMaxServerHandshakeLines) or
         (HeaderBytes+LineBytes+2>WSMaxServerHandshakeBytes) then
        Raise EWSHandShake.Create(SErrHandshakeHeadersTooLarge);
      Inc(HeaderBytes,LineBytes+2);
      if aLine<>'' then
        Headers.Add(aLine);
      end;
    until aLine='';
    HSR:=TWSHandShakeRequest.Create(aResource,Headers);
    FHandshakeResponseSent:=DoHandshake(HSR);
  finally
    HSR.Free;
    Headers.Free;
  end;
end;


function TWSServerConnection.GetHandshakeCompleted: Boolean;
begin
  Result:=HandshakeResponseSent;
end;

procedure TWSServerConnection.DoDisconnect;
begin
  if Assigned(FTransport) then
    FTransport.CloseSocket;
  FreeAndNil(FTransPort);
end;

function TWSServerConnection.GetTransport: IWSTransport;
begin
  Result:=FTransport;
end;

function TWSServerConnection.DoPrepareHandshakeResponse(
  aRequest: TWSHandShakeRequest; aResponse: TWSHandShakeResponse): boolean;
begin
  If Assigned(OnHandshake) then
    Result:=OnHandShake(aRequest,aResponse)
  else
    Result:=true;
end;


function TWSServerConnection.DoHandshake(const aRequest : TWSHandShakeRequest) : Boolean;

var
  aLine,Reply : string;
  aResp : TWSHandShakeResponse;
  H : TStrings;
  B : TBytes;


begin
  Result:=False;
  H:=Nil;
  aResp:=TWSHandShakeResponse.Create('',FExtraHeaders);
  try
    try
      DoPrepareHandshakeResponse(aRequest,aResp);
      H:=TStringList.Create;
      aResp.ToStrings(aRequest,H,True);
      Reply:='';
      For aLine in H do
        Reply:=Reply+aLine+#13#10;
      Reply:=Reply+#13#10;
      B:=TEncoding.UTF8.GetAnsiBytes(Reply);
      Transport.WriteBuffer(B);
      Result:=True;
      FHandshakeResponseSent:=True;
    except
      on E: Exception do
      begin
        // Close the connection if the handshake failed
        Disconnect;
      end;
    end;
  finally
    H.Free;
    aResp.Free;
  end;
end;


end.
