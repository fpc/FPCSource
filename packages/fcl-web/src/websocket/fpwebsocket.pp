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

unit fpwebsocket;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, sockets, ssockets;

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

type
  EWebSocket = Class(Exception);
  EWSHandShake = class(EWebSocket);

  TFrameType = (ftContinuation,ftText,ftBinary,ftClose,ftPing,ftPong,ftFutureOpcodes);

  TFrameTypes = Set of TFrameType;

  TFrameSequence = (fsFirst,fsContinuation,fsLast);
  TFrameSequences = Set of TFrameSequence;

  TIncomingResult = (irNone,    // No data waiting
                     irWaiting, // Data waiting
                     irOK,      // Data was waiting and handled
                     irClose    // Data was waiting, handled, and we must disconnect (CloseState=csClosed)
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
  Public
    Constructor Create (aSocket : TSocketStream);
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
    Procedure Read(buffer: TBytes; aTransport : IWSTransport);
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
    function Read(aTransport: IWSTransport): boolean;
    function GetAsBytes : TBytes; virtual;
  Public
    // Read a message from transport. Returns Nil if the connection was closed when reading.
    class function CreateFromStream(aTransport : IWSTransport): TWSFrame;
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
               woSkipUpgradeCheck,  // Skip handshake "Upgrade:" HTTP header cheack.
               woSkipVersionCheck   // Skip handshake "Sec-WebSocket-Version' HTTP header check.
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
    Function GetPeerIP : String;
    Function GetPeerPort : word;
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
    Procedure Disconnect;
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
    Property CloseState : TCloseState Read FCloseState;
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
    //
    Property HandShakeResponse : TWSHandShakeResponse Read FHandshakeResponse Write FHandshakeResponse;
  End;

  { TWSServerTransport }

  TWSServerTransport = class(TWSTransport)
  end;

  { TWSServerConnection }
  TWSConnectionHandshakeEvent =  procedure (aRequest : TWSHandShakeRequest; aResponse : TWSHandShakeResponse) of object;

  TWSServerConnection = Class(TWSConnection)
  Private
    FExtraHeaders: TStrings;
    FHandshakeResponseSent: Boolean;
    FOnHandShake: TWSConnectionHandshakeEvent;
    FTransport : TWSServerTransport;
  Protected
    Procedure DoDisconnect; override;
    function GetTransport: IWSTransport; override;
    procedure DoPrepareHandshakeResponse(aRequest : TWSHandShakeRequest; aResponse : TWSHandShakeResponse); virtual;
    function GetHandshakeCompleted: Boolean; override;
  public
    // Transport is owned by connection
    constructor Create(aOwner : TComponent; aTransport : TWSServerTransport; aOptions : TWSOptions); overload;
    // disconnect
    destructor Destroy; override;
    // Do full circle.
    Procedure PerformHandshake; virtual;
    // Given a request, send response
    function DoHandshake(const aRequest : TWSHandShakeRequest): Boolean;
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

function DecodeBytesBase64(const s: string; Strict: boolean = false) : TBytes;
function EncodeBytesBase64(const aBytes : TBytes) : String;


implementation

uses strutils, sha1,base64;

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
    hash : TSHA1Digest;
    K : string;
  begin
    // respond key
    b:=[];
    k:= Trim(aHandshake.Key) + SSecWebSocketGUID;
    hash:=sha1.SHA1String(k);
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
  Result:=FHelper.Socket
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
  sockets.CloseSocket(FStream.Handle);
end;

{ TWSTransport }

constructor TWSSocketHelper.Create(aSocket: TSocketStream);
begin
  FSocket:=aSocket;
{$if defined(FreeBSD) or defined(Linux)}
  FSocket.ReadFlags:=MSG_NOSIGNAL;
  FSocket.WriteFlags:=MSG_NOSIGNAL;
{$endif}
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
  aSize : integer;

begin
  // Preset
  Result:='';
  SetLength(Result,255);
  aSize:=0;
  C:=0;
  While (FSocket.Read(C,1)=1) and (C<>10) do
    begin
    Inc(aSize);
    if aSize>Length(Result) then
      SetLength(Result,Length(Result)+255);
    Result[aSize]:=AnsiChar(C);
    end;
  if (aSize>0) and (Result[aSize]=#13) then
    Dec(aSize);
  SetLength(Result,aSize);
end;

function TWSSocketHelper.ReadBytes(var aBytes: TBytes; aCount: Integer): Integer;
var
  buf: TBytes;
  aPos, toRead: QWord;
begin
  if aCount=0 then exit(0);
  aPos := 0;
  SetLength(aBytes, aCount);
  repeat
    SetLength(buf, aCount);
    Result := FSocket.Read(buf[0], aCount - aPos);
    if Result <= 0 then
      break;
    SetLength(buf, Result);
    Move(buf[0], aBytes[aPos], Result);
    Inc(aPos, Result);
    ToRead := aCount - aPos;
    Result := aCount;
  until toRead <= 0;
end;

procedure TWSSocketHelper.ReadBuffer(aBytes: TBytes);
begin
  if Length(ABytes)=0 then exit;
  FSocket.ReadBuffer(aBytes[0],Length(ABytes));
end;

function TWSSocketHelper.WriteBytes(aBytes: TBytes; aCount: Integer): Integer;
begin
  if aCount=0 then exit(0);
  Result:=FSocket.Write(aBytes[0],aCount);
end;

procedure TWSSocketHelper.WriteBuffer(aBytes: TBytes);
begin
  if Length(aBytes)=0 then exit;
  FSocket.WriteBuffer(aBytes[0],Length(aBytes));
end;

{ TWSMessage }

function TWSMessage.GetAsString: UTF8String;

begin
  Result:=TEncoding.UTF8.GetString(Payload);
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
  aPos,toRead : QWord;
  aCount : Longint;

begin
  Buf:=[];
  ToRead:=DataLength;
  aPos:=0;
  Repeat
    aCount:=ToRead;
    if aCount>MaxBufSize then
      aCount:=MaxBufSize;
    SetLength(Buf,aCount);
    aCount := aTransport.ReadBytes(Buf,aCount);
    Move(Buf[0],Content[aPos],aCount);
    Inc(aPos,aCount);
    ToRead:=DataLength-aPos;
  Until (ToRead<=0);
end;

procedure TWSFramePayload.Read(buffer: TBytes; aTransport: IWSTransport);

Var
  LenFlag : Byte;
  paylen16 : Word;
  content: TBytes;

begin
  content:=[];
  Masked := ((buffer[1] and FlagMasked) <> 0);
  LenFlag := buffer[1] and FlagLengthMask;

  Case LenFlag of
   FlagTwoBytes:
    begin
    aTransport.ReadBytes(Buffer,2);
    Paylen16:=Buffer.ToWord(0);
    DataLength := ntohs(PayLen16);
    end;
  FlagEightBytes:
    begin
    aTransport.ReadBytes(Buffer,8);
    DataLength:=Buffer.ToQWord(0);
    DataLength := ntohx(DataLength); // MVC : Needs fixing
    end
  else
    DataLength:=lenFlag;
  end;

  if Masked then
  begin
    // In some times, not 4 bytes are returned
    aTransport.ReadBytes(Buffer,4);
    MaskKey:=buffer.ToDword(0);
  end;
  SetLength(content, DataLength);
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

var
  closeData: TBytes;

begin
  Create(aType,aIsFinal,aMask);

  FPayload.Data := APayload;
  if Assigned(aPayload) then
    FPayload.DataLength := Cardinal(Length(aPayload));
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
  Data:=TEncoding.UTF8.GetBytes(AMessage);
  Create(ftText,True,Data,aMask);
end;

class function TWSFrame.CreateFromStream(aTransport : IWSTransport): TWSFrame;

begin
  Result:=TWSFrame.Create;
  try
    if not Result.Read(aTransport) then
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


function TWSFrame.Read(aTransport: IWSTransport): boolean;

Var
  Buffer : Tbytes;
  B1 : Byte;

begin
  Result:=False;
  Buffer:=Default(TBytes);
  SetLength(Buffer,2);
  if aTransport.ReadBytes(Buffer,2)=0 then
    Exit;
  if Length(Buffer)<2 then
    Raise EWebSocket.Create('Could not read frame header');
  B1:=buffer[0];
  FFinalFrame:=(B1 and FlagFinalFrame) = FlagFinalFrame;
  FRSV:=(B1 and %01110000) shr 4;
  FFrameType.AsFlag:=(B1 and $F);
  FPayload.Read(Buffer,aTransport);
  FReason:=CLOSE_NORMAL_CLOSURE;
  if FFrameType=ftClose then
    if FPayload.DataLength = 1 then
      FReason:=CLOSE_PROTOCOL_ERROR
    else
    if FPayload.DataLength>1 then
    begin
      FReason:=SwapEndian(FPayload.Data.ToWord(0));
      FPayload.DataLength := FPayload.DataLength - 2;
      if FPayload.DataLength > 0 then
        move(FPayload.Data[2], FPayload.Data[0], FPayload.DataLength - 2);
      SetLength(FPayload.Data, FPayload.DataLength);
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
  SetLength(buffer,aOffset+Int64(FPayload.DataLength));
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
      move(Payload.Data[0], buffer[aOffset], Payload.DataLength);
  Result := Buffer;
end;

class procedure TWSFramePayload.DoMask(var aData: TBytes; Key: DWORD);


begin
  CopyMasked(aData,aData,Key,0)
end;

class procedure TWSFramePayload.CopyMasked(SrcData: TBytes; var DestData: TBytes; Key: DWORD; aOffset: Integer);

var
  currentMaskIndex: Longint;
  byteKeys: TBytes;
  I: Longint;

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
  aFrame:=FrameClass.Create(aFrameType,True,aData);
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
  FOwner:=aOwner;
  Foptions:=aOptions;
  FWebSocketVersion:=WebSocketVersion;
  AllocateConnectionID;
end;

destructor TWSConnection.Destroy;
begin
  FreeAndNil(FHandshakeRequest);
  If FreeUserData then
    FreeAndNil(FUserData);
  inherited;
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
begin
  FCloseState:=aValue;
  if (FCloseState=csClosed) and AutoDisconnect then
    Disconnect;
end;

function TWSConnection.ReadMessage: Boolean;
begin
  Result:=DoReadMessage;
end;

procedure TWSConnection.DispatchEvent(aInitialType: TFrameType; aFrame: TWSFrame; aMessageContent: TBytes);

Var
  msg: TWSMessage;

begin
  Case aInitialType of
  ftPing,
  ftPong,
  ftClose :
    If Assigned(FOnControl) then
      FOnControl(Self,aInitialType,aMessageContent);
  ftBinary,
  ftText :
    begin
      if Assigned(FOnMessageReceived) then
      begin
        Msg:=Default(TWSMessage);
        Msg.IsText:=(aInitialType=ftText);
        if aFrame.FrameType=ftBinary then
          Msg.Sequences:=[fsFirst]
        else
          Msg.Sequences:=[fsContinuation];
        if aFrame.FinalFrame then
          Msg.Sequences:=Msg.Sequences+[fsLast];
        Msg.PayLoad:=aMessageContent;
        FOnMessageReceived(Self, Msg);
      end;
    end;
  ftContinuation: ; // Cannot happen normally
  end;
end;

function TWSConnection.HandleIncoming(aFrame: TWSFrame) : Boolean;

   Procedure UpdateCloseState;

   begin
     if (FCloseState=csNone) then
       FCloseState:=csReceived
     else if (FCloseState=csSent) then
       FCloseState:=csClosed;
   end;

   procedure ProtocolError(aCode: Word);
   begin
     Close('', aCode);
     UpdateCloseState;
     Result:=false;
   end;

begin
  Result:=true;
  // check Reserved bits
  if aFrame.Reserved<>0 then
  begin
    ProtocolError(CLOSE_PROTOCOL_ERROR);
    Exit;
  end;
  // check Reserved opcode
  if aFrame.FrameType = ftFutureOpcodes then
  begin
    ProtocolError(CLOSE_PROTOCOL_ERROR);
    Exit;
  end;
  { If control frame it must be complete }
  if ((aFrame.FrameType=ftPing) or
      (aFrame.FrameType=ftPong) or
      (aFrame.FrameType=ftClose))
     and (not aFrame.FinalFrame) then
  begin
    ProtocolError(CLOSE_PROTOCOL_ERROR);
    Exit;
  end;
  //

  // here we handle payload.
//  if aFrame.FrameType in [ftBinary,ftText] then
//  begin
//    FInitialOpcode:=aFrame.FrameType;
//    FMessageContent:=aFrame.Payload.Data;
//  end;

  // Special handling
  Case aFrame.FrameType of
    ftContinuation:
      begin
        if FInitialOpcode=ftContinuation then
        begin
          ProtocolError(CLOSE_PROTOCOL_ERROR);
          Exit;
        end;

        FMessageContent.Append(aFrame.Payload.Data);
        if aFrame.FinalFrame then
        begin
          if FInitialOpcode = ftText then
            if IsValidUTF8(FMessageContent) then
              DispatchEvent(FInitialOpcode,aFrame,FMessageContent)
            else
              ProtocolError(CLOSE_INVALID_FRAME_PAYLOAD_DATA)
          else
            DispatchEvent(FInitialOpcode,aFrame,FMessageContent);
          // reset initial opcode
          FInitialOpcode:=ftContinuation;
        end;
      end;

    ftPing:
      begin
        if aFrame.Payload.DataLength > 125 then
          ProtocolError(CLOSE_PROTOCOL_ERROR)
        else
        if not (woPongExplicit in Options) then
        begin
          Send(ftPong,aFrame.Payload.Data);
          DispatchEvent(ftPing,aFrame,aFrame.Payload.Data);
        end;
     end;
   ftClose:
     begin
       // If our side sent the initial close, this is the reply, and we must disconnect (Result=false).
       Result:=FCloseState=csNone;
       if Result then
       begin
         if (aFrame.Payload.DataLength>123) then
         begin
           ProtocolError(CLOSE_PROTOCOL_ERROR);
           exit;
         end;

         if not (woCloseExplicit in Options) then
         begin
          if (aFrame.Reason<CLOSE_NORMAL_CLOSURE) or
             (aFrame.Reason=CLOSE_RESERVER) or
             (aFrame.Reason=CLOSE_NO_STATUS_RCVD) or
             (aFrame.Reason=CLOSE_ABNORMAL_CLOSURE) or
             ((aFrame.Reason>CLOSE_TLS_HANDSHAKE) and (aFrame.Reason<3000)) then
           begin
             ProtocolError(CLOSE_PROTOCOL_ERROR);
             exit;
           end;
           if IsValidUTF8(aFrame.Payload.Data) then
           begin
             DispatchEvent(ftClose,aFrame,aFrame.Payload.Data);
             Close('', aFrame.Reason); // Will update state
             UpdateCloseState;
             Result:=False; // We can disconnect.
           end
           else
            ProtocolError(CLOSE_PROTOCOL_ERROR);

         end
         else
           UpdateCloseState
       end
       else
         UpdateCloseState;
     end;
   ftBinary,ftText:
     begin
       if FInitialOpcode in [ftText, ftBinary] then
       begin
         ProtocolError(CLOSE_PROTOCOL_ERROR);
         Exit;
       end;
       FInitialOpcode:=aFrame.FrameType;
       FMessageContent:=aFrame.Payload.Data;
       if aFrame.FinalFrame then
       begin
         if aFrame.FrameType = ftText then
           if IsValidUTF8(aFrame.Payload.Data) then
             DispatchEvent(FInitialOpcode,aFrame,aFrame.Payload.Data)
           else
             ProtocolError(CLOSE_INVALID_FRAME_PAYLOAD_DATA)
         else
           DispatchEvent(FInitialOpcode,aFrame,aFrame.Payload.Data);

         FInitialOpcode:=ftContinuation;
       end;
     end;
  else
    ; // avoid Compiler warning
  End;
end;

function TWSConnection.IsValidUTF8(aValue: TBytes): boolean;
var
  i, len, n, j: integer;
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
    if (c^ >= $00) and (c^ <= $7f) then
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
  aFrame:=FrameClass.Create(aMessage);
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
  aFrame:=FrameClass.Create(ftBinary,True,ABytes);
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
  aSize: Integer;
begin
  aData := [];
  // first two bytes is reason of close RFC 6455 section-5.5.1
  aData := TEncoding.UTF8.GetAnsiBytes(aMessage);
  aSize := Length(aData);
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
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TWSConnection.Close(aData: TBytes);
begin
  Send(ftClose,aData);
end;

procedure TWSConnection.Send(aFrame: TWSFrame);

Var
  Data : TBytes;

begin
  if FCloseState=csClosed then
    Raise EWebSocket.Create(SErrCloseAlreadySent);
  Data:=aFrame.AsBytes;
  Transport.WriteBytes(Data,Length(Data));
  if (aFrame.FrameType=ftClose) then
    begin
    if FCloseState=csNone then
      FCloseState:=csSent
    else if FCloseState=csReceived then
      FCloseState:=csClosed;
    end;
end;

function TWSConnection.DoReadMessage: Boolean;

Var
  F : TWSFrame;

begin
  Result:=False;
  If not Transport.CanRead(0) then
    Exit;
  f:=FrameClass.CreateFromStream(Transport);
  try
    if Assigned(F) then
      Result:=HandleIncoming(F)
  finally
    F.Free;
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
  inherited;
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
  DisConnect;
  inherited;
end;

procedure TWSServerConnection.PerformHandshake;

Var
  Headers : TStrings;
  aResource,Status,aLine : String;
  HSR : TWSHandShakeRequest;

begin
  Status:=Transport.ReadLn;
  aResource:=ExtractWord(2,Status,[' ']);
  HSR:=Nil;
  Headers:=TStringList.Create;
  try
    Headers.NameValueSeparator:=':';
    aLine:=Transport.ReadLn;
    While aLine<>'' do
      begin
      Headers.Add(aLine);
      aLine:=Transport.ReadLn;
      end;
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

procedure TWSServerConnection.DoPrepareHandshakeResponse(aRequest: TWSHandShakeRequest; aResponse: TWSHandShakeResponse);
begin
  If Assigned(OnHandshake) then
    OnHandShake(aRequest,aResponse);
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
    DoPrepareHandshakeResponse(aRequest,aResp);
    try
      H:=TStringList.Create;
      aResp.ToStrings(aRequest,H,True);
      Reply:='';
      For aLine in H do
        Reply:=Reply+aLine+#13#10;
      Reply:=Reply+#13#10;
      B:=TEncoding.UTF8.GetAnsiBytes(Reply);
      Transport.WriteBytes(B,Length(B));
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
