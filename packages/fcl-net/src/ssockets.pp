{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}{$H+}
{$R-}

{$IFNDEF FPC_DOTTEDUNITS}
unit ssockets;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, System.CTypes, System.Net.Sockets;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, ctypes, sockets;
{$ENDIF FPC_DOTTEDUNITS}

type

  TSocketErrorType = (
    seHostNotFound,
    seCreationFailed,
    seBindFailed,
    seListenFailed,
    seConnectFailed,
    seConnectTimeOut,
    seAcceptFailed,
    seAcceptWouldBlock,
    seIOTimeOut);

  TSocketOption = (soDebug,soReuseAddr,soKeepAlive,soDontRoute,soBroadcast,
                   soOOBinline);
  TSocketOptions = Set of TSocketOption;

  ESocketError = class(Exception)
    Code: TSocketErrorType;
    constructor Create(ACode: TSocketErrorType; const MsgArgs: array of const);overload;
  end;

  TAcceptErrorAction = (aeaRaise,aeaIgnore,aeaStop);
  TSocketStream = Class;
  TSocketServer = Class;
  TInetSocket = Class;
{$IFDEF UNIX}
  TUnixSocket = class;
  TUnixSocketClass = Class of TUnixSocket;
{$ENDIF}
  TSocketStreamClass = Class of TSocketStream;
  TInetSocketClass = Class of TInetSocket;



  // Handles all OS calls

  { TSocketHandler }
  TSocketState = (sosCanread,sosCanWrite,sosException);
  TSocketStates = Set of TSocketState;

  TSocketHandler = Class(TObject)
  Private
    FSocket: TSocketStream;
  Protected
    FLastError : integer;
    Procedure SetSocket(const AStream: TSocketStream); virtual;
    Procedure CheckSocket;
  Public
    constructor Create; virtual;
    // Called after the connect call succeded. Returns True to continue, false to close connection.
    function Connect: boolean; virtual;
    // Called after the accept call succeded on the NEW client socket
    function Accept : Boolean; virtual;
    Function Close : Boolean; virtual;
    function Shutdown(BiDirectional : Boolean): boolean; virtual;
    function Select(aCheck : TSocketStates; TimeOut : Integer): TSocketStates; virtual;
    function CanRead(TimeOut : Integer): Boolean; virtual;
    function Recv(Const Buffer; Count: Integer): Integer; virtual;
    function Send(Const Buffer; Count: Integer): Integer; virtual;
    function BytesAvailable: Integer; virtual;
    // Call this to get extra error info.
    Function GetLastErrorDescription : String; virtual;
    Property Socket : TSocketStream Read FSocket;
    Property LastError : Integer Read FLastError;
  end;
  TSocketHandlerClass = Class of TSocketHandler;

  { TSocketStream }
  TSocketStreamArray = Array of TSocketStream;

  TSocketStream = class(THandleStream)
  Private
    FClosed: Boolean;
    FOnClose: TNotifyEvent;
    FPeerClosed: Boolean;
    FReadFlags: Integer;
    FSocketInitialized : Boolean;
    FSocketOptions : TSocketOptions;
    FWriteFlags: Integer;
    FHandler : TSocketHandler;
    FIOTimeout : Integer;
    FConnectTimeout : Integer;
    function GetLastError: Integer;
    Procedure GetSockOptions;
    procedure SetConnectTimeout(AValue: Integer);
    Procedure SetSocketOptions(Value : TSocketOptions);
    function GetLocalAddress: {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.TSockAddr;
    function GetRemoteAddress: {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.TSockAddr;
    procedure SetIOTimeout(AValue: Integer);
  Protected
    Procedure DoOnClose; virtual;
  Public
    Constructor Create (AHandle : Longint; AHandler : TSocketHandler = Nil);virtual;
    destructor Destroy; override;
    Class Function Select(Var aRead,aWrite,aExceptions : TSocketStreamArray; aTimeOut: Integer): Boolean; virtual;
    Procedure Close;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Select(aCheck : TSocketStates; TimeOut : Integer): TSocketStates;
    Function CanRead(TimeOut : Integer): Boolean; virtual;
    Function Read (Var Buffer; Count : Longint) : longint; Override;
    Function Write (Const Buffer; Count : Longint) :Longint; Override;
    Property SocketOptions : TSocketOptions Read FSocketOptions
                                            Write SetSocketOptions;
    property LocalAddress: {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.TSockAddr read GetLocalAddress;
    property RemoteAddress: {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.TSockAddr read GetRemoteAddress;
    Property LastError : Integer Read GetLastError;
    Property ReadFlags : Integer Read FReadFlags Write FReadFlags;
    Property WriteFlags : Integer Read FWriteFlags Write FWriteFlags;
    Property IOTimeout : Integer read FIOTimeout Write SetIOTimeout;
    Property ConnectTimeout : Integer read FConnectTimeout Write SetConnectTimeout;
    Property OnClose : TNotifyEvent Read FOnClose Write FOnClose;
    Property Handler : TSocketHandler Read FHandler;
    // We called close
    Property Closed : Boolean read FClosed;
    // Peer closed detected when reading.
    Property PeerClosed : Boolean Read FPeerClosed;
  end;

  TSocketClientEvent = Procedure (Sender : TObject; Data : TSocketStream) Of Object;
  TConnectEvent = TSocketClientEvent;
  TDisconnectEvent = TSocketClientEvent;
  TConnectionDroppedEvent = TSocketClientEvent;
  TConnectQuery = Procedure (Sender : TObject; ASocket : Longint; Var Allow : Boolean) of Object;
  TOnAcceptError = Procedure (Sender : TObject; ASocket : Longint; E : Exception; Var ErrorAction : TAcceptErrorAction) of Object;
  TGetClientSocketHandlerEvent = Procedure (Sender : TObject; Out AHandler : TSocketHandler) of object;
  TForeachHandler = Procedure (Sender : TObject; aClient : TSocketStream; var aContinue : Boolean) of object;

  { TSocketServer }

  TSocketServer = Class(TObject)
  Private
    FIdleTimeOut: Cardinal;
    FMaxSimultaneousConnections: longint;
    FOnAcceptError: TOnAcceptError;
    FOnConnectionDropped: TConnectionDroppedEvent;
    FOnCreateClientSocketHandler: TGetClientSocketHandlerEvent;
    FOnDisconnect: TDisconnectEvent;
    FOnIdle : TNotifyEvent;
    FNonBlocking : Boolean;
    FSocket : longint;
    FAccepting : Boolean;
    FMaxConnections : Longint;
    FQueueSize : Longint;
    FOnConnect : TConnectEvent;
    FOnConnectQuery : TConnectQuery;
    FHandler : TSocketHandler;
    FConnections : TThreadList;
    Procedure DoOnIdle;
    function GetConnectionCount: Integer;
    Function GetReuseAddress: Boolean;
    Function GetKeepAlive : Boolean;
    Function GetLinger : Integer;
    Procedure SetReuseAddress (AValue : Boolean);
    Procedure SetKeepAlive (AValue : Boolean);
    Procedure SetLinger(ALinger : Integer);
  Protected
    FSockType : Longint;
    FBound : Boolean;
    Procedure SocketClosed(aSocket: TSocketStream);
    Procedure DoConnectionDropped(aSocket : TSocketStream); virtual;
    Procedure DoDisconnect(aSocket : TSocketStream); virtual;
    Procedure DoConnect(ASocket : TSocketStream); Virtual;
    Function  DoConnectQuery(ASocket : longint): Boolean ;Virtual;
    Procedure Bind; Virtual; Abstract;
    Function  Accept: Longint;Virtual;Abstract;
    Function  SockToStream (ASocket : Longint) : TSocketStream;Virtual;Abstract;
    Procedure Close; Virtual;
    Procedure Abort;
    Procedure RemoveSelfFromConnections; virtual;

    Function RunIdleLoop : Boolean;
    function GetConnection: TSocketStream; virtual; abstract;
    Function HandleAcceptError(E : ESocketError) : TAcceptErrorAction;
    Function GetClientSocketHandler(aSocket : Longint) : TSocketHandler; virtual;
    Property Handler : TSocketHandler Read FHandler;
  Public
    Constructor Create(ASocket : Longint; AHandler : TSocketHandler);
    Destructor Destroy; Override;
    Procedure Listen;
    function  GetSockopt(ALevel,AOptName : cint; var optval; Var optlen : tsocklen): Boolean;
    function  SetSockopt(ALevel,AOptName : cint; var optval; optlen : tsocklen): Boolean;
    Procedure StartAccepting;
    Procedure StopAccepting(DoAbort : Boolean = False);
    Procedure SetNonBlocking;
    Procedure Foreach(aHandler : TForeachHandler);
    Property Bound : Boolean Read FBound;
    // Maximium number of connections in total. *Not* the simultaneous connection count. -1 keeps accepting.
    Property MaxConnections : longint Read FMaxConnections Write FMaxConnections;
    Property MaxSimultaneousConnections : longint Read FMaxSimultaneousConnections Write FMaxSimultaneousConnections;

    Property QueueSize : Longint Read FQueueSize Write FQueueSize default 5;
    Property OnConnect : TConnectEvent Read FOnConnect Write FOnConnect;
    Property OnDisconnect : TDisconnectEvent Read FOnDisconnect Write FOnDisconnect;
    Property OnConnectionDropped : TConnectionDroppedEvent Read FOnConnectionDropped Write FOnConnectionDropped;
    Property OnConnectQuery : TConnectQuery Read FOnConnectQuery Write FOnConnectQuery;
    Property OnAcceptError : TOnAcceptError Read FOnAcceptError Write FOnAcceptError;
    Property OnIdle : TNotifyEvent Read FOnIdle Write FOnIdle;
    Property NonBlocking : Boolean Read FNonBlocking;
    Property Socket : Longint Read FSocket;
    Property SockType : Longint Read FSockType;
    Property KeepAlive : Boolean Read GetKeepAlive Write SetKeepAlive;
    Property ReuseAddress : Boolean Read GetReuseAddress Write SetReuseAddress;
    // -1 means no linger. Any value >=0 sets linger on.
    Property Linger: Integer Read GetLinger Write Setlinger;
    // Accept Timeout in milliseconds.
    // If Different from 0, then there will be an idle loop before accepting new connections, Calling OnIdle if no new connection appeared in the specified timeout.
    Property AcceptIdleTimeOut : Cardinal Read FIdleTimeOut Write FIdleTimeout;
    Property OnCreateClientSocketHandler : TGetClientSocketHandlerEvent Read FOnCreateClientSocketHandler Write FOnCreateClientSocketHandler;
    Property ConnectionCount : Integer Read GetConnectionCount;
  end;

  { TInetServer }

  TInetServer = Class(TSocketServer)
  private
    FAddr : TINetSockAddr;
    FPort : Word;
    FHost: string;
  Protected
    Function GetConnection: TSocketStream; override;
    Function SockToStream (ASocket : Longint) : TSocketStream;Override;
    Function Accept : Longint;override;
    Property Addr : TINetSockAddr Read FAddr;
  Public
    DefaultInetSocketClass : TInetSocketClass;
  Public
    Procedure Bind; Override;
    Constructor Create(APort: Word);
    Constructor Create(const aHost: string; const APort: Word; AHandler : TSocketHandler = Nil);
    Property Port : Word Read FPort;
    Property Host : string Read FHost;
  end;

{$ifdef Unix}

  { TUnixServer }

  TUnixServer = Class(TSocketServer)
  Private
    FUnixAddr : TUnixSockAddr;
    FFileName : String;
  Protected
    Procedure Bind; Override;
    Function Accept : Longint;override;
    function GetConnection: TSocketStream; override;
    Function SockToStream (ASocket : Longint) : TSocketStream;Override;
    Procedure Close; override;
  Public
    DefaultUnixSocketClass : TUnixSocketClass;
  Public
    Constructor Create(const AFileName : String; AHandler : TSocketHandler = Nil);
    Property FileName : String Read FFileName;
  end;
{$endif}

  { TInetSocket }
  TBlockingMode = (bmBlocking,bmNonBlocking);
  TBlockingModes = Set of TBlockingMode;
  TCheckTimeoutResult = (ctrTimeout,ctrError,ctrOK);

  { TServerSocketStream }

  TServerSocketStream = class(TSocketStream)
  Protected
    FServer : TSocketServer;
  Protected
    Procedure DoOnClose; override;
    Property Server : TSocketServer Read FServer;
  Public
    Function CanRead(TimeOut : Integer): Boolean; override;
  end;

{$if defined(unix) or defined(windows)}
{$DEFINE HAVENONBLOCKING}
{$endif}
  TInetSocket = Class(TSocketStream)
  Private
    FHost : String;
    FPort : Word;
  Protected
{$IFDEF HAVENONBLOCKING}
    function SetSocketBlockingMode(ASocket: cint; ABlockMode: TBlockingMode; AFDSPtr: Pointer): boolean; virtual;
    function CheckSocketConnectTimeout(ASocket: cint; AFDSPtr: Pointer; ATimeVPtr: Pointer): TCheckTimeoutResult; virtual;
{$ENDIF}
  Public
    Constructor Create(const AHost: String; APort: Word; AHandler : TSocketHandler = Nil); Overload;
    Constructor Create(const AHost: String; APort: Word; aConnectTimeout : Integer; AHandler : TSocketHandler = Nil); Overload;
    Procedure Connect; Virtual;
    Property Host : String Read FHost;
    Property Port : Word Read FPort;
  end;

{$ifdef Unix}

  TUnixSocket = Class(TSocketStream)
  Private
    FFileName : String;
  Protected
    Procedure DoConnect(ASocket : longint); Virtual;
  Public
    Constructor Create(ASocket : Longint); Overload;
    Constructor Create(const AFileName : String); Overload;
    Property FileName : String Read FFileName;
  end;
{$endif}

Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
// This must be here, to prevent it from overriding the sockets definitions... :/
{$ifdef unix}
  UnixApi.Base,UnixApi.Unix,
{$endif}
{$ifdef Windows}
  WinApi.Winsock2, WinApi.Windows,
{$endif}
  System.Net.Resolve;
{$ELSE FPC_DOTTEDUNITS}
uses
// This must be here, to prevent it from overriding the sockets definitions... :/
{$ifdef unix}
  BaseUnix,Unix,
{$endif}
{$ifdef windows}
  winsock2, windows,
{$endif}
  resolve;
{$ENDIF FPC_DOTTEDUNITS}

Const
  SocketBlockingMode = 0;
  SocketNonBlockingMode = 1;


{ ---------------------------------------------------------------------
  ESocketError
  ---------------------------------------------------------------------}

resourcestring
  strHostNotFound = 'Host name resolution for "%s" failed.';
  strSocketCreationFailed = 'Creation of socket failed: %s';
  strSocketBindFailed = 'Binding of socket failed: %s';
  strSocketListenFailed = 'Listening on port #%d failed, error: %d';
  strSocketConnectFailed = 'Connect to %s failed: %s';
  strSocketAcceptFailed = 'Could not accept a client connection on socket: %d, error %d';
  strSocketAcceptWouldBlock = 'Accept would block on socket: %d';
  strSocketIOTimeOut = 'Failed to set IO Timeout to %d';
  strErrNoStream = 'Socket stream not assigned';
  strSocketConnectTimeOut = 'Connection to %s timed out.';

{ TServerSocketStream }

function TServerSocketStream.CanRead(TimeOut : Integer): Boolean;
begin
  Result:=inherited CanRead(TimeOut);
  Result:=Result and Assigned(FServer); // main server is gone, cannot read from it
end;

procedure TServerSocketStream.DoOnClose;
begin
  if Assigned(FServer) then
    FServer.SocketClosed(Self);
  inherited DoOnClose;
end;

{ TSocketHandler }

Procedure TSocketHandler.SetSocket(const AStream: TSocketStream);
begin
  FSocket:=AStream;
end;

Procedure TSocketHandler.CheckSocket;
begin
  If not Assigned(FSocket) then
    Raise ESocketError.Create(StrErrNoStream);
end;

constructor TSocketHandler.Create;
begin
  FSocket:=Nil;
end;

function TSocketHandler.Connect: boolean;

begin
  // Only descendents can change this
  Result:=True;
end;

function TSocketHandler.Accept : Boolean;


begin
  // Only descendents can change this
  Result:=True;
end;

function TSocketHandler.Shutdown(BiDirectional: Boolean): boolean;
begin
  CheckSocket ;
  Result:=False;
end;

function TSocketHandler.Select(aCheck: TSocketStates; TimeOut: Integer): TSocketStates;
{$if defined(unix) or defined(windows)}
var
  FDSR,FDSW,FDSE : TFDSet;
  PFDSR,PFDSW,PFDSE : PFDSet;
  TimeV: TTimeVal;
  PTV : ^TTimeVal;
  res : Longint;

  Procedure DoSet(var FDS : TFDSet; var PFDS : PFDSet; aState : TSocketState);

  begin
    if not (aState in aCheck) then
      PFDS:=nil
    else
      begin
      FDS := Default(TFDSet);
      {$ifdef unix}
      fpFD_Zero(FDS);
      fpFD_Set(FSocket.Handle, FDS);
      {$endif}
      {$ifdef windows}
      FDS := Default(TFDSet);
      FD_Zero(FDS);
      FD_Set(FSocket.Handle, FDS);
      {$ENDIF}
      PFDS:=@FDS;
      end
  end;

  Procedure CheckSet(var FDS : TFDSet; aState : TSocketState);

  begin
    if aState in aCheck then
      begin
      {$ifdef unix}
      if fpFD_IsSet(FSocket.Handle, FDS)>0 then
        Include(Result,aState);
      {$endif}
      {$ifdef windows}
      if FD_IsSet(FSocket.Handle, FDS) then
        Include(Result,aState);
      {$endif}
      end;
  end;

{$endif}
begin
  Result:=[];
{$if defined(unix) or defined(windows)}
  Res:=-1;
  if Timeout<0 then
    PTV:=Nil
  else
    begin
    TimeV.tv_usec := (TimeOut mod 1000) * 1000;
    TimeV.tv_sec := TimeOut div 1000;
    PTV:=@TimeV;
    end;
  DoSet(FDSR,PFDSR,sosCanRead);
  DoSet(FDSW,PFDSW,sosCanWrite);
  DoSet(FDSE,PFDSE,sosException);
{$endif}
{$ifdef unix}
  Res:=fpSelect(Socket.Handle + 1, PFDSR, PFDSW, PFDSE, PTV);
{$endif}
{$ifdef windows}
  Res:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Winsock2.Select(Socket.Handle + 1, PFDSR, PFDSW, PFDSE, @TimeV);
{$endif}
{$if defined(unix) or defined(windows)}
  if Res>0 then
    begin
    CheckSet(FDSR,sosCanRead);
    CheckSet(FDSW,sosCanWrite);
    CheckSet(FDSE,sosException);
    end;
  if Res<0 then
    FLastError:=SocketError
  else
    FLastError:=0;
{$else}
  FLastError:=0;    
{$endif}
end;

function TSocketHandler.CanRead(TimeOut : Integer): Boolean;
begin
  Result:=Select([sosCanRead],Timeout)<>[];
end;

function TSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

Var
  Flags : longint;
begin
  Flags:=Socket.FReadFlags;
{$ifdef unix}
  FLastError:=ESysEINTR;
  While (FlastError=ESysEINTR) do
{$endif}
    begin
    Result:=fprecv(Socket.Handle,@Buffer,count,flags);
    If (Result<0) then
      FLastError:=SocketError
    else
      FLastError:=0;
    end;
end;

function TSocketHandler.Send(Const Buffer; Count: Integer): Integer;

Var
  Flags : longint;

begin
  Flags:=FSocket.FWriteFlags;
{$ifdef unix}
  FLastError:=ESysEINTR;
  While (FlastError=ESysEINTR) do
{$endif}
    begin
    Result:=fpsend(Socket.Handle,@Buffer,count,flags);
    If Result<0 then
      FLastError:=SocketError
    else
      FlastError:=0;
    end;
end;

function TSocketHandler.BytesAvailable: Integer;
begin
  Result:=0;
  { we need ioctlsocket here }
end;

function TSocketHandler.GetLastErrorDescription: String;
begin
  Result:='';
end;


Function TSocketHandler.Close: Boolean;
begin
  Result:=True;
end;


constructor ESocketError.Create(ACode: TSocketErrorType; const MsgArgs: array of const);
var
  s: String;
begin
  Code := ACode;
  case ACode of
    seHostNotFound     : s := strHostNotFound;
    seCreationFailed   : s := strSocketCreationFailed;
    seBindFailed       : s := strSocketBindFailed;
    seListenFailed     : s := strSocketListenFailed;
    seConnectFailed    : s := strSocketConnectFailed;
    seAcceptFailed     : s := strSocketAcceptFailed;
    seAcceptWouldBLock : S := strSocketAcceptWouldBlock;
    seIOTimeout        : S := strSocketIOTimeOut;
    seConnectTimeOut   : s := strSocketConnectTimeout;
  end;
  s := Format(s, MsgArgs);
  inherited Create(s);
end;

{ ---------------------------------------------------------------------
    TSocketStream
  ---------------------------------------------------------------------}
constructor TSocketStream.Create(AHandle: Longint; AHandler: TSocketHandler);

begin
  Inherited Create(AHandle);
  FSocketInitialized := true;
  GetSockOptions;
  FHandler:=AHandler;
  If (FHandler=Nil) then
    FHandler:=TSocketHandler.Create;
  FHandler.SetSocket(Self);
end;

destructor TSocketStream.Destroy;
begin
  Close;
  inherited Destroy;
end;

class function TSocketStream.Select(var aRead, aWrite,
  aExceptions: TSocketStreamArray; aTimeOut: Integer): Boolean;

{$if defined(unix) or defined(windows)}
var
  FDR,FDW,FDE: TFDSet;
  TimeV: TTimeVal;
  MaxHandle : Longint;

  Procedure FillFD(var FD : TFDSet; anArray : TSocketStreamArray);

  Var
    S : TSocketStream;

  begin
    FD := Default(TFDSet);
    {$ifdef unix}
    fpFD_Zero(FD);
    For S in AnArray do
      begin
      fpFD_Set(S.Handle, FD);
      if S.Handle>MaxHandle then
        MaxHandle:=S.Handle;
      end;
    {$ENDIF}
    {$ifdef windows}
    FD_Zero(FD);
    For S in AnArray do
      begin
      FD_Set(S.Handle, FD);
      if S.Handle>MaxHandle then
        MaxHandle:=S.Handle;
      end;
    {$ENDIF}
  end;

  function FillArr(FD : TFDSet; Src : TSocketStreamArray) : TSocketStreamArray;

  Var
    S : TSocketStream;
    aLen : Integer;

  begin
    Result:=nil;
    SetLength(Result,Length(Src));
    aLen:=0;
    For S in Src do
      begin
{$IFDEF UNIX}
      if fpFD_IsSet(S.Handle, FD)>0 then
{$ENDIF}
{$IFDEF Windows}
      if FD_isSet(S.Handle, FD) then
{$ENDIF}
        begin
        Result[aLen]:=S;
        Inc(aLen);
        end;
      end;
    SetLength(Result,aLen);
  end;
  
{$ENDIF} // Unix or windows

begin
  Result:=False;
{$if defined(unix) or defined(windows)}
  MaxHandle:=0;
  TimeV.tv_usec := (aTimeOut mod 1000) * 1000;
  TimeV.tv_sec := aTimeOut div 1000;
  FillFD(FDR,aRead);
  FillFD(FDW,aWrite);
  FillFD(FDE,aExceptions);
  if MaxHandle=0 then
    exit;
{$ifdef unix}
  Result := fpSelect(MaxHandle+1, @FDR, @FDW, @FDE, @TimeV) > 0;
{$endif}
{$ifdef windows}
  Result := {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Winsock2.Select(MaxHandle+1, @FDR, @FDW, @FDE, @TimeV) > 0;
{$endif}
  aRead:=FillArr(FDR,aRead);
  aWrite:=FillArr(FDR,aRead);
  aExceptions:=FillArr(FDR,aRead);
{$ELSE}  // Unix or windows
  aRead:=[];
  aWrite:=[];
  aExceptions:=[];
{$ENDIF}  
end;

procedure TSocketStream.Close;
begin
  DoOnClose;
  if FSocketInitialized then
    FHandler.Close; // Ignore the result
  FSocketInitialized:=False;
  FreeAndNil(FHandler);
  CloseSocket(Handle);
  FClosed:=True;
end;

procedure TSocketStream.GetSockOptions;
{$ifdef windows}
var
  opt: DWord;
  olen: tsocklen;
{$endif windows}
{$ifdef unix}
var
  time: ttimeval;
  olen: tsocklen;
{$endif unix}
begin
  {$ifdef windows}
  olen:=4;
  if fpgetsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, @opt, @olen) = 0 then
    FIOTimeout:=opt;
  {$endif windows}
  {$ifdef unix}
  olen:=sizeof(time);
  if fpgetsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, @time, @olen) = 0 then
    FIOTimeout:=(time.tv_sec*1000)+(time.tv_usec div 1000);
  {$endif}
end;

procedure TSocketStream.SetConnectTimeout(AValue: Integer);
begin
  if FConnectTimeout = AValue then Exit;
  FConnectTimeout := AValue;
end;

function TSocketStream.GetLastError: Integer;
begin
  Result:=FHandler.LastError;
end;

procedure TSocketStream.SetSocketOptions(Value: TSocketOptions);

begin
  FSocketOptions:=Value;
end;

function TSocketStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Result:=0;
end;

function TSocketStream.Select(aCheck: TSocketStates; TimeOut: Integer): TSocketStates;
begin
  Result:=FHandler.Select(aCheck,TimeOut);
end;

function TSocketStream.CanRead(TimeOut: Integer): Boolean;
begin
  Result:=FHandler.CanRead(TimeOut);
end;

function TSocketStream.Read(var Buffer; Count: Longint): longint;

begin
  Result:=FHandler.Recv(Buffer,Count);
  if (Result=0) then
    FPeerClosed:=True;
end;

function TSocketStream.Write(const Buffer; Count: Longint): Longint;

begin
  Result:=FHandler.Send(Buffer,Count);
end;

function TSocketStream.GetLocalAddress: {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf({$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.TSockAddr);
  if fpGetSockName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

function TSocketStream.GetRemoteAddress: {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf({$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}sockets.TSockAddr);
  if fpGetPeerName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TSocketStream.SetIOTimeout(AValue: Integer);

Var
  E : Boolean;
{$ifdef windows}
  opt: DWord;
{$endif windows}
{$ifdef unix}
  time: ttimeval;
{$endif unix}

begin
  E:=False;
  if FIOTimeout=AValue then Exit;
  FIOTimeout:=AValue;

  {$ifdef windows}
  opt := AValue;
  E:=fpsetsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, @opt, 4)<>0;
  if not E then
    E:=fpsetsockopt(Handle, SOL_SOCKET, SO_SNDTIMEO, @opt, 4)<>0;
  {$endif windows}
  {$ifdef unix}
  time.tv_sec:=avalue div 1000;
  time.tv_usec:=(avalue mod 1000) * 1000;
  E:=fpsetsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, @time, sizeof(time))<>0;
  if not E then
    E:=fpsetsockopt(Handle, SOL_SOCKET, SO_SNDTIMEO, @time, sizeof(time))<>0;
  {$endif}
  if E then
    Raise ESocketError.Create(seIOTimeout,[AValue]);
end;

procedure TSocketStream.DoOnClose;
begin
  If Assigned(FOnClose) then
    FOnClose(Self);
end;

{ ---------------------------------------------------------------------
    TSocketServer
  ---------------------------------------------------------------------}

constructor TSocketServer.Create(ASocket: Longint; AHandler: TSocketHandler);

begin
  FSocket:=ASocket;
  FQueueSize :=5;
  FMaxConnections:=-1;
  if (AHandler=Nil) then
    AHandler:=TSocketHandler.Create;
  FHandler:=AHandler;
  FConnections:=TThreadList.Create;
end;

destructor TSocketServer.Destroy;

begin
  RemoveSelfFromConnections;
  FreeAndNil(FConnections);
  Close;
  FreeAndNil(FHandler);
  Inherited;
end;

procedure TSocketServer.Close;

begin
  If FSocket<>-1 Then
    CloseSocket(FSocket);
  FSocket:=-1;
end;

procedure TSocketServer.Abort;
{$if defined(unix) or defined(mswindows) or defined(hasamiga)}
{$else}
var
  ASocket: longint;
{$endif}
begin
{$if defined(unix)}
  fpShutdown(FSocket,SHUT_RDWR);
{$elseif defined(mswindows) or defined(hasamiga)}
  CloseSocket(FSocket);
{$else}
  {$WARNING Method Abort is not tested on this platform!}
  ASocket:=FSocket;
  fpShutdown(ASocket,SHUT_RDWR);
  CloseSocket(ASocket);
{$endif}
end;

procedure TSocketServer.RemoveSelfFromConnections;

Var
  L : TList;
  P: Pointer;

begin
  L:=FConnections.LockList;
  try
    for P in L do
      TServerSocketStream(P).FServer:=Nil;
  finally
    FConnections.UnlockList;
  end;
end;

function TSocketServer.RunIdleLoop: Boolean;

// Run Accept idle loop. Return True if there is a new connection waiting
{$if defined(unix) or defined(windows)}
var
  FDS: TFDSet;
  TimeV: TTimeVal;
{$endif}
begin
  Repeat
    Result:=False;
{$if defined(unix) or defined(windows)}
    TimeV.tv_usec := (AcceptIdleTimeout mod 1000) * 1000;
    TimeV.tv_sec := AcceptIdleTimeout div 1000;
{$endif}
{$ifdef unix}
    FDS := Default(TFDSet);
    fpFD_Zero(FDS);
    fpFD_Set(FSocket, FDS);
    Result := fpSelect(FSocket + 1, @FDS, @FDS, @FDS, @TimeV) > 0;
{$else}
{$ifdef windows}
    FDS := Default(TFDSet);
    FD_Zero(FDS);
    FD_Set(FSocket, FDS);
    Result := {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Winsock2.Select(FSocket + 1, @FDS, @FDS, @FDS, @TimeV) > 0;
{$endif}
{$endif}
    If Result then
      break;
    DoOnIdle;
  Until (Not FAccepting);
end;

procedure TSocketServer.Listen;

begin
  If Not FBound then
    Bind;
  If  {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.FpListen(FSocket,FQueueSize)<>0 then
    Raise ESocketError.Create(seListenFailed,[FSocket,SocketError]);
end;

function TSocketServer.GetSockopt(ALevel, AOptName: cint; var optval;
  var optlen: tsocklen): Boolean;
begin
  Result:=fpGetSockOpt(FSocket,ALevel,AOptName,@optval,@optlen)<>-1;
end;

function TSocketServer.SetSockopt(ALevel, AOptName: cint; var optval;
  optlen: tsocklen): Boolean;
begin
  Result:=fpSetSockOpt(FSocket,ALevel,AOptName,@optval,optlen)<>-1;
end;

function TInetServer.GetConnection: TSocketStream;

var
  NewSocket : longint;

begin
  Result:=Nil;
  NewSocket:=Accept;
  if (NewSocket<0) then
    if not FAccepting then
      exit
    else
      Raise ESocketError.Create(seAcceptFailed,[Socket,SocketError]);
  If FAccepting and DoConnectQuery(NewSocket) Then
    Result:=SockToStream(NewSocket)
  else
    CloseSocket(NewSocket);
end;

function TSocketServer.HandleAcceptError(E: ESocketError): TAcceptErrorAction;
begin
  if FAccepting then
    Result:=aeaRaise
  else
    Result:=aeaStop;
  if Assigned(FOnAcceptError) then
    FOnAcceptError(Self,FSocket,E,Result);
end;

function TSocketServer.GetClientSocketHandler(aSocket : Longint): TSocketHandler;
begin
  If Assigned(FOnCreateClientSocketHandler) then
    FOnCreateClientSocketHandler(Self,Result)
  else
    if Assigned(FHandler) then
      Result:=TSocketHandlerClass(FHandler.ClassType).Create;
end;

procedure TSocketServer.StartAccepting;

Var
 NoConnections : Integer;
 Stream : TSocketStream;

begin
  FAccepting := True;
  NoConnections := 0;
  Listen;
  Repeat
    Repeat
      Stream:=Nil;
      Try
        If (AcceptIdleTimeOut=0) or RunIdleLoop then
          Stream:=GetConnection;
        if Assigned(Stream) then
          if (MaxSimultaneousConnections>0) and (ConnectionCount>=MaxSimultaneousConnections) then
            begin
            Stream.Close;
            DoConnectionDropped(Stream);
            FreeAndNil(Stream);
            end
          else
            begin
            if Stream is TServerSocketStream then
              begin
              FConnections.Add(Stream);
              TServerSocketStream(Stream).FServer:=Self;
              end;
            Inc(NoConnections);
            DoConnect(Stream);
            end;
      except
        On E : ESocketError do
          begin
          If E.Code=seAcceptWouldBlock then
            DoOnIdle
          else
            Case HandleAcceptError(E) of
              aeaIgnore : ;
              aeaStop : FAccepting:=False;
              aeaRaise : Raise;
            end;
          end;
       end;
    Until (Stream<>Nil) or (Not NonBlocking);
  Until Not (FAccepting) or ((FMaxConnections<>-1) and (NoConnections>=FMaxConnections));
end;

procedure TSocketServer.StopAccepting(DoAbort: Boolean = False);

begin
  FAccepting:=False;
  If DoAbort then
    Abort;
end;

procedure TSocketServer.DoOnIdle;

begin
  If Assigned(FOnIdle) then
    FOnIdle(Self);
end;

function TSocketServer.GetConnectionCount: Integer;

Var
  L : TList;

begin
  L:=FConnections.LockList;
  try
    Result:=L.Count;
  finally
    FConnections.UnlockList;
  end;
end;

function TSocketServer.GetReuseAddress: Boolean;
Var
  L : cint;
  ls : Tsocklen;
begin
  L:=0;
  ls:=0;
{$IFDEF UNIX}
  if not GetSockOpt(SOL_SOCKET, SO_REUSEADDR, L, LS) then
    Raise ESocketError.CreateFmt('Failed to get SO_REUSEADDR to %d: %d',[l,socketerror]);
  Result:=(L<>0);
{$ELSE}
  Result:=True;
{$ENDIF}

end;

function TSocketServer.GetKeepAlive: Boolean;
Var
  L : cint;
  ls : Tsocklen;
begin
  L:=0;
  ls:=0;
{$IFDEF UNIX}
  if Not GetSockOpt(SOL_SOCKET, SO_KEEPALIVE, L, LS) then
    Raise ESocketError.CreateFmt('Failed to get SO_KEEPALIVE: %d',[socketerror]);
  Result:=(L<>0);
{$ELSE}
  Result:=True;
{$ENDIF}
end;

function TSocketServer.GetLinger: Integer;
Var
  L : linger;
  ls : tsocklen;

begin
  L.l_onoff:=0;
  l.l_linger:=0;
  if Not GetSockOpt(SOL_SOCKET, SO_LINGER, l, ls) then
    Raise ESocketError.CreateFmt('Failed to set linger: %d',[socketerror]);
  if l.l_onoff=0 then
    Result:=-1
  else
    Result:=l.l_linger;
end;

procedure TSocketServer.DoConnect(ASocket: TSocketStream);

begin
  If Assigned(FOnConnect) Then
    FOnConnect(Self,ASocket);
end;

function TSocketServer.DoConnectQuery(ASocket: longint): Boolean;

begin
  Result:=True;
  If Assigned(FOnConnectQuery) then
    FOnConnectQuery(Self,ASocket,Result);
end;

procedure TSocketServer.SetNonBlocking;

begin
{$ifdef Unix}
  fpfcntl(FSocket,F_SETFL,O_NONBLOCK);
{$endif}
  FNonBlocking:=True;
end;

procedure TSocketServer.Foreach(aHandler: TForeachHandler);

Var
  L : TList;
  P : Pointer;
  aContinue : Boolean;

begin
  L:=FConnections.LockList;
  try
    aContinue:=True;
    For P in L do
      begin
      aHandler(Self,TSocketStream(P),aContinue);
      if not aContinue then
        break;
      end;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TSocketServer.SetLinger(ALinger: Integer);
Var
  L : linger;
begin
  L.l_onoff:=Ord(ALinger>0);
  if ALinger<0 then
    l.l_linger:=ALinger
  else
    l.l_linger:=0;
  if Not SetSockOpt(SOL_SOCKET, SO_LINGER, l, SizeOf(L)) then
    Raise ESocketError.CreateFmt('Failed to set linger: %d',[socketerror]);
end;

procedure TSocketServer.SocketClosed(aSocket: TSocketStream);
begin
  FConnections.Remove(aSocket);
  DoDisConnect(aSocket);
end;

procedure TSocketServer.DoConnectionDropped(aSocket: TSocketStream);
begin
  If Assigned(FOnConnectionDropped) then
    FOnConnectionDropped(Self,aSocket);
end;

procedure TSocketServer.DoDisconnect(aSocket: TSocketStream);
begin
  If Assigned(FOnDisconnect) then
    FOnDisconnect(Self,aSocket);
end;

procedure TSocketServer.SetReuseAddress(AValue: Boolean);
Var
  L : cint;
begin
  L:=Ord(AValue);
{$IFDEF UNIX}
  if not SetSockOpt(SOL_SOCKET, SO_REUSEADDR , L, SizeOf(L)) then
    Raise ESocketError.CreateFmt('Failed to set SO_REUSEADDR to %d: %d',[l,socketerror]);
{$ENDIF}
end;

procedure TSocketServer.SetKeepAlive(AValue: Boolean);
Var
  L : cint;
begin
  L:=Ord(AValue);
{$IFDEF UNIX}
  if Not SetSockOpt(SOL_SOCKET, SO_KEEPALIVE, L, SizeOf(L)) then
    Raise ESocketError.CreateFmt('Failed to set SO_REUSEADDR to %d: %d',[l,socketerror]);
{$ENDIF}
end;

{ ---------------------------------------------------------------------
    TInetServer
  ---------------------------------------------------------------------}

constructor TInetServer.Create(APort: Word);

begin
  Create('0.0.0.0', aPort);
end;

constructor TInetServer.Create(const aHost: string; const APort: Word;
  AHandler: TSocketHandler);

Var S : longint;

begin
  FHost:=aHost;
  FPort:=APort;
  S:={$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.FpSocket(AF_INET,SOCK_STREAM,0);
  If S=-1 Then
    Raise ESocketError.Create(seCreationFailed,[Format('%d',[APort])]);
  Inherited Create(S,AHandler);
end;

procedure TInetServer.Bind;

begin
  Faddr.sin_family := AF_INET;
  Faddr.sin_port := ShortHostToNet(FPort);
  Faddr.sin_addr.s_addr := LongWord(StrToNetAddr(FHost));
  if {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.fpBind(FSocket, @FAddr, Sizeof(FAddr))<>0 then
    raise ESocketError.Create(seBindFailed, [IntToStr(FPort)]);
  FBound:=True;
end;

function TInetServer.SockToStream(ASocket: Longint): TSocketStream;
Var
  H : TSocketHandler;
  ok : Boolean;
  aClass : TInetSocketClass;

  procedure ShutDownH;
  begin
    H.Shutdown(False);
    FreeAndNil(Result);
  end;

begin
  H:=GetClientSocketHandler(aSocket);
  aClass:=DefaultInetSocketClass;

  // Should be: Result:=TServerSocketStream.Create(ASocket,H);

  if aClass=Nil then
    aClass:=TInetSocket;
  Result:=aClass.Create(ASocket,H);
  (Result as TInetSocket).FHost:='';
  (Result as TInetSocket).FPort:=FPort;

  ok:=false;
  try
    ok:=H.Accept;
  finally
    if not ok then
      ShutDownH;
  end;
end;

function TInetServer.Accept: Longint;

Var
  L : longint;
  R : integer;
begin
  L:=SizeOf(FAddr);
{$IFDEF UNIX}
  R:=ESysEINTR;
  While (R=ESysEINTR) do
{$ENDIF UNIX}
   begin
   Result:={$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.fpAccept(Socket,@Faddr,@L);
   R:=SocketError;
   end;
{$ifdef Unix}
  If (Result<0) then
    If R=ESysEWOULDBLOCK then
      Raise ESocketError.Create(seAcceptWouldBlock,[socket]);
{$endif}
  if (Result<0) or Not FAccepting then
    begin
    If (Result>=0) then
      CloseSocket(Result);
    // Do not raise an error if we've stopped accepting.
    if FAccepting then
      Raise ESocketError.Create(seAcceptFailed,[Socket,SocketError])
    end;
end;

{ ---------------------------------------------------------------------
    TUnixServer
  ---------------------------------------------------------------------}
{$ifdef Unix}
Constructor TUnixServer.Create(const AFileName : String; AHandler : TSocketHandler = Nil);

Var S : Longint;

begin
  FFileName:=AFileName;
  S:={$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.fpSocket(AF_UNIX,SOCK_STREAM,0);
  If S=-1 then
    Raise ESocketError.Create(seCreationFailed,[AFileName])
  else
    Inherited Create(S,AHandler);
end;

Procedure TUnixServer.Close;
begin
  Inherited Close;
  DeleteFile(FFileName);
  FFileName:='';
end;

Procedure TUnixServer.Bind;

var
  AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,FUnixAddr,AddrLen);
  If  {$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.FpBind(Socket,@FUnixAddr,AddrLen)<>0 then
    Raise ESocketError.Create(seBindFailed,[FFileName]);
  FBound:=True;
end;

Function TUnixServer.Accept : Longint;

Var L : longint;

begin
  L:=Length(FFileName);
  Result:={$IFDEF FPC_DOTTEDUNITS}System.Net.{$ENDIF}Sockets.fpAccept(Socket,@FUnixAddr,@L);
  If Result<0 then
    If SocketError=ESysEWOULDBLOCK then
      Raise ESocketError.Create(seAcceptWouldBlock,[socket])
    else
      Raise ESocketError.Create(seAcceptFailed,[socket,SocketError]);
end;

Function  TUnixServer.SockToStream (ASocket : Longint) : TSocketStream;

var
  aClass : TUnixSocketClass;

begin
  aClass:=DefaultUnixSocketClass;
  if aClass=Nil then
    aClass:=TUnixSocket;
  Result:=aClass.Create(ASocket);
  (Result as TUnixSocket).FFileName:=FFileName;
end;

Function TUnixServer.GetConnection : TSocketStream;

var
  NewSocket : longint;

begin
  Result:=Nil;
  NewSocket:=Accept;
  if (NewSocket<0) then
    Raise ESocketError.Create(seAcceptFailed,[Socket,SocketError]);
  If FAccepting and DoConnectQuery(NewSocket) Then
    Result:=SockToStream(NewSocket)
  else
    CloseSocket(NewSocket);
end;

{$endif}

{ ---------------------------------------------------------------------
    TInetSocket
  ---------------------------------------------------------------------}

Constructor TInetSocket.Create(const AHost: String; APort: Word;AHandler : TSocketHandler = Nil);
begin
  Create(AHost,aPort,0,AHandler);
end;

Constructor TInetSocket.Create(const AHost: String; APort: Word; aConnectTimeout : Integer; AHandler : TSocketHandler = Nil);
Var
  S : Longint;

begin
  FHost:=AHost;
  FPort:=APort;
  ConnectTimeout:=aConnectTimeout;
  S:=fpSocket(AF_INET,SOCK_STREAM,0);
  Inherited Create(S,AHandler);
  if (AHandler=Nil) then // Backwards compatible behaviour.
    Connect;
end;

{$IFDEF HAVENONBLOCKING}
function TInetSocket.SetSocketBlockingMode(ASocket: cint; ABlockMode: TBlockingMode; AFDSPtr: Pointer): Boolean;

Const
    BlockingModes : Array[TBlockingMode] of DWord =
                  (SocketBlockingMode, SocketNonBlockingMode);


var
  locFDS: PFDSet;
{$ifdef unix}
  flags: Integer;
{$endif}
begin
  locFDS := PFDSet(AFDSPtr);
  if (AblockMode = bmNonBlocking) then
    begin
{$ifdef unix}
    locFDS^ := Default(TFDSet);
    fpFD_Zero(locFDS^);
    fpFD_Set(ASocket, locFDS^);
{$else}
{$ifdef windows}
    locFDS^ := Default(TFDSet);
    FD_Zero(locFDS^);
    FD_Set(ASocket, locFDS^);
{$endif}
{$endif}
    end;
{$ifdef unix}
  flags := FpFcntl(ASocket, F_GetFl, 0);
  if (AblockMode = bmNonBlocking) then
    result := FpFcntl(ASocket, F_SetFl, flags or O_NONBLOCK) = 0
  else
    result := FpFcntl(ASocket, F_SetFl, flags and (not O_NONBLOCK)) = 0;
{$endif}
{$ifdef windows}
  result := ioctlsocket(ASocket,longint(FIONBIO),@ABlockMode) = 0;
{$endif}
end;

// Return true if a timeout happened. Will only be called in case of eWouldBlock.
function TInetSocket.CheckSocketConnectTimeout(ASocket: cint; AFDSPtr: Pointer; ATimeVPtr: Pointer): TCheckTimeoutResult;

var
  Err,ErrLen : Longint;
  Res : LongInt;
  locTimeVal: PTimeVal;
  locFDS: PFDSet;

begin
  locTimeVal := PTimeVal(ATimeVPtr);
  locFDS := PFDSet(AFDSPtr);
  locTimeVal^.tv_usec := (FConnectTimeout mod 1000) * 1000;
  locTimeVal^.tv_sec := FConnectTimeout div 1000;
  Res:=-1;
  {$ifdef unix}
    Res:=fpSelect(ASocket + 1, nil, locFDS, nil, locTimeVal); // 0 -> TimeOut
  {$ENDIF}
  {$ifdef windows}
    Res:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Winsock2.select(ASocket + 1, nil, locFDS, nil, locTimeVal); // 0 -> TimeOut
  {$ENDIF}
  if (Res=0) then
    Result:=ctrTimeout
  else if (Res<0) then
    Result:=ctrError
  else if (Res>0) then
    begin
    Result:=ctrError;
    ErrLen := SizeOf(Err);
    {$ifdef unix}
    if fpFD_ISSET(ASocket, locFDS^)=1 then
    {$ENDIF}
    {$ifdef windows}
    if FD_ISSET(ASocket, locFDS^) then
    {$ENDIF}
      begin
      fpGetSockOpt(ASocket, SOL_SOCKET, SO_ERROR, @Err, @ErrLen);
      if Err=0 then // 0 -> connected
        Result:=ctrOK
      end;
    end;
end;
{$ENDIF HAVENONBLOCKING}

procedure TInetSocket.Connect;

{$IFDEF HAVENONBLOCKING}
Const
 {$IFDEF UNIX}
    ErrWouldBlock = ESysEInprogress;
 {$ELSE}
    ErrWouldBlock = WSAEWOULDBLOCK;
 {$ENDIF}
{$ENDIF}

Var
  A : THostAddr;
  addr: TInetSockAddr;
  IsError : Boolean;
  TimeOutResult : TCheckTimeOutResult;
  Err: Integer;
  aErrMsg : String;
{$IFDEF HAVENONBLOCKING}
  FDS: TFDSet;
  TimeV: TTimeVal;
{$endif}
begin
  A := StrToHostAddr(FHost);
  if A.s_bytes[1] = 0 then
    With THostResolver.Create(Nil) do
      try
        If Not NameLookup(FHost) then
          raise ESocketError.Create(seHostNotFound, [FHost]);
        A:=HostAddress;
      finally
        free;
      end;
  addr.sin_family := AF_INET;
  addr.sin_port := ShortHostToNet(FPort);
  addr.sin_addr.s_addr := HostToNet(a.s_addr);
{$IFDEF HAVENONBLOCKING}
  if ConnectTimeOut>0 then
    SetSocketBlockingMode(Handle, bmNonBlocking, @FDS) ;
{$ENDIF}
  IsError:=True;
  TimeOutResult:=ctrError;
  {$ifdef unix}
  Err:=ESysEINTR;
  While IsError and ((Err=ESysEINTR) or (Err=ESysEAGAIN)) do
  {$endif}
    begin
    IsError:=fpConnect(Handle, @addr, sizeof(addr))<>0;
    if IsError then
      Err:=Socketerror;
    end;
{$IFDEF HAVENONBLOCKING}
  if (ConnectTimeOut>0) then
    begin
    if IsError and (Err=ErrWouldBlock) then
      begin
      TimeOutResult:=CheckSocketConnectTimeout(Handle, @FDS, @TimeV);
      IsError:=(TimeOutResult<>ctrOK);
      end;
    SetSocketBlockingMode(Handle, bmBlocking, @FDS);
    end;
{$ENDIF}
  If Not IsError then
    begin
    IsError:=Not FHandler.Connect;
    if IsError then
      CloseSocket(Handle);
    end;
  If IsError then
    if TimeoutResult=ctrTimeout then
      Raise ESocketError.Create(seConnectTimeOut, [Format('%s:%d',[FHost, FPort])])
    else
      begin
      aErrMsg:=FHandler.GetLastErrorDescription;
      Raise ESocketError.Create(seConnectFailed, [Format('%s:%d',[FHost, FPort]),aErrMsg]);
      end;
end;

{ ---------------------------------------------------------------------
    TUnixSocket
  ---------------------------------------------------------------------}
{$ifdef Unix}
Constructor TUnixSocket.Create(ASocket : Longint);

begin
  Inherited Create(ASocket);
end;

Constructor TUnixSocket.Create(const AFileName : String);

Var S : Longint;

begin
  FFileName:=AFileName;
  S:=FpSocket(AF_UNIX,SOCK_STREAM,0);
  DoConnect(S);
  Inherited Create(S);
end;

Procedure TUnixSocket.DoConnect(ASocket : longint);

Var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,UnixAddr,AddrLen);
  If  FpConnect(ASocket,@UnixAddr,AddrLen)<>0 then
    Raise ESocketError.Create(seConnectFailed,[FFilename,'']);
end;
{$endif}
end.
