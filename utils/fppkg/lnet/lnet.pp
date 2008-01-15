{ lNet v0.5.8

  CopyRight (C) 2004-2007 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lNet;

{$mode objfpc}{$H+}{$T-}
{$interfaces corba}

interface

uses
  Classes, lEvents,
  {$i sys/osunits.inc}

const
  { Address constants }
  LADDR_ANY = '0.0.0.0';
  LADDR_BR  = '255.255.255.255';
  LADDR_LO  = '127.0.0.1';
  { ICMP }
  LICMP_ECHOREPLY     = 0;
  LICMP_UNREACH       = 3;
  LICMP_ECHO          = 8;
  LICMP_TIME_EXCEEDED = 11;
  { Protocols }
  LPROTO_IP     =     0;
  LPROTO_ICMP   =     1;
  LPROTO_IGMP   =     2;
  LPROTO_TCP    =     6;
  LPROTO_UDP    =    17;
  LPROTO_IPV6   =    41;
  LPROTO_ICMPV6 =    58;
  LPROTO_RAW    =   255;
  LPROTO_MAX    =   256;

type
  PLIPHeader = ^TLIPHeader;
  TLIPHeader = record
      VerLen      : Byte;
      TOS         : Byte;
      TotalLen    : Word;
      Identifer   : Word;
      FragOffsets : Word;
      TTL         : Byte;
      Protocol    : Byte;
      CheckSum    : Word;
      SourceIp    : DWord;
      DestIp      : DWord;
      Options     : DWord;
  end;  // TLIPHeader


  TLSocket = class;
  TLComponent = class;
  
  { Callback Event procedure for errors }
  TLSocketErrorEvent = procedure(const msg: string; aSocket: TLSocket) of object;

  { Callback Event procedure for others }
  TLSocketEvent = procedure(aSocket: TLSocket) of object;

  { Callback Event procedure for progress reports}
  TLSocketProgressEvent = procedure (aSocket: TLSocket; const Bytes: Integer) of object;

  { Base socket class, Holds Address and socket info, perForms basic
    socket operations, uses select always to figure out if it can work (slow) }

  { TLSocket }

  TLSocket = class(TLHandle)
   protected
    FAddress: TInetSockAddr;
    FPeerAddress: TInetSockAddr;
    FReuseAddress: Boolean;
    FConnected: Boolean;
    FConnecting: Boolean;
    FNextSock: TLSocket;
    FPrevSock: TLSocket;
    FIgnoreShutdown: Boolean;
    FCanSend: Boolean;
    FCanReceive: Boolean;
    FServerSocket: Boolean;
    FOnFree: TLSocketEvent;
    FBlocking: Boolean;
    FListenBacklog: Integer;
    FProtocol: Integer;
    FSocketType: Integer;
    FCreator: TLComponent;
   protected
    function DoSend(const TheData; const TheSize: Integer): Integer;
    
    function SetupSocket(const APort: Word; const Address: string): Boolean; virtual;
    
    function GetLocalPort: Word;
    function GetPeerPort: Word;
    function GetPeerAddress: string;
    function GetLocalAddress: string;
    function CanSend: Boolean; virtual;
    function CanReceive: Boolean; virtual;
    
    procedure SetOptions; virtual;
    procedure SetBlocking(const aValue: Boolean);
    procedure SetReuseAddress(const aValue: Boolean);

    function Bail(const msg: string; const ernum: Integer): Boolean;
    
    procedure LogError(const msg: string; const ernum: Integer); virtual;
   public
    constructor Create; override;
    destructor Destroy; override;
    
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
    function Accept(const SerSock: Integer): Boolean;
    
    function Connect(const Address: string; const APort: Word): Boolean;
    
    function Send(const aData; const aSize: Integer): Integer; virtual;
    function SendMessage(const msg: string): Integer;
    
    function Get(var aData; const aSize: Integer): Integer; virtual;
    function GetMessage(out msg: string): Integer;
    
    procedure Disconnect; virtual;
   public
    property Connected: Boolean read FConnected;
    property Connecting: Boolean read FConnecting;
    property Blocking: Boolean read FBlocking write SetBlocking;
    property ListenBacklog: Integer read FListenBacklog write FListenBacklog;
    property Protocol: Integer read FProtocol write FProtocol;
    property SocketType: Integer read FSocketType write FSocketType;
    property PeerAddress: string read GetPeerAddress;
    property PeerPort: Word read GetPeerPort;
    property LocalAddress: string read GetLocalAddress;
    property LocalPort: Word read GetLocalPort;
    property ReuseAddress: Boolean read FReuseAddress write SetReuseAddress;
    property NextSock: TLSocket read FNextSock write FNextSock;
    property PrevSock: TLSocket read FPrevSock write FPrevSock;
    property Creator: TLComponent read FCreator;
  end;
  TLSocketClass = class of TLSocket;

  { this is the socket used by TLConnection }
  
  TLActionEnum = (acConnect, acAccept, acSend, acReceive, acError);

  { Base interface common to ALL connections }
  
  ILComponent = interface
    procedure Disconnect;
    procedure CallAction;
    
    property SocketClass: TLSocketClass;
    property Host: string;
    property Port: Word;
  end;
  
  { Interface for protools with direct send/get capabilities }

  ILDirect = interface
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer;

    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
  end;
  
  { Interface for all servers }
  
  ILServer = interface
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
  end;

  { Interface for all clients }
  
  ILClient = interface
    function Connect(const Address: string; const APort: Word): Boolean; overload;
    function Connect: Boolean; overload;
  end;
  
  { TLComponent }

  TLComponent = class(TComponent, ILComponent)
   protected
    FHost: string;
    FPort: Word;
    FCreator: TLComponent;
   public
    constructor Create(aOwner: TComponent); override;
    procedure Disconnect; virtual; abstract;
    procedure CallAction; virtual; abstract;
   public
    SocketClass: TLSocketClass;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property Creator: TLComponent read FCreator write FCreator;
  end;
  
  { TLConnection
    Common ancestor for TLBaseTcp and TLUdp classes. Holds Event properties
    and common variables. }

  TLConnection = class(TLComponent, ILDirect, ILServer, ILClient)
   protected
    FTimeVal: TTimeVal;
    FOnReceive: TLSocketEvent;
    FOnAccept: TLSocketEvent;
    FOnConnect: TLSocketEvent;
    FOnDisconnect: TLSocketEvent;
    FOnCanSend: TLSocketEvent;
    FOnError: TLSocketErrorEvent;
    FRootSock: TLSocket;
    FIterator: TLSocket;
    FID: Integer; // internal number for server
    FEventer: TLEventer;
    FEventerClass: TLEventerClass;
    FTimeout: Integer;
    FListenBacklog: Integer;
   protected
    function InitSocket(aSocket: TLSocket): TLSocket; virtual;
    
    function GetConnected: Boolean; virtual; abstract;
    function GetCount: Integer; virtual;
    function GetItem(const i: Integer): TLSocket;
    
    function GetTimeout: Integer;
    procedure SetTimeout(const AValue: Integer);
    
    procedure SetEventer(Value: TLEventer);
    
    procedure ConnectAction(aSocket: TLHandle); virtual;
    procedure AcceptAction(aSocket: TLHandle); virtual;
    procedure ReceiveAction(aSocket: TLHandle); virtual;
    procedure SendAction(aSocket: TLHandle); virtual;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); virtual;
    
    procedure ConnectEvent(aSocket: TLHandle); virtual;
    procedure DisconnectEvent(aSocket: TLHandle); virtual;
    procedure AcceptEvent(aSocket: TLHandle); virtual;
    procedure ReceiveEvent(aSocket: TLHandle); virtual;
    procedure CanSendEvent(aSocket: TLHandle); virtual;
    procedure ErrorEvent(const msg: string; aSocket: TLHandle); virtual;
    procedure EventerError(const msg: string; Sender: TLEventer);
    
    procedure RegisterWithEventer; virtual;
    
    procedure FreeSocks; virtual;
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    
    function Connect(const Address: string; const APort: Word): Boolean; virtual; overload;
    function Connect: Boolean; virtual; overload;
    
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; virtual; abstract; overload;
    function Listen: Boolean; virtual; overload;
    
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    
    function IterNext: Boolean; virtual; abstract;
    procedure IterReset; virtual; abstract;
   public
    property OnError: TLSocketErrorEvent read FOnError write FOnError;
    property OnReceive: TLSocketEvent read FOnReceive write FOnReceive;
    property OnDisconnect: TLSocketEvent read FOnDisconnect write FOnDisconnect;
    property OnCanSend: TLSocketEvent read FOnCanSend write FOnCanSend;
    property Socks[index: Integer]: TLSocket read GetItem; default;
    property Count: Integer read GetCount;
    property Connected: Boolean read GetConnected;
    property ListenBacklog: Integer read FListenBacklog write FListenBacklog;
    property Iterator: TLSocket read FIterator;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property Eventer: TLEventer read FEventer write SetEventer;
    property EventerClass: TLEventerClass read FEventerClass write FEventerClass;
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }

  { TLUdp }

  TLUdp = class(TLConnection)
   protected
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    
    function GetConnected: Boolean; override;
    
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); override;
    
    function Bail(const msg: string): Boolean;
    
    procedure SetAddress(const Address: string);
   public
    constructor Create(aOwner: TComponent); override;
    
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; override;
    
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;
    
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; const Address: string): Integer; overload;
    
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; const Address: string): Integer; overload;
    
    function IterNext: Boolean; override;
    procedure IterReset; override;

    procedure Disconnect; override;

    procedure CallAction; override;
  end;
  
  { TCP Client/Server class. Provided to enable usage of TCP sockets }

  { TLTcp }

  TLTcp = class(TLConnection)
   protected
    FCount: Integer;
    FReuseAddress: Boolean;
    function InitSocket(aSocket: TLSocket): TLSocket; override;

    function GetConnected: Boolean; override;
    function GetConnecting: Boolean;
    function GetCount: Integer; override;

    procedure SetReuseAddress(const aValue: Boolean);

    procedure ConnectAction(aSocket: TLHandle); override;
    procedure AcceptAction(aSocket: TLHandle); override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); override;

    function Bail(const msg: string; aSocket: TLSocket): Boolean;

    procedure SocketDisconnect(aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;

    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; override;

    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;

    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;

    function IterNext: Boolean; override;
    procedure IterReset; override;

    procedure CallAction; override;

    procedure Disconnect; override;
   public
    property Connecting: Boolean read GetConnecting;
    property OnAccept: TLSocketEvent read FOnAccept write FOnAccept;
    property OnConnect: TLSocketEvent read FOnConnect write FOnConnect;
    property ReuseAddress: Boolean read FReuseAddress write SetReuseAddress;
  end;
  
implementation

uses
  lCommon;
  
//********************************TLSocket*************************************

constructor TLSocket.Create;
begin
  inherited Create;
  FHandle := INVALID_SOCKET;
  FBlocking := False;
  FListenBacklog := LDEFAULT_BACKLOG;
  FServerSocket := False;
  FPrevSock := nil;
  FNextSock := nil;
  FCanSend := True;
  FCanReceive := False;
  FConnected := False;
  FConnecting := False;
  FIgnoreShutdown := False;
  FSocketType := SOCK_STREAM;
  FProtocol := LPROTO_TCP;
end;

destructor TLSocket.Destroy;
begin
  if Assigned(FOnFree) then
    FOnFree(Self);

  inherited Destroy; // important! must be called before disconnect
  Disconnect;
end;

procedure TLSocket.Disconnect;
var
  WasConnected: Boolean;
begin
  WasConnected := FConnected;
  FDispose := True;
  FCanSend := True;
  FCanReceive := True;
  FIgnoreWrite := True;
  if FConnected or FConnecting then begin
    FConnected := False;
    FConnecting := False;
    if (FSocketType = SOCK_STREAM) and (not FIgnoreShutdown) and WasConnected then
      if fpShutDown(FHandle, 2) <> 0 then
        LogError('Shutdown error', LSocketError);
        
    if Assigned(FEventer) then
      FEventer.UnregisterHandle(Self);
        
    if CloseSocket(FHandle) <> 0 then
      LogError('Closesocket error', LSocketError);
    FHandle := INVALID_SOCKET;
  end;
end;

procedure TLSocket.LogError(const msg: string; const ernum: Integer);
begin
  if Assigned(FOnError) then
    if ernum > 0 then
      FOnError(Self, msg + LStrError(ernum))
    else
      FOnError(Self, msg);
end;

function TLSocket.Bail(const msg: string; const ernum: Integer): Boolean;
begin
  Result := False; // return the result for the caller

  Disconnect;
  LogError(msg, ernum);
end;

function TLSocket.GetPeerAddress: string;
begin
  Result := '';
  if FSocketType = SOCK_STREAM then
    Result := NetAddrtoStr(FAddress.Addr)
  else
    Result := NetAddrtoStr(FPeerAddress.Addr);
end;

function TLSocket.GetLocalAddress: string;
var
  a: TSockAddr;
  l: Integer;
begin
  Result := '';
  l := SizeOf(a);
  if fpGetSockName(FHandle, @a, @l) = 0 then
    Result := NetAddrToStr(LongWord(a.sin_addr));
end;

function TLSocket.CanSend: Boolean;
begin
  Result := FCanSend and FConnected;
end;

function TLSocket.CanReceive: Boolean;
begin
  Result := FCanReceive and FConnected;
end;

procedure TLSocket.SetOptions;
begin
  SetBlocking(FBlocking);
end;

procedure TLSocket.SetBlocking(const aValue: Boolean);
begin
  FBlocking := aValue;
  if FHandle >= 0 then // we already set our socket
    if not lCommon.SetBlocking(FHandle, aValue) then
      Bail('Error on SetBlocking', LSocketError);
end;

procedure TLSocket.SetReuseAddress(const aValue: Boolean);
begin
  if not FConnected then
    FReuseAddress := aValue;
end;

function TLSocket.GetMessage(out msg: string): Integer;
begin
  Result := 0;
  SetLength(msg, BUFFER_SIZE);
  SetLength(msg, Get(PChar(msg)^, Length(msg)));
  Result := Length(msg);
end;

function TLSocket.Get(var aData; const aSize: Integer): Integer;
var
  AddressLength: Integer = SizeOf(FPeerAddress);
  LastError: Longint;
begin
  Result := 0;
  if CanReceive then begin
    if FSocketType = SOCK_STREAM then
      Result := sockets.fpRecv(FHandle, @aData, aSize, LMSG)
    else
      Result := sockets.fpRecvfrom(FHandle, @aData, aSize, LMSG, @FPeerAddress, @AddressLength);
      
    if Result = 0 then
      if FSocketType = SOCK_STREAM then
        Disconnect
      else
        Bail('Receive Error [0 on recvfrom with UDP]', 0);
      
    if Result = SOCKET_ERROR then begin
      LastError := LSocketError;
      if IsBlockError(LastError) then begin
        FCanReceive  :=  False;
        IgnoreRead  :=  False;
      end else
        Bail('Receive Error', LastError);
      Result := 0;
    end;
  end;
end;

function TLSocket.DoSend(const TheData; const TheSize: Integer): Integer;
var
  AddressLength: Integer;
begin
  AddressLength := SizeOf(FPeerAddress);
  if FSocketType = SOCK_STREAM then
    Result := sockets.fpsend(FHandle, @TheData, TheSize, LMSG)
  else
    Result := sockets.fpsendto(FHandle, @TheData, TheSize, LMSG, @FPeerAddress, AddressLength);
end;

function TLSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
  Arg, Opt: Integer;
begin
  Result := false;
  if not FConnected and not FConnecting then begin
    Done := true;
    FHandle := fpSocket(AF_INET, FSocketType, FProtocol);
    if FHandle = INVALID_SOCKET then
      Exit(Bail('Socket error', LSocketError));
    SetOptions;

    Arg := 1;
    if FSocketType = SOCK_DGRAM then begin
      if fpsetsockopt(FHandle, SOL_SOCKET, SO_BROADCAST, @Arg, Sizeof(Arg)) = SOCKET_ERROR then
        Exit(Bail('SetSockOpt error', LSocketError));
    end else if FReuseAddress then begin
      Opt := SO_REUSEADDR;
      {$ifdef WIN32} // I expect 64 has it oddly, so screw them for now
      if (Win32Platform = 2) and (Win32MajorVersion >= 5) then
        Opt := Integer(not Opt);
      {$endif}
      if fpsetsockopt(FHandle, SOL_SOCKET, Opt, @Arg, Sizeof(Arg)) = SOCKET_ERROR then
        Exit(Bail('SetSockOpt error', LSocketError));
    end;
    
    {$ifdef darwin}
    Arg := 1;
    if fpsetsockopt(FHandle, SOL_SOCKET, SO_NOSIGPIPE, @Arg, Sizeof(Arg)) = SOCKET_ERROR then
      Exit(Bail('SetSockOpt error', LSocketError));
    {$endif}
    
    FillAddressInfo(FAddress, AF_INET, Address, aPort);
    FillAddressInfo(FPeerAddress, AF_INET, LADDR_BR, aPort);

    Result  :=  Done;
  end;
end;

function TLSocket.GetLocalPort: Word;
begin
  Result := ntohs(FAddress.sin_port);
end;

function TLSocket.GetPeerPort: Word;
begin
  Result := ntohs(FPeerAddress.sin_port);
end;

function TLSocket.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  if not Connected then begin
    Result := false;
    SetupSocket(APort, AIntf);
    if fpBind(FHandle, psockaddr(@FAddress), SizeOf(FAddress)) = SOCKET_ERROR then
      Bail('Error on bind', LSocketError)
    else
      Result := true;
    if (FSocketType = SOCK_STREAM) and Result then
      if fpListen(FHandle, FListenBacklog) = SOCKET_ERROR then
        Result := Bail('Error on Listen', LSocketError)
      else
        Result := true;
  end;
end;

function TLSocket.Accept(const sersock: Integer): Boolean;
var
  AddressLength: tsocklen = SizeOf(FAddress);
begin
  Result := false;
  if not Connected then begin
    FHandle := fpAccept(sersock, psockaddr(@FAddress), @AddressLength);
    if FHandle <> INVALID_SOCKET then begin
      SetOptions;
      Result := true;
      FConnected := true;
    end else
      Bail('Error on accept', LSocketError);
  end;
end;

function TLSocket.Connect(const Address: string; const aPort: Word): Boolean;
begin
  Result := False;
  if Connected or FConnecting then
    Disconnect;
  if SetupSocket(APort, Address) then begin
    fpConnect(FHandle, psockaddr(@FAddress), SizeOf(FAddress));
    FConnecting := True;
    Result := FConnecting;
  end;
end;

function TLSocket.SendMessage(const msg: string): Integer;
begin
  Result := Send(PChar(msg)^, Length(msg));
end;

function TLSocket.Send(const aData; const aSize: Integer): Integer;
var
  LastError: Longint;
begin
  Result := 0;
  if not FServerSocket then begin
    if aSize <= 0 then begin
      Bail('Send error: wrong size (Size <= 0)', -1);
      Exit(0);
    end;

    if CanSend then begin
      Result := DoSend(aData, aSize);
      if Result = SOCKET_ERROR then begin
        LastError := LSocketError;
        if IsBlockError(LastError) then begin
          FCanSend := False;
          IgnoreWrite := False;
        end else
          Bail('Send error', LastError);
        Result := 0;
      end;
    end;
 end;
end;

//*******************************TLConnection*********************************

constructor TLConnection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHost := '';
  FPort := 0;
  FListenBacklog := LDEFAULT_BACKLOG;
  FTimeout := 0;
  SocketClass := TLSocket;
  FOnReceive := nil;
  FOnError := nil;
  FOnDisconnect := nil;
  FOnCanSend := nil;
  FOnConnect := nil;
  FOnAccept := nil;
  FTimeVal.tv_sec := 0;
  FTimeVal.tv_usec := 0;
  FIterator := nil;
  FEventer := nil;
  FEventerClass := BestEventerClass;
end;

destructor TLConnection.Destroy;
begin
  FreeSocks;
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  inherited Destroy;
end;

function TLConnection.Connect(const Address: string; const APort: Word
  ): Boolean;
begin
  FHost := Address;
  FPort := aPort;
  Result := False;
end;

function TLConnection.Connect: Boolean;
begin
  Result := Connect(FHost, FPort);
end;

function TLConnection.Listen: Boolean;
begin
  Result := Listen(FPort, FHost);
end;

function TLConnection.InitSocket(aSocket: TLSocket): TLSocket;
begin
  aSocket.OnRead := @ReceiveAction;
  aSocket.OnWrite := @SendAction;
  aSocket.OnError := @ErrorAction;
  aSocket.ListenBacklog := FListenBacklog;
  aSocket.FCreator := FCreator;
  Result := aSocket;
end;

function TLConnection.GetCount: Integer;
begin
  Result := 1;
end;

function TLConnection.GetItem(const i: Integer): TLSocket;
var
  Tmp: TLSocket;
  Jumps: Integer;
begin
  Result := nil;
  Tmp := FRootSock;
  Jumps := 0;
  while Assigned(Tmp.NextSock) and (Jumps < i) do begin
    Tmp := Tmp.NextSock;
    Inc(Jumps);
  end;
  if Jumps = i then
    Result := Tmp;
end;

function TLConnection.GetTimeout: Integer;
begin
  if Assigned(FEventer) then
    Result := FEventer.Timeout
  else
    Result := FTimeout;
end;

procedure TLConnection.ConnectAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.AcceptAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.ReceiveAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.SendAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.ErrorAction(aSocket: TLHandle; const msg: string);
begin
end;

procedure TLConnection.ConnectEvent(aSocket: TLHandle);
begin
  if Assigned(FOnConnect) then
    FOnConnect(TLSocket(aSocket));
end;

procedure TLConnection.DisconnectEvent(aSocket: TLHandle);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(TLSocket(aSocket));
end;

procedure TLConnection.AcceptEvent(aSocket: TLHandle);
begin
  if Assigned(FOnAccept) then
    FOnAccept(TLSocket(aSocket));
end;

procedure TLConnection.ReceiveEvent(aSocket: TLHandle);
begin
  if Assigned(FOnReceive) then
    FOnReceive(TLSocket(aSocket));
end;

procedure TLConnection.CanSendEvent(aSocket: TLHandle);
begin
  if Assigned(FOnCanSend) then
    FOnCanSend(TLSocket(aSocket));
end;

procedure TLConnection.ErrorEvent(const msg: string; aSocket: TLHandle);
begin
  if Assigned(FOnError) then
    FOnError(msg, TLSocket(aSocket));
end;

procedure TLConnection.SetTimeout(const AValue: Integer);
begin
  if Assigned(FEventer) then
    FEventer.Timeout := aValue;
  FTimeout := aValue;
end;

procedure TLConnection.SetEventer(Value: TLEventer);
begin
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  FEventer := Value;
  FEventer.AddRef;
end;

procedure TLConnection.EventerError(const msg: string; Sender: TLEventer);
begin
  ErrorEvent(msg, nil);
end;

procedure TLConnection.RegisterWithEventer;
begin
  if not Assigned(FEventer) then begin
    FEventer := FEventerClass.Create;
    FEventer.OnError := @EventerError;
  end;

  if Assigned(FRootSock) then
    FEventer.AddHandle(FRootSock);

  if (FEventer.Timeout = 0) and (FTimeout <> 0) then
    FEventer.Timeout := FTimeout
  else
    FTimeout := FEventer.Timeout;
end;

procedure TLConnection.FreeSocks;
var
  Tmp, Tmp2: TLSocket;
begin
  Tmp := FRootSock;
  while Assigned(Tmp) do begin
    Tmp2 := Tmp;
    Tmp := Tmp.NextSock;
    Tmp2.Disconnect;
    Tmp2.Free;
  end;
end;

//*******************************TLUdp*********************************

constructor TLUdp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimeVal.tv_usec := 0;
  FTimeVal.tv_sec := 0;
end;

procedure TLUdp.Disconnect;
begin
  if Assigned(FRootSock) then begin
    FRootSock.Disconnect;
    FRootSock := nil; // even if the old one exists, eventer takes care of it
  end;
end;

function TLUdp.Connect(const Address: string; const APort: Word): Boolean;
begin
  Result := inherited Connect(Address, aPort);

  if Assigned(FRootSock) and FRootSock.Connected then
    Disconnect;

  FRootSock := InitSocket(SocketClass.Create);
  FIterator := FRootSock;

  Result := FRootSock.SetupSocket(APort, LADDR_ANY);
  
  if Result then begin
    FillAddressInfo(FRootSock.FPeerAddress, AF_INET, Address, aPort);
    FRootSock.FConnected := true;
    RegisterWithEventer;
  end;
end;

function TLUdp.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result := False;

  if Assigned(FRootSock) and FRootSock.Connected then
    Disconnect;

  FRootSock := InitSocket(SocketClass.Create);
  FIterator := FRootSock;
  
  if FRootSock.Listen(APort, AIntf) then begin
    FillAddressInfo(FRootSock.FPeerAddress, AF_INET, LADDR_BR, aPort);
  
    FRootSock.FConnected := True;
    RegisterWithEventer;
    Result := True;
  end;
end;

function TLUdp.Bail(const msg: string): Boolean;
begin
  Result  :=  False;

  Disconnect;
  ErrorEvent(msg, FRootSock);
end;

procedure TLUdp.SetAddress(const Address: string);
var
  n: Integer;
  s: string;
  p: Word;
begin
  n := Pos(':', Address);
  if n > 0 then begin
    s := Copy(Address, 1, n-1);
    p := Word(StrToInt(Copy(Address, n+1, Length(Address))));

    FillAddressInfo(FRootSock.FPeerAddress, AF_INET, s, p);
  end else
    FillAddressInfo(FRootSock.FPeerAddress, AF_INET, Address,
                                            FRootSock.PeerPort);
end;

function TLUdp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result := FRootSock;
  if not Assigned(FRootSock) then begin
    Result := inherited InitSocket(aSocket);
    aSocket.SocketType := SOCK_DGRAM;
    aSocket.Protocol := LPROTO_UDP;
  end;
end;

procedure TLUdp.ReceiveAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    FCanReceive := True;
    ReceiveEvent(aSocket);
  end;
end;

procedure TLUdp.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    FCanSend := True;
    IgnoreWrite := True;
    CanSendEvent(aSocket);
  end;
end;

procedure TLUdp.ErrorAction(aSocket: TLHandle; const msg: string);
begin
  Bail(msg);
end;

function TLUdp.IterNext: Boolean;
begin
  Result := False;
end;

procedure TLUdp.IterReset;
begin
end;

procedure TLUdp.CallAction;
begin
  if Assigned(FEventer) then
    FEventer.CallAction;
end;

function TLUdp.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(FRootSock) then
  Result := FRootSock.Connected;
end;

function TLUdp.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.Get(aData, aSize);
end;

function TLUdp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.GetMessage(msg);
end;

function TLUdp.SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.SendMessage(msg)
end;

function TLUdp.SendMessage(const msg: string; const Address: string): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then begin
    SetAddress(Address);
    Result := FRootSock.SendMessage(msg)
  end;
end;

function TLUdp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.Send(aData, aSize)
end;

function TLUdp.Send(const aData; const aSize: Integer; const Address: string
  ): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then begin
    SetAddress(Address);
    Result := FRootSock.Send(aData, aSize);
  end;
end;

//******************************TLTcp**********************************

constructor TLTcp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIterator := nil;
  FCount := 0;
  FRootSock := nil;
end;

function TLTcp.Connect(const Address: string; const APort: Word): Boolean;
begin
  Result := inherited Connect(Address, aPort);
  
  if Assigned(FRootSock) then
    Disconnect;
    
  FRootSock := InitSocket(SocketClass.Create);
  Result := FRootSock.Connect(Address, aPort);
  
  if Result then begin
    Inc(FCount);
    FIterator := FRootSock;
    RegisterWithEventer;
  end else begin
    FreeAndNil(FRootSock); // one possible use, since we're not in eventer yet
    FIterator := nil;
  end;
end;

function TLTcp.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result := false;
  
  if Assigned(FRootSock) then
    Disconnect;
  
  FRootSock := InitSocket(SocketClass.Create);
  FRootSock.FIgnoreShutdown := True;
  FRootSock.SetReuseAddress(FReuseAddress);
  if FRootSock.Listen(APort, AIntf) then begin
    FRootSock.FConnected := True;
    FRootSock.FServerSocket := True;
    FIterator := FRootSock;
    Inc(FCount);
    RegisterWithEventer;
    Result := true;
  end;
end;

function TLTcp.Bail(const msg: string; aSocket: TLSocket): Boolean;
begin
  Result  :=  False;
  
  ErrorEvent(msg, aSocket);
  if Assigned(aSocket) then
    aSocket.Disconnect
  else
    Disconnect;
end;

procedure TLTcp.SocketDisconnect(aSocket: TLSocket);
begin
  if aSocket = FIterator then begin
    if Assigned(FIterator.NextSock) then
      FIterator := FIterator.NextSock
    else if Assigned(FIterator.PrevSock) then
      FIterator := FIterator.PrevSock
    else FIterator := nil; // NOT iterreset, not reorganized yet
    if Assigned(FIterator) and FIterator.FServerSocket then
      FIterator := nil;
  end;

  if aSocket = FRootSock then
    FRootSock := aSocket.NextSock;
  if Assigned(aSocket.PrevSock) then
    aSocket.PrevSock.NextSock := aSocket.NextSock;
  if Assigned(aSocket.NextSock) then
    aSocket.NextSock.PrevSock := aSocket.PrevSock;
    
  Dec(FCount);
end;

function TLTcp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result := inherited InitSocket(aSocket);
  aSocket.SocketType := SOCK_STREAM;
  aSocket.Protocol := LPROTO_TCP;
  aSocket.FOnFree := @SocketDisconnect;
end;

function TLTcp.IterNext: Boolean;
begin
  Result := False;
  if Assigned(FIterator.NextSock) then begin
    FIterator := FIterator.NextSock;
    Result := True;
  end else IterReset;
end;

procedure TLTcp.IterReset;
begin
  FIterator := FRootSock;
end;

procedure TLTcp.Disconnect;
begin
  FreeSocks;
  FRootSock := nil;
  FCount := 0;
  FIterator := nil;
end;

procedure TLTcp.CallAction;
begin
  if Assigned(FEventer) then
    FEventer.CallAction;
end;

procedure TLTcp.ConnectAction(aSocket: TLHandle);
var
  a: TInetSockAddr;
  l: Longint;
begin
  with TLSocket(aSocket) do begin
    l := SizeOf(a);
    if Sockets.fpGetPeerName(FHandle, @a, @l) <> 0 then
      Self.Bail('Error on connect: connection refused', TLSocket(aSocket))
    else begin
      FConnected := True;
      FConnecting := False;
      ConnectEvent(aSocket);
    end;
  end;
end;

procedure TLTcp.AcceptAction(aSocket: TLHandle);
var
  Tmp: TLSocket;
begin
  Tmp := InitSocket(SocketClass.Create);
  if Tmp.Accept(FRootSock.FHandle) then begin
    if Assigned(FRootSock.FNextSock) then begin
      Tmp.FNextSock := FRootSock.FNextSock;
      FRootSock.FNextSock.FPrevSock := Tmp;
    end;
    FRootSock.FNextSock := Tmp;
    Tmp.FPrevSock := FRootSock;
    if not Assigned(FIterator)      // if we don't have (bug?) an iterator yet
    or FIterator.FServerSocket then // or if it's the first socket accepted
      FIterator := Tmp;  // assign it as iterator (don't assign later acceptees)
    Inc(FCount);
    FEventer.AddHandle(Tmp);
    AcceptEvent(Tmp);
  end else Tmp.Free;
end;

procedure TLTcp.ReceiveAction(aSocket: TLHandle);
begin
  if (TLSocket(aSocket) = FRootSock) and TLSocket(aSocket).FServerSocket then
    AcceptAction(aSocket)
  else with TLSocket(aSocket) do begin
    if Connected then begin
      FCanReceive := True;
      ReceiveEvent(aSocket);
      if not Connected then begin
        DisconnectEvent(aSocket);
        aSocket.Free;
      end;
    end;
  end;
end;

procedure TLTcp.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    if Connecting then
      ConnectAction(aSocket);
    FCanSend := True;
    IgnoreWrite := True;
    CanSendEvent(aSocket);
  end;
end;

procedure TLTcp.ErrorAction(aSocket: TLHandle; const msg: string);
begin
  with TLSocket(aSocket) do begin
    if Connecting then
      Self.Bail('Error on connect: connection refused' , TLSocket(aSocket))
    else
      Self.Bail(msg, TLSocket(aSocket));
  end;
end;

function TLTcp.GetConnected: Boolean;
var
  Tmp: TLSocket;
begin
  Result := False;
  Tmp := FRootSock;
  while Assigned(Tmp) do begin
    if Tmp.Connected then begin
      Result := True;
      Exit;
    end else Tmp := Tmp.NextSock;
  end;
end;

function TLTcp.GetConnecting: Boolean;
begin
  Result := False;
  if Assigned(FRootSock) then
    Result := FRootSock.Connecting;
end;

function TLTcp.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TLTcp.SetReuseAddress(const aValue: Boolean);
begin
  if not Assigned(FRootSock)
  or not FRootSock.Connected then
    FReuseAddress := aValue;
end;

function TLTcp.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if not Assigned(aSocket) then
    aSocket := FIterator;
  if Assigned(aSocket) then
    Result := aSocket.Get(aData, aSize);
end;

function TLTcp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if not Assigned(aSocket) then
    aSocket := FIterator;
  if Assigned(aSocket) then
    Result := aSocket.GetMessage(msg);
end;

function TLTcp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if not Assigned(aSocket) then
    aSocket := FIterator;
  if Assigned(aSocket) and (aSize > 0) then
    Result := aSocket.Send(aData, aSize);
end;

function TLTcp.SendMessage(const msg: string; aSocket: TLSocket): Integer;
begin
  Result := Send(PChar(msg)^, Length(msg), aSocket);
end;


{ TLComponent }

constructor TLComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCreator := Self;
end;

end.

