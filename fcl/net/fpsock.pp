{
    $Id$

    Socket components    
    Copyright (c) 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit fpSock;

interface

uses SysUtils, Sockets, Classes, fpAsync;

type

  ESocketError = class(Exception)
  end;

  TSocketComponent = class(TComponent)
  private
    FEventLoop: TEventLoop;
  protected
    DataAvailableNotifyHandle: Pointer;
  public
    destructor Destroy; override;
    property EventLoop: TEventLoop read FEventLoop write FEventLoop;
  end;

  TSocketStream = class(THandleStream)
  private
    FOnDisconnect: TNotifyEvent;
    function GetLocalAddress: TSockAddr;
    function GetPeerAddress: TSockAddr;
  public
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;

    property LocalAddress: TSockAddr read GetLocalAddress;
    property PeerAddress: TSockAddr read GetPeerAddress;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

  // TCP/IP components

  TTCPConnection = class(TSocketComponent)
  end;

  TTCPClient = class(TTCPConnection)
  end;

  TCustomTCPServer = class;

  TQueryConnectEvent = procedure(Sender: TCustomTCPServer; Socket: Integer;
    var DoConnect: Boolean) of object;
  TConnectEvent = procedure(Sender: TCustomTCPServer;
    Stream: TSocketStream) of object;

  TCustomTCPServer = class(TTCPConnection)
  private
    FActive: Boolean;
    FPort: Word;
    FOnQueryConnect: TQueryConnectEvent;
    FOnConnect: TConnectEvent;
    procedure SetActive(Value: Boolean);
    procedure ListenerDataAvailable(Sender: TObject);
  protected
    FSocket: Integer;

    function DoQueryConnect(ASocket: Integer): Boolean; virtual;
    procedure DoConnect(AStream: TSocketStream); virtual;

    //!!!: Interface/bindings list?
    property Active: Boolean read FActive write SetActive;
    property Port: Word read FPort write FPort;
    property OnQueryConnect: TQueryConnectEvent read FOnQueryConnect
      write FOnQueryConnect;
    property OnConnect: TConnectEvent read FOnConnect write FOnConnect;
  end;

  TTCPServer = class(TCustomTCPServer)
  public
    property Socket: Integer read FSocket;
  published
    property Active;
    property Port;
    property OnQueryConnect;
    property OnConnect;
  end;


  // UDP/IP components

  TUDPBase = class(TSocketComponent)
  end;

  TUDPClient = class(TUDPBase)
  end;

  TUDPServer = class(TUDPBase)
  end;


implementation

uses Resolve;

resourcestring
  SSocketCreationError = 'Could not create socket';
  SSocketBindingError = 'Could not bind socket to port %d';
  SSocketAcceptError = 'Connection accept failed';

destructor TSocketComponent.Destroy;
begin
  if Assigned(DataAvailableNotifyHandle) then
  begin
    EventLoop.ClearDataAvailableNotify(DataAvailableNotifyHandle);
    // Set to nil to be sure that descendant classes don't do something stupid
    DataAvailableNotifyHandle := nil;
  end;
  inherited Destroy;
end;


destructor TSocketStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

function TSocketStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := recv(Handle, Buffer, Count, MSG_NOSIGNAL);
  if Result = -1 then
  begin
    Result := 0;
    if Assigned(OnDisconnect) then
      OnDisconnect(Self);
  end;
end;

function TSocketStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := send(Handle, Buffer, Count, MSG_NOSIGNAL);
  if Result = -1 then
  begin
    Result := 0;
    if Assigned(OnDisconnect) then
      OnDisconnect(Self);
  end;
end;

function TSocketStream.GetLocalAddress: TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(TSockAddr);
  if GetSocketName(Handle, Result, len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

function TSocketStream.GetPeerAddress: TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(TSockAddr);
  if GetPeerName(Handle, Result, len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;


function TCustomTCPServer.DoQueryConnect(ASocket: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnQueryConnect) then
    OnQueryConnect(Self, ASocket, Result);
end;

procedure TCustomTCPServer.DoConnect(AStream: TSocketStream);
begin
  if Assigned(OnConnect) then
    OnConnect(Self, AStream);
end;

procedure TCustomTCPServer.SetActive(Value: Boolean);
var
  Addr: TInetSockAddr;
begin
  if Active <> Value then
  begin
    FActive := False;
    if Value then
    begin
      FSocket := Sockets.Socket(AF_INET, SOCK_STREAM, 0);
      if FSocket = -1 then
        raise ESocketError.Create(SSocketCreationError);
      Addr.Family := AF_INET;
      Addr.Port := ShortHostToNet(Port);
      Addr.Addr := 0;
      if not Bind(FSocket, Addr, SizeOf(Addr)) then
        raise ESocketError.CreateFmt(SSocketBindingError, [Port]);
      Listen(FSocket, 5);
      DataAvailableNotifyHandle := EventLoop.SetDataAvailableNotify(FSocket,
        @ListenerDataAvailable, nil);
    end else
    begin
      FileClose(FSocket);
      EventLoop.ClearDataAvailableNotify(DataAvailableNotifyHandle);
      DataAvailableNotifyHandle := nil;
    end;
  end;
end;

procedure TCustomTCPServer.ListenerDataAvailable(Sender: TObject);
var
  ClientSocket: Integer;
  Addr: TInetSockAddr;
  AddrSize: Integer;
begin
  AddrSize := SizeOf(Addr);
  ClientSocket := Accept(FSocket, Addr, AddrSize);
  if ClientSocket = -1 then
    raise ESocketError.Create(SSocketAcceptError);

  if DoQueryConnect(ClientSocket) then
    DoConnect(TSocketStream.Create(ClientSocket));
end;

end.


{
  $Log$
  Revision 1.1  2003-11-22 11:55:28  sg
  * First version, a simple starting point for further development

}
