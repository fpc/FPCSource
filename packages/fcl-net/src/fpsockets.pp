{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Frederic Kehrein

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpsockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$TypedAddress on}
{$modeswitch advancedrecords}

interface

// If a platform is fully operational, add it here
{$IF DEFINED(WINDOWS) or DEFINED(UNIX) or DEFINED(HASAMIGA) }
{$DEFINE FULL_IP_STACK}
{$DEFINE HAVE_SELECT_CALL}
{$ENDIF}

{$IfDef HASAMIGA}
{$inline off}  // inlining on Amiga currently does not work, results in linker error on Default() in NetAddr();
{$EndIf}

{$IFDEF FPC_DOTTEDUNITS}
uses
  {$IfDef WINDOWS}WinApi.WinSock2, {$ENDIF}
  {$ifdef unix} UnixApi.Base, UnixApi.TermIO, {$EndIf}
  {$IfDef HASAMIGA}system.ctypes, {$EndIf}
  System.SysUtils, System.Net.Sockets, System.Nullable, System.Tuples;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$IfDEF WINDOWS}WinSock2, {$ENDIF}
  {$IFDEF unix}BaseUnix, termio, {$EndIf}  
  {$IfDef HASAMIGA}ctypes, {$EndIf}
  sysutils, sockets, nullable, tuples;
{$ENDIF FPC_DOTTEDUNITS}

type

  { Basic Socket Types }

  TFPSocketType = (stIPv4, stIPv6, stIPDualStack, stUnixSocket);
  TFPSocketProtocol = (spStream, spDatagram);
  TFPSocketProto = TFPSocketProtocol;

  { TFPSocket }

  TFPSocket = record
    FD: TSocket;
    Protocol: TFPSocketProto;
    SocketType: TFPSocketType;
    Constructor create(aFD : TSocket; aProtocol : TFPSocketProtocol; aType : TFPSocketType);
  end;

  TAddressType = (atIN4, atIN6, atUnixSock);
  TNetworkAddress = record
    Address: String;
    AddressType: TAddressType;
  end;

  TFPSocketConnection = record
    ClientAddress: TNetworkAddress;
    ClientPort: Word;
    Socket: TFPSocket;
  end;

  { ReceiveFrom Return Types }

  TReceiveFromResult = record
    FromAddr: TNetworkAddress;
    FromPort: Word;
    DataSize: SizeInt;
  end;

  generic TReceiveFromMessage<T> = record
    FromAddr: TNetworkAddress;
    FromPort: Word;
    Data: T;
  end;

  TReceiveFromStringMessage = specialize TReceiveFromMessage<String>;

  { State Management }

  TConnectionState = (csError, csNotConnected, csRefused, csPending, csConnected);

  { Exceptions }

  EDualStackNotSupported = class(Exception);
  EUnsupportedAddress = class(Exception);

  { ESocketCodeError }

  ESocketCodeError = class(Exception)
  private
    FCode: Integer;
  public
    constructor Create(ACode: Integer; const FunName: String);

    property Code: Integer read FCode;
  end;

  EConnectionClosedException = class(Exception);

  { EFragmentedData }

  EFragmentedData = class(Exception)
  private
    FFragment: TBytes;
    FExpectedSize: SizeInt;
  public
    constructor Create(const AFragment: TBytes; AExpected: SizeInt; const AMessage: String);

    property Fragment: TBytes read FFragment;
    property ExpectedSize: SizeInt read FExpectedSize;
  end;

const
  MaxUDPPackageSize = 512;

  { Address Management }

function isIPv4Address(const Address: String): Boolean; inline;
function isIPv6Address(const Address: String): Boolean; inline;

function IN4Address(const Address: String): TNetworkAddress; inline;
function IN6Address(const Address: String): TNetworkAddress; inline;
function IN4MappedIN6Address(const In4Address: String): TNetworkAddress; inline;
function UnixAddr(const Address: String): TNetworkAddress; inline;
function NetAddr(const Address: String): TNetworkAddress; inline;

function isINAddr(const AAddr: TNetworkAddress): Boolean; inline;
function IsIPv4Mapped(const IPv6Addr: TNetworkAddress): Boolean; inline;
function ExtractIPv4Address(const IPv6Addr: TNetworkAddress): TNetworkAddress; inline;

function IN6Equal(const A, B: String): Boolean;
operator =(const A, B: TNetworkAddress): Boolean; inline;
operator <>(const A, B: TNetworkAddress): Boolean; inline;
operator :=(const AStr: String): TNetworkAddress; inline;
operator :=(const AAddr: TNetworkAddress): String; inline;
operator =(const AStr: String; const AAddr: TNetworkAddress): Boolean; inline;
operator =(const AAddr: TNetworkAddress; const AStr: String): Boolean; inline;

  { Socket Functions }

function TCPSocket(AType: TFPSocketType): TFPSocket; inline;
function UDPSocket(AType: TFPSocketType): TFPSocket; inline;

procedure CloseSocket(const ASocket: TFPSocket); inline;

procedure Bind(const ASocket: TFPSocket; const AAddress: TNetworkAddress; APort: Word; ReuseAddr: Boolean = True);

procedure Listen(const ASocket: TFPSocket; Backlog: Integer); inline;
function AcceptConnection(const ASocket: TFPSocket): TFPSocketConnection; inline;
function AcceptNonBlocking(const ASocket: TFPSocket): specialize TNullable<TFPSocketConnection>; inline;

function Connect(const ASocket: TFPSocket; const AAddress: TNetworkAddress; APort: Word): TConnectionState; inline;

function Receive(const ASocket: TFPSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): SizeInt; inline;
function ReceiveFrom(const ASocket: TFPSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): TReceiveFromResult;
function ReceiveFromNonBlocking(const ASocket: TFPSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): specialize TNullable<TReceiveFromResult>; inline;
function Send(const ASocket: TFPSocket; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt; inline;
function SendTo(const ASocket: TFPSocket; const ReceiverAddr: TNetworkAddress;
                  ReceiverPort: Word; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt; inline;

function ReceiveStr(const ASocket: TFPSocket; MaxLength: SizeInt = -1; AFlags: Integer = 0): String;
function ReceiveStrFrom(const ASocket: TFPSocket; MaxLength: SizeInt = MaxUDPPackageSize; AFlags: Integer = 0): TReceiveFromStringMessage; inline;
function ReceiveStrFromNonBlocking(const ASocket: TFPSocket; MaxLength: SizeInt = MaxUDPPackageSize; AFlags: Integer = 0): specialize TNullable<TReceiveFromStringMessage>; inline;
function SendStr(const ASocket: TFPSocket; const AData: String; AFlags: Integer = 0): SizeInt; inline;
function SendStrTo(const ASocket: TFPSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: String; AFlags: Integer = 0): SizeInt; inline;

generic function Receive<T>(const ASocket: TFPSocket; AFlags: Integer = 0): T;
generic function ReceiveNonBlocking<T>(const ASocket: TFPSocket; AFlags: Integer = 0): specialize TNullable<T>;
generic function ReceiveFrom<T>(const ASocket: TFPSocket; AFlags: Integer = 0): specialize TReceiveFromMessage<T>;
generic function ReceiveFromNonBlocking<T>(const ASocket: TFPSocket; AFlags: Integer = 0): specialize TNullable<specialize TReceiveFromMessage<T>>;
generic function Send<T>(const ASocket: TFPSocket; constref AData: T; AFlags: Integer = 0): SizeInt; inline;
generic function SendTo<T>(const ASocket: TFPSocket; constref ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: T; AFlags: Integer = 0): SizeInt; inline;

generic function ReceiveArray<T>(const ASocket: TFPSocket; MaxCount: SizeInt = -1; AFlags: Integer = 0): specialize TArray<T>;
generic function ReceiveArrayFrom<T>(const ASocket: TFPSocket; MaxCount: SizeInt = -1; AFlags: Integer = 0): specialize TReceiveFromMessage<specialize TArray<T>>; inline;
generic function ReceiveArrayFromNonBlocking<T>(const ASocket: TFPSocket; MaxCount: SizeInt = -1; AFlags: Integer = 0): specialize TNullable<specialize TReceiveFromMessage<specialize TArray<T>>>; inline;
generic function SendArray<T>(const ASocket: TFPSocket; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt; inline;
generic function SendArrayTo<T>(const ASocket: TFPSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt; inline;

  { Socket/Connection State Management }

procedure SetNonBlocking(const ASocket: TFPSocket; AValue: Boolean);

function LocalEndpoint(const ASocket: TFPSocket): specialize TPair<TNetworkAddress, Word>;
function RemoteEndpoint(const ASocket: TFPSocket): specialize TPair<TNetworkAddress, Word>;

// Timeout in MS
{$IFDEF HAVE_SELECT_CALL}
function DataAvailable(const SocketArray: specialize TArray<TFPSocket>; TimeOut: Integer = 0): specialize TArray<TFPSocket>; overload;
function DataAvailable(const ASocket: TFPSocket; TimeOut: Integer = 0): Boolean; overload; //inline;
function DataAvailable(const SocketArray: array of TFPSocket; TimeOut: Integer = 0): specialize TArray<TFPSocket>; overload; inline;
{$ENDIF}

function BytesAvailable(const ASocket: TFPSocket): SizeInt;

function StreamClosed(const ASocket: TFPSocket): Boolean; inline;

// For non blocking connections, connect will return a pending connection that needs to be checked
// Note: csConnected means that connection was establised at least once
// If it has been closed by the other side, it is still csConnected, use StreamClosed to figure out
// if the stream is actually open
function ConnectionState(const ASocket: TFPSocket): TConnectionState;


{ Helper }

type
  PAddressUnion = ^TAddressUnion;
  TAddressUnion = record
  case TFPSocketType of
    stIPv4: (In4Addr: sockaddr_in);
    stIPv6: (In6Addr: sockaddr_in6);
    stUnixSocket: (UnixAddr: sockaddr_un);
  end;

function SocketInvalid(ASocket: TSocket): Boolean; inline;

function CreateAddr(AAddress: TNetworkAddress; APort: Word; DualStack: Boolean): TAddressUnion;
procedure ReadAddr(constref Addr: TAddressUnion; DualStack: Boolean; out
  AAddress: TNetworkAddress; out APort: Word);
function CreateRawSocket(ADomain: TFPSocketType; ASockProto: TFPSocketProto; AProto: Integer; RaiseSocketException: Boolean=True): TSocket;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Math;
{$ELSE FPC_DOTTEDUNITS}
uses
  math;
{$ENDIF FPC_DOTTEDUNITS}

{$macro on}
{$IFDEF FPC_DOTTEDUNITS}
{$define socketsunit:=System.Net.Sockets}
{$ELSE FPC_DOTTEDUNITS}
{$define socketsunit:=sockets}
{$ENDIF FPC_DOTTEDUNITS}

{ Helper }

const
  {$IFDEF WINDOWS}
  IPPROTO_IPV6 = 41;
  IPV6_V6ONLY = 27;
  {$ENDIF}
  {$IFDEF UNIX}
  IPPROTO_IPV6 = 41;
  IPV6_V6ONLY = 26;
  {$ENDIF}
  {$IFDEF HASAMIGA}
  IPPROTO_IPV6 = 41;
  IPV6_V6ONLY = 26;
  {$ENDIF}

  {$IFNDEF FULL_IP_STACK}
  IPPROTO_IPV6 = -1;
  IPV6_V6ONLY = -1;
  {$ENDIF}

function WouldBlock(SockErr: Integer): Boolean; inline;
begin
  Result := (SockErr = EsockEWOULDBLOCK)
            {$IfDef Unix} or (SockErr = ESysEAGAIN) {$EndIf}
end;

function CreateAddr(AAddress:TNetworkAddress;APort:Word;DualStack:Boolean)
  :TAddressUnion;
begin
  if (AAddress.AddressType = atIN4) and DualStack then
    AAddress := IN4MappedIN6Address(AAddress.Address);
  if AAddress.AddressType = atIN4 then
  begin
    Result.In4Addr.sin_family := AF_INET;
    Result.In4Addr.sin_port := HToNS(APort);
    Result.In4Addr.sin_addr.s_addr := LongWord(StrToNetAddr(AAddress.Address));
  end
  else if AAddress.AddressType = atIN6 then
  begin
    Result.In6Addr.sin6_family := AF_INET6;
    Result.In6Addr.sin6_port := HToNS(APort);
    Result.In6Addr.sin6_addr := StrToHostAddr6(AAddress.Address);
    Result.In6Addr.sin6_flowinfo := 0;
    Result.In6Addr.sin6_scope_id := 0;
  end
  else if AAddress.AddressType = atUnixSock then
  begin
    if Length(AAddress.Address) > SizeOf(Result.UnixAddr.sun_path)-1 then
      raise EUnsupportedAddress.Create('Unix address should be at most 108 characters');
    Result.UnixAddr.sun_family := AF_UNIX;
    FillChar(Result.UnixAddr, SizeOf(Result.UnixAddr), #00);
    Move(AAddress.Address[1], Result.UnixAddr.sun_path, Length(AAddress.Address));
  end
  else
    raise EUnsupportedAddress.Create('Address type ' + ord(AAddress.AddressType).ToString + ' not supported');
end;

procedure ReadAddr(constref Addr: TAddressUnion; DualStack: Boolean; out
  AAddress: TNetworkAddress; out APort: Word);
var
  i:Integer;
begin
  if Addr.In4Addr.sin_family = AF_INET then
  begin
    AAddress := IN4Address(NetAddrToStr(Addr.In4Addr.sin_addr));
    APort := NToHs(Addr.In4Addr.sin_port);
  end
  else if Addr.In6Addr.sin6_family = AF_INET6 then
  begin
    AAddress := IN6Address(HostAddrToStr6(Addr.In6Addr.sin6_addr));
    if DualStack and IsIPv4Mapped(AAddress.Address) then
      AAddress := ExtractIPv4Address(AAddress);
    APort := NToHs(Addr.In6Addr.sin6_port);
  end
  else if Addr.In6Addr.sin6_family = AF_INET6 then
  begin
    AAddress.AddressType := atUnixSock;
    SetLength(AAddress.Address, SizeOf(Addr.UnixAddr.sun_path));
    i:=0;
    while i < Length(Addr.UnixAddr.sun_path) do
      if Addr.UnixAddr.sun_path[i+low(Addr.UnixAddr.sun_path)] = #00 then
        break
      else
        AAddress.Address[i+1] := Addr.UnixAddr.sun_path[i+low(Addr.UnixAddr.sun_path)];
    SetLength(AAddress.Address, i);
    APort := 0;
  end
  else
    raise EUnsupportedAddress.Create('Address Family ' + Addr.In4Addr.sin_family.ToString + ' not supported');
end;

function SocketInvalid(ASocket: TSocket): Boolean; inline;
begin
  {$IfDef Windows}
  Result := ASocket = TSocket(INVALID_SOCKET);
  {$Else}
  Result := ASocket < 0;
  {$EndIf}
end;

function CreateRawSocket(ADomain: TFPSocketType; ASockProto: TFPSocketProto; AProto: Integer; RaiseSocketException: Boolean): TSocket;
var
  AFam, AType, v6Only: Integer;
  IPV6 : Boolean;
begin
  case ADomain of
  stIPv4: AFam := AF_INET;
  stIPv6,
  stIPDualStack: AFam := AF_INET6;
  stUnixSocket: AFam := AF_UNIX;
  end;
  case ASockProto of
  spStream: AType := SOCK_STREAM;
  spDatagram: AType := SOCK_DGRAM;
  end;
  Result := fpsocket(AFam, AType, AProto);
  if SocketInvalid(Result) then
    if RaiseSocketException then
      raise ESocketCodeError.Create(socketerror, 'socket')
    else
      exit;

  if ADomain = stIPDualStack then
  begin
    v6Only := 0;
    if IPV6_V6ONLY=-1 then
      raise EDualStackNotSupported.Create('Dualstack option not supported on this system.');
    if fpsetsockopt(Result, IPPROTO_IPV6, IPV6_V6ONLY, @v6Only, SizeOf(v6Only)) <> 0 then
      begin
        socketsunit.CloseSocket(Result);
      raise EDualStackNotSupported.Create('Dualstack option not supported on this system: ' + socketerror.ToString);
      end;
  end;
end;

function isIPv4Address(const Address:String):Boolean;
var
  dummy:socketsunit.in_addr;
begin
  Result := TryStrToHostAddr(Address, dummy);
end;

function isIPv6Address(const Address:String):Boolean;
var
  dummy:in6_addr;
begin
  Result := TryStrToHostAddr6(Address, dummy);
end;

function IN4Address(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atIN4;
end;

function IN6Address(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atIN6;
end;

function IN4MappedIN6Address(const In4Address: String): TNetworkAddress;
var
  InAddr: TIn_addr;
begin
  InAddr := StrToNetAddr(In4Address);
  Result := IN6Address('::FFFF:%x:%x'.Format([(InAddr.s_bytes[1] shl 8) or InAddr.s_bytes[2],
                                              (InAddr.s_bytes[3] shl 8) or InAddr.s_bytes[4]]));
end;

function UnixAddr(const Address: String):TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atUnixSock;
end;

function NetAddr(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
  if isIPv4Address(Address) then
    Result.AddressType := atIN4
  else if isIPv6Address(Address) then
    Result.AddressType := atIN6
  else // Filenames can be pretty much anything
    Result.AddressType := atUnixSock;
  Result.Address := Address;
end;

function IsIPv4Mapped(const IPv6Addr: TNetworkAddress): Boolean;
var
  In6Addr: socketsunit.TIn6Addr;
begin
  if IPv6Addr.AddressType = atIN4 then
    Exit(True);
  if IPv6Addr.AddressType  <> atIN6 then
    raise EUnsupportedAddress.Create('Can only check IPv4 mapping for IPv6 addresses');
  IN6Addr := StrToHostAddr6(IPv6Addr.Address);
  Result := (IN6Addr.u6_addr16[0] = 0) and
            (IN6Addr.u6_addr16[1] = 0) and
            (IN6Addr.u6_addr16[2] = 0) and
            (IN6Addr.u6_addr16[3] = 0) and
            (IN6Addr.u6_addr16[4] = 0) and
            (IN6Addr.u6_addr16[5] = $FFFF);
end;

function isINAddr(const AAddr:TNetworkAddress):Boolean;
begin
  Result := AAddr.AddressType in [atIN4, atIN6];
end;

function ExtractIPv4Address(const IPv6Addr: TNetworkAddress): TNetworkAddress;
var
  In6Addr: socketsunit.TIn6Addr;
begin
  if IPv6Addr.AddressType = atIN4 then
    Exit(IPv6Addr);
  if IPv6Addr.AddressType  <> atIN6 then
    raise EUnsupportedAddress.Create('Can only extract IPv4 mapping from IPv6 addresses');
  IN6Addr := StrToHostAddr6(IPv6Addr.Address);
  Result := IN4Address('%d.%d.%d.%d'.Format([IN6Addr.s6_addr8[12],
                                             IN6Addr.s6_addr8[13],
                                             IN6Addr.s6_addr8[14],
                                             IN6Addr.s6_addr8[15]]));
end;

function IN6Equal(const A, B: String): Boolean;
var
  AAddr, BAddr: socketsunit.Tin6_addr;
begin
  AAddr := StrToHostAddr6(A);
  BAddr := StrToHostAddr6(B);
  Result := (AAddr.s6_addr32[0] = BAddr.s6_addr32[0]) and
            (AAddr.s6_addr32[1] = BAddr.s6_addr32[1]) and
            (AAddr.s6_addr32[2] = BAddr.s6_addr32[2]) and
            (AAddr.s6_addr32[3] = BAddr.s6_addr32[3]);
end;

operator=(const A, B: TNetworkAddress): Boolean;
begin
  Result := (A.AddressType = B.AddressType) and (
              ((A.AddressType = atIN4) and (A.Address = B.Address)) or // IPv4: simple string equality
              ((A.AddressType = atIN6) and IN6Equal(A.Address, B.Address)) or // IPv6 check binary equality
              ((A.AddressType = atUnixSock) and SameFileName(A.Address, B.Address)) // UnixSock check if filename equals
            );
end;

operator<>(const A, B: TNetworkAddress): Boolean;
begin
  Result := (A.AddressType <> B.AddressType) or not (
              ((A.AddressType = atIN4) and (A.Address = B.Address)) or // IPv4: simple string equality
              ((A.AddressType = atIN6) and IN6Equal(A.Address, B.Address)) or // IPv6 check binary equality
              ((A.AddressType = atUnixSock) and SameFileName(A.Address, B.Address)) // UnixSock check if filename equals
            );
end;

operator:=(const AStr: String): TNetworkAddress;
begin
  Result := NetAddr(AStr);
end;

function TCPSocket(AType: TFPSocketType): TFPSocket;
begin
  Result.SocketType := AType;
  Result.Protocol := spStream;
  Result.FD := CreateRawSocket(Result.SocketType, Result.Protocol, 0);
end;

function UDPSocket(AType: TFPSocketType): TFPSocket;
begin
  Result.SocketType := AType;
  Result.Protocol := spDatagram;
  Result.FD := CreateRawSocket(Result.SocketType, Result.Protocol, 0);
end;

procedure CloseSocket(const ASocket: TFPSocket);
begin
  socketsunit.CloseSocket(ASocket.FD);
end;

procedure Bind(const ASocket: TFPSocket; const AAddress: TNetworkAddress;
  APort: Word; ReuseAddr: Boolean);
var
  enableReuse: Integer = 1;
  addr: TAddressUnion;
begin
  if ReuseAddr then
    fpsetsockopt(ASocket.FD, SOL_SOCKET, SO_REUSEADDR, @enableReuse, SizeOf(enableReuse));
  addr := CreateAddr(AAddress, APort, ASocket.SocketType = stIPDualStack);
  if fpbind(ASocket.FD, socketsunit.PSockAddr(@addr), SizeOf(addr)) <> 0 then raise
    ESocketCodeError.Create(socketerror, 'bind (%s:%d)'.Format([AAddress.Address, APort]));
end;

procedure Listen(const ASocket: TFPSocket; Backlog: Integer);
begin
  if fplisten(ASocket.FD, Backlog) <> 0 then raise
    ESocketCodeError.Create(socketerror, 'listen');
end;

function AcceptConnection(const ASocket: TFPSocket): TFPSocketConnection;
var
  addr: TAddressUnion;
  addrLen: TSocklen = SizeOf(addr);
begin
  Result.Socket.FD := fpaccept(ASocket.FD, socketsunit.psockaddr(@addr), @addrLen);
  if SocketInvalid(Result.Socket.FD) then
    raise ESocketCodeError.Create(socketerror, 'accept');
  Result.Socket.SocketType := ASocket.SocketType;
  Result.Socket.Protocol := ASocket.Protocol;
  ReadAddr(addr, ASocket.SocketType = stIPDualStack, Result.ClientAddress, Result.ClientPort);
end;

function AcceptNonBlocking(const ASocket: TFPSocket): specialize TNullable<
  TFPSocketConnection>;
var
  addr: TAddressUnion;
  addrLen: TSocklen = SizeOf(addr);
begin
  Result.Ptr^.Socket.FD := fpaccept(ASocket.FD, socketsunit.psockaddr(@addr), @addrLen);
  if SocketInvalid(Result.Ptr^.Socket.FD) then
    if WouldBlock(socketerror) then
      Exit(null)
    else
      raise ESocketCodeError.Create(socketerror, 'accept');
  Result.Ptr^.Socket.SocketType := ASocket.SocketType;
  Result.Ptr^.Socket.Protocol := ASocket.Protocol;
  ReadAddr(addr, ASocket.SocketType = stIPDualStack, Result.Ptr^.ClientAddress, Result.Ptr^.ClientPort);
end;

function Connect(const ASocket: TFPSocket; const AAddress: TNetworkAddress;
  APort: Word): TConnectionState;
var
  addr: TAddressUnion;
  addrlen : cint;
  addrptr : socketsunit.psockaddr;

const
  {$IFDEF WINDOWS} 
  EALREADY = WSAEALREADY;
  EINPROGRESS = WSAEINPROGRESS;
  ECONNREFUSED = WSAECONNREFUSED;
  {$ENDIF}
  
  {$IFDEF UNIX}
  EALREADY = ESysEALREADY;
  EINPROGRESS = ESysEINPROGRESS;
  ECONNREFUSED = ESysECONNREFUSED;
  {$ENDIF}
  {$IFDEF HASAMIGA}
  EALREADY = ESockEALREADY;
  EINPROGRESS = ESockEINPROGRESS;
  ECONNREFUSED = ESockECONNREFUSED;
  {$ENDIF}

  {$IFNDEF FULL_IP_STACK}
  // Fallback
  EALREADY     = -2;
  EINPROGRESS  = -3;
  ECONNREFUSED = -4;
  {$ENDIF}
begin
  addr := CreateAddr(AAddress, APort, ASocket.SocketType = stIPDualStack);
  case ASocket.SocketType of
    stIPv4,
    stIPDualStack:
      begin
      addrlen:=sizeof(sockaddr_in);
      addrptr:=psockaddr(@addr.In4Addr);
      end;
    stIPv6 :
      begin
      addrlen:=sizeof(sockaddr_in6);
      addrptr:=psockaddr(@addr.In6Addr);
      end;
    stUnixSocket :
      begin
      addrlen:=sizeof(sockaddr_un);
      addrptr:=psockaddr(@addr.UnixAddr);
      end;
  end;
  if fpconnect(ASocket.FD, addrptr, addrlen) <> 0 then
    case socketerror of
    EALREADY,
    EINPROGRESS,
    EsockEWOULDBLOCK:
      Exit(csPending);
    ECONNREFUSED:
      Exit(csRefused);
    else
      raise ESocketCodeError.Create(socketerror, 'connect');
    end;
  if ASocket.Protocol<>spStream then
    Result := csNotConnected
  else
    Result := csConnected;
end;

function Receive(const ASocket: TFPSocket; ABuffer: Pointer; MaxSize: SizeInt;
  AFlags: Integer): SizeInt;
begin
  Result := fprecv(ASocket.FD, ABuffer, MaxSize, AFlags);
  if Result = 0 then
    raise EConnectionClosedException.Create('The connection closed')
  else if Result < 0 then
    if WouldBlock(socketerror) then
      Result := 0
    else
      raise ESocketCodeError.Create(socketerror, 'recv');
end;

function ReceiveFrom(const ASocket: TFPSocket; ABuffer: Pointer; MaxSize: SizeInt;
  AFlags: Integer): TReceiveFromResult;
var
  addr: TAddressUnion;
  addrLen: TSocklen;
begin
  Result := Default(TReceiveFromResult);
  addrLen := SizeOf(TAddressUnion);
  Result.DataSize := fprecvfrom(ASocket.FD, ABuffer, MaxSize, AFlags, socketsunit.PSockAddr(@addr), @addrLen);
  if Result.DataSize < 0 then
    if WouldBlock(socketerror) then
      Exit(Default(TReceiveFromResult)) // Will set the DataSize of return to 0
    else
      raise ESocketCodeError.Create(socketerror, 'recvfrom');
  ReadAddr(addr, ASocket.SocketType = stIPDualStack, Result.FromAddr, Result.FromPort);
end;

function ReceiveFromNonBlocking(const ASocket:TFPSocket;ABuffer:Pointer;MaxSize:
  SizeInt;AFlags:Integer):specialize TNullable<TReceiveFromResult>;
begin
  Result := ReceiveFromNonBlocking(ASocket, ABuffer, MaxSize, AFlags);
  if Result.Value.DataSize = 0 then
    Result := null;
end;

function Send(const ASocket: TFPSocket; ABuffer: Pointer; ASize: SizeInt;
  AFlags: Integer): SizeInt;
begin
  Result := fpsend(ASocket.FD, ABuffer, ASize, AFlags);
  if Result < 0 then
    if WouldBlock(socketerror) then
      Result := 0
    else
      raise ESocketCodeError.Create(socketerror, 'send');
end;

function SendTo(const ASocket: TFPSocket; const ReceiverAddr: TNetworkAddress;
  ReceiverPort: Word; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer
  ): SizeInt;
var
  addr: TAddressUnion;
begin
  addr := CreateAddr(ReceiverAddr, ReceiverPort, ASocket.SocketType = stIPDualStack);
  Result := fpsendto(ASocket.FD, ABuffer, ASize, AFlags, socketsunit.psockaddr(@addr), SizeOf(addr));
  if Result < 0 then
    if WouldBlock(socketerror) then
      Result := 0
    else
      raise ESocketCodeError.Create(socketerror, 'sendto');
end;

function ReceiveStr(const ASocket: TFPSocket; MaxLength: SizeInt;
  AFlags: Integer): String;
const
  ReadSize = 1024;
var
  Len, ReadLen: SizeInt;
begin
  Result := '';
  if (MaxLength < 0) and (ASocket.Protocol = spDatagram) then
    MaxLength := MaxUDPPackageSize;
  // If maxlength read as much
  if MaxLength > 0 then
  begin
    SetLength(Result, MaxLength);
    Len := Receive(ASocket, @Result[1], MaxLength, AFlags);
    SetLength(Result, Len);
    Exit;
  end;
  // If no maxlength do a blocking read (required to figure if stream was closed)
  Len := 0;
  MaxLength := BytesAvailable(ASocket);
  if MaxLength = 0 then
    MaxLength := ReadSize;
  repeat
    SetLength(Result, Len + MaxLength);
    ReadLen := Receive(ASocket, @Result[1+Len], MaxLength, AFlags);
    if ReadLen = 0 then // non blocking
      break;
    Len += ReadLen;
    // Check if more was received while reading
    MaxLength:=BytesAvailable(ASocket);
  until (Len < Length(Result)) or (MaxLength <= 0);
  SetLength(Result, Len);
end;

function ReceiveStrFrom(const ASocket: TFPSocket; MaxLength: SizeInt;
  AFlags: Integer): TReceiveFromStringMessage;
var
  UdpMessage: TReceiveFromResult;
begin
  Result := Default(TReceiveFromStringMessage);
  SetLength(Result.Data, MaxLength);
  UdpMessage := ReceiveFrom(ASocket, @Result.Data[1], MaxLength, AFlags);
  SetLength(Result.Data, UdpMessage.DataSize);
  Result.FromAddr := UdpMessage.FromAddr;
  Result.FromPort := UdpMessage.FromPort;
end;

function ReceiveStrFromNonBlocking(const ASocket: TFPSocket;
  MaxLength: SizeInt; AFlags: Integer): specialize TNullable<
  TReceiveFromStringMessage>;
var
  UdpMessage: TReceiveFromResult;
begin
  SetLength(Result.Ptr^.Data, MaxLength);
  UdpMessage := ReceiveFrom(ASocket, @Result.Ptr^.Data[1], MaxLength, AFlags);
  if UdpMessage.DataSize = 0 then
    Exit(null);
    
  SetLength(Result.Ptr^.Data, UdpMessage.DataSize);
  Result.Ptr^.FromAddr := UdpMessage.FromAddr;
  Result.Ptr^.FromPort := UdpMessage.FromPort;
end;

function SendStr(const ASocket: TFPSocket; const AData: String; AFlags: Integer
  ): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := Send(ASocket, @AData[1], Length(AData), AFlags);
end;

function SendStrTo(const ASocket: TFPSocket;
  const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: String; AFlags: Integer
  ): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := SendTo(ASocket, ReceiverAddr, ReceiverPort, @AData[1], Length(AData), AFlags);
end;

generic function Receive<T>(const ASocket: TFPSocket; AFlags: Integer = 0): T;
var
  Frag: TBytes;
  Len, ReadLen: SizeInt;
begin
  Result := Default(T);
  Len := 0;
  while Len < SizeOf(Result) do
  begin
    ReadLen := Receive(ASocket, @PByte(@Result)[Len], SizeOf(Result) - Len, AFlags);
    if ReadLen = 0 then
      if Len = 0 then
        raise ESocketCodeError.Create(EsockEWOULDBLOCK, 'recv')
      else // Fragment received but non blocking afterwards
      begin
        SetLength(Frag, Len);
        Move(Result, Frag[0], Len);
        raise EFragmentedData.Create(Frag, SizeOf(T), 'Only fragment received in non blocking read');
      end;
    Len += ReadLen;
  end;
end;

generic function ReceiveNonBlocking<T>(const ASocket: TFPSocket; AFlags: Integer = 0): specialize TNullable<T>;
var
  Frag: TBytes;
  Len, ReadLen: SizeInt;
begin
  Len := 0;
  while Len < SizeOf(Result.Ptr^) do
  begin
    ReadLen := Receive(ASocket, @PByte(@Result.Ptr^)[Len], SizeOf(Result.Ptr^) - Len, AFlags);
    if ReadLen = 0 then
      if Len = 0 then
        Exit(null)
      else // Fragment received but non blocking afterwards
      begin
        SetLength(Frag, Len);
        Move(Result.Ptr^, Frag[0], Len);
        raise EFragmentedData.Create(Frag, SizeOf(T), 'Only fragment received in non blocking read');
      end;
    Len += ReadLen;
  end;
end;

generic function ReceiveFrom<T>(const ASocket: TFPSocket; AFlags: Integer = 0): specialize TReceiveFromMessage<T>;
var
  Frag: TBytes;
  UdpMessage: TReceiveFromResult;
begin
  Result := Default(specialize TReceiveFromMessage<T>);
  UdpMessage := ReceiveFrom(ASocket, @Result.Data, SizeOf(Result.Data), AFlags);
  if UdpMessage.DataSize < SizeOf(T) then
    if UdpMessage.DataSize = 0 then
        raise ESocketCodeError.Create(EsockEWOULDBLOCK, 'recvfrom')
    else
    begin
      SetLength(Frag, UdpMessage.DataSize);
      Move(Result.Data, Frag[0], UdpMessage.DataSize);
      raise EFragmentedData.Create(Frag, SizeOf(T), 'Only fragment received ReceiveFrom, likely UDP Fragmentation');
    end;
  Result.FromAddr := UdpMessage.FromAddr;
  Result.FromPort := UdpMessage.FromPort;
end;

generic function ReceiveFromNonBlocking<T>(const ASocket: TFPSocket; AFlags: Integer = 0): specialize TNullable<specialize TReceiveFromMessage<T>>;
var
  Frag: TBytes;
  UdpMessage: TReceiveFromResult;
begin
  UdpMessage := ReceiveFrom(ASocket, @Result.Ptr^.Data, SizeOf(Result.Ptr^.Data), AFlags);
  if UdpMessage.DataSize < SizeOf(T) then
    if UdpMessage.DataSize = 0 then
      Exit(null)
    else
    begin
      SetLength(Frag, UdpMessage.DataSize);
      Move(Result.Ptr^.Data, Frag[0], UdpMessage.DataSize);
      raise EFragmentedData.Create(Frag, SizeOf(T), 'Only fragment received ReceiveFrom, likely UDP Fragmentation');
    end;
  Result.Ptr^.FromAddr := UdpMessage.FromAddr;
  Result.Ptr^.FromPort := UdpMessage.FromPort;
end;

generic function Send<T>(const ASocket: TFPSocket; constref AData: T; AFlags: Integer = 0): SizeInt;
begin
  Result := Send(ASocket, @AData, SizeOf(T), AFlags);
end;

generic function SendTo<T>(const ASocket: TFPSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; constref AData: T; AFlags: Integer = 0): SizeInt;
begin
  Result := SendTo(ASocket, ReceiverAddr, ReceiverPort, @AData, SizeOf(T), AFlags);
end;

generic function ReceiveArray<T>(const ASocket: TFPSocket; MaxCount: SizeInt;
  AFlags: Integer = 0): specialize TArray<T>;
const
  SizeOfT = SizeOf(T);
  ReadCount = 1024 div SizeOfT;
var
  Frag: TBytes;
  Len, ReadLen: SizeInt;
begin
  Result := nil;
  if (MaxCount < 0) and (ASocket.Protocol = spDatagram) then
  {$Push}
  {$WARN 6018 off}
    if SizeOf(T) < MaxUDPPackageSize then
      MaxCount := MaxUDPPackageSize div SizeOf(T)
    else // Lets try anyway and if it fails it fails
      MaxCount := 1;
  {$Pop}

  // If MaxCount, read MaxCount
  if MaxCount > 0 then
  begin
    SetLength(Result, MaxCount);
    Len := 0;
    repeat
      ReadLen := Receive(ASocket, @PByte(@Result[0])[Len], MaxCount * SizeOf(T) - Len, AFlags);
      if ReadLen = 0 then
        if Len = 0 then
          break
        else
        begin
          SetLength(Frag, Len);
          Move(Result[0], Frag[0], Len);
          raise EFragmentedData.Create(Frag, (Len div SizeOf(T) + 1) * SizeOf(T),
            'Receiving of fragmented data is not supported by typed receive');
        end;
      Len += ReadLen;
    until (Len mod SizeOf(T)) = 0;
    SetLength(Result, Len div SizeOf(T));
    Exit;
  end;

  // Else do a (blocking) read and then read as much as in buffer, plus block to finish open blocks
  Len := 0;
  MaxCount := BytesAvailable(ASocket) div SizeOfT;
  {$Push}
  {$WARN 6018 off}
  if MaxCount = 0 then
    if ReadCount = 0 then
      MaxCount := 1
    else
      MaxCount := ReadCount;
  {$Pop}
  repeat
    SetLength(Result, Length(Result)+MaxCount);
    ReadLen := Receive(ASocket, @PByte(@Result[0])[Len], MaxCount*SizeOfT, AFlags);
    if ReadLen = 0 then
      if Len = 0 then
        break
      else
      begin
        SetLength(Frag, Len);
        Move(Result[0], Frag[0], Len);
        raise EFragmentedData.Create(Frag, (Len div SizeOf(T) + 1) * SizeOf(T),
          'Receiving of fragmented data is not supported by typed receive');
      end;
    Len += ReadLen;
    MaxCount := BytesAvailable(ASocket) div SizeOfT;
    
  until ((Len<Length(Result)*SizeOf(T)) Or (MaxCount = 0)) And ((Len mod SizeOf(T)) = 0);
  SetLength(Result, Len div SizeOf(T));
end;

generic function ReceiveArrayFrom<T>(const ASocket: TFPSocket; MaxCount: SizeInt;
  AFlags: Integer = 0): specialize TReceiveFromMessage<specialize TArray<T>>;
var
  Frag: TBytes;
  UdpMessage: TReceiveFromResult;
begin
  if MaxCount < 0 then
    if SizeOf(T) < MaxUDPPackageSize then
      MaxCount := MaxUDPPackageSize div SizeOf(T)
    else // Lets try anyway and if it fails it fails
      MaxCount := 1;
  Result.Data := nil;
  SetLength(Result.Data, MaxCount);
  UdpMessage := ReceiveFrom(ASocket, @Result.Data[0], MaxCount * SizeOf(T), AFlags);
  if UdpMessage.DataSize mod SizeOf(T) > 0 then
  begin
    SetLength(Frag, UdpMessage.DataSize);
    Move(Result.Data[0], Frag[0], UdpMessage.DataSize);
    raise EFragmentedData.Create(Frag, (UdpMessage.DataSize div SizeOf(T) + 1) * SizeOf(T),
      'Receiving of fragmented data is not supported by typed receive');
  end;
  SetLength(Result.Data, UdpMessage.DataSize div SizeOf(T));
  Result.FromAddr := UdpMessage.FromAddr;
  Result.FromPort := UdpMessage.FromPort;
end;

generic function ReceiveArrayFromNonBlocking<T>(const ASocket: TFPSocket;
  MaxCount: SizeInt = -1; AFlags: Integer = 0
  ): specialize TNullable<specialize TReceiveFromMessage<specialize TArray<T>>>;
var
  Frag: TBytes;
  UdpMessage: TReceiveFromResult;
begin
  if MaxCount < 0 then
    if SizeOf(T) < MaxUDPPackageSize then
      MaxCount := MaxUDPPackageSize div SizeOf(T)
    else // Lets try anyway and if it fails it fails
      MaxCount := 1;
  Result.Ptr^.Data := nil;
  SetLength(Result.Ptr^.Data, MaxCount);
  UdpMessage := ReceiveFrom(ASocket, @Result.Ptr^.Data[0], MaxCount * SizeOf(T), AFlags);
  if UdpMessage.DataSize = 0 then
    Exit(null);
  if UdpMessage.DataSize mod SizeOf(T) > 0 then
  begin
    SetLength(Frag, UdpMessage.DataSize);
    Move(Result.Ptr^.Data[0], Frag[0], UdpMessage.DataSize);
    raise EFragmentedData.Create(Frag, (UdpMessage.DataSize div SizeOf(T) + 1) * SizeOf(T),
      'Receiving of fragmented data is not supported by typed receive');
  end;
  SetLength(Result.Ptr^.Data, UdpMessage.DataSize div SizeOf(T));
  Result.Ptr^.FromAddr := UdpMessage.FromAddr;
  Result.Ptr^.FromPort := UdpMessage.FromPort;
end;

generic function SendArray<T>(const ASocket: TFPSocket; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := Send(ASocket, @AData[0], Length(AData) * SizeOf(T), AFlags);
end;

generic function SendArrayTo<T>(const ASocket: TFPSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := SendTo(ASocket, ReceiverAddr, ReceiverPort, @AData[0], Length(AData) * SizeOf(T), AFlags);
end;

{$IfDef Windows}
procedure SetNonBlocking(const ASocket: TFPSocket; AValue: Boolean);
var
  nonblock: u_long;
begin
  nonblock := Ord(AValue);
  ioctlsocket(ASocket.FD, LongInt(FIONBIO), @nonblock);
end;
{$ENDIF}
{$IFDEF UNIX}
procedure SetNonBlocking(const ASocket: TFPSocket; AValue: Boolean);

var
  State: cint;
begin
  State := FpFcntl(ASocket.FD, F_GetFl);
  if AValue then
    State := State Or O_NONBLOCK
  else
    State := State And not O_NONBLOCK;
  FpFcntl(ASocket.FD, F_SetFL, state);
end;
{$EndIf}
{$IfDef HASAMIGA}
procedure SetNonBlocking(const ASocket: TFPSocket; AValue: Boolean);
var
  Arg: LongInt;
begin
  if AValue then
    arg := 1
  else
    Arg := 0;
  FpIOCtl(ASocket.FD, FIONBIO, @arg);
end;
{$EndIf}

{$IFNDEF FULL_IP_STACK}
procedure SetNonBlocking(const ASocket: TFPSocket; AValue: Boolean);

begin
  if avalue then
    Raise ENotImplemented.Create('NonBlocking socket')
end;
{$ENDIF}

{$IFDEF HAVE_SELECT_CALL}
function DataAvailable(const SocketArray: specialize TArray<TFPSocket>;
  TimeOut: Integer): specialize TArray<TFPSocket>;
var
  FDSet: TFDSet;
  MaxSock: socketsunit.TSocket;
  timeval: TTimeVal;
  Ret: LongInt;
  i, WriteHead: Integer;
begin
  Result := nil;
  MaxSock := 0;
  {$If Defined(UNIX) or Defined(HASAMIGA)}fpFD_ZERO{$else}FD_ZERO{$endif}(FDSet);
  for i:=0 to Length(SocketArray) - 1 do
  begin
    MaxSock := Max(MaxSock, SocketArray[i].FD);
    {$If Defined(UNIX) or Defined(HASAMIGA)}fpFD_SET{$else}FD_SET{$endif}(SocketArray[i].FD, FDSet);
  end;
  timeval.tv_sec := TimeOut div 1000;
  timeval.tv_usec := (TimeOut mod 1000) * 1000;
  Ret := {$If Defined(UNIX) or Defined(HASAMIGA)}fpselect{$else}select{$endif}(MaxSock + 1, @FDSet, nil, nil, @timeval);
  if Ret < 0 then
    raise ESocketCodeError.Create(socketerror, 'select');

  SetLength(Result, Ret);
  WriteHead := 0;
  for i:=0 to Length(SocketArray) - 1 do
    if {$If Defined(UNIX) or Defined(HASAMIGA)}fpFD_ISSET{$else}FD_ISSET{$endif}(SocketArray[i].FD, FDSet) {$If Defined(UNIX) or Defined(HASAMIGA)}> 0{$Endif} then
    begin
      Result[WriteHead] := SocketArray[i];
      Inc(WriteHead);
    end;
end;

function DataAvailable(const ASocket: TFPSocket; TimeOut: Integer): Boolean;
var
  Arr: array of TFPSocket;
begin
  Arr := [ASocket];
  Result := Length(DataAvailable(Arr, TimeOut)) > 0;
end;

function DataAvailable(const SocketArray: array of TFPSocket; TimeOut: Integer
  ): specialize TArray<TFPSocket>;
var
  Arr: array of TFPSocket;
begin
  if Length(SocketArray) = 0 then Exit(nil);
  SetLength(Arr, Length(SocketArray));
  Move(SocketArray[0], Arr[0], Length(SocketArray) * SizeOf(SocketArray[0]));
  Result := DataAvailable(arr, TimeOut);
end;

function BytesAvailable(const ASocket: TFPSocket): SizeInt;
var
  {$IfDef WINDOWS}
  count: DWord;
  {$Else}
  count: cint;
  {$EndIf}
begin
  Result := -1;
  {$IfDef WINDOWS}
  if ioctlsocket(ASocket.FD, FIONREAD, @count) = 0 then
  {$Else}
  if FpIOCtl(ASocket.FD, FIONREAD, @count) = 0 then
  {$EndIf}
    Result := Count;
end;

{$ELSE HAVE_SELECT_CALL}

function BytesAvailable(const ASocket: TFPSocket): SizeInt;
begin
  Result:=0;
end;

{$ENDIF HAVE_SELECT_CALL}



function StreamClosed(const ASocket:TFPSocket):Boolean;
begin
  Result := (ASocket.Protocol <> spStream) 
            {$IFDEF HAVE_SELECT_CALL}
            Or (
              DataAvailable(ASocket, 0) And
              (BytesAvailable(ASocket) = 0)
            )
            {$ENDIF}
            ;
end;

function ConnectionState(const ASocket:TFPSocket): TConnectionState;
const
  {$IFDEF WINDOWS}
  ECONNREFUSED = WSAECONNREFUSED;
  {$ENDIF}
  
  {$IFDEF UNIX}
  ECONNREFUSED = ESysECONNREFUSED;
  {$EndIf}

  {$IFDEF HASAMIGA}
  ECONNREFUSED = ESockECONNREFUSED;
  {$EndIf}

  {$IFNDEF FULL_IP_STACK}
  ECONNREFUSED = -999;
  {$ENDIF}
begin
  if (ASocket.Protocol <> spStream) then
    Exit(csNotConnected);
  if (socketsunit.fprecv(ASocket.FD, nil, 0, 0) = 0) And
     (socketsunit.fpsend(ASocket.FD, nil, 0, 0) = 0) then
    Exit(csConnected);
  case socketerror of
  EsockEWOULDBLOCK: Result := csConnected;
  ESockENOTCONN: Result := csPending;
  ECONNREFUSED: Result := csRefused;
  else
    Result := csError;
  end;
end;

function LocalEndpoint(const ASocket:TFPSocket):specialize TPair<TNetworkAddress
  ,Word>;
var
  addr: TAddressUnion;
  len: TSocklen;
begin
  len:=SizeOf(addr);
  if fpGetSockName(ASocket.FD,psockaddr(@addr),@len) <> 0 then
    raise ESocketCodeError.Create(socketerror,'getsockname');
  ReadAddr(addr,ASocket.SocketType=stIPDualStack,Result.First,Result.Second);
end;

function RemoteEndpoint(const ASocket:TFPSocket):specialize TPair<
  TNetworkAddress,Word>;
var
  addr: TAddressUnion;
  len: TSocklen;
begin
  len:=SizeOf(addr);
  if fpGetPeerName(ASocket.FD,psockaddr(@addr),@len) <> 0 then
    raise ESocketCodeError.Create(socketerror,'getpeername');
  ReadAddr(addr,ASocket.SocketType=stIPDualStack,Result.First,Result.Second);
end;

operator:=(const AAddr:TNetworkAddress):String;
begin
  Result := AAddr.Address;
end;

operator=(const AStr:String;const AAddr:TNetworkAddress):Boolean;
begin
  Result:=NetAddr(AStr)=AAddr;
end;

operator=(const AAddr:TNetworkAddress;const AStr:String):Boolean;
begin
  Result:=AAddr=NetAddr(AStr);
end;

{ TFPSocket }

constructor TFPSocket.create(aFD: TSocket; aProtocol: TFPSocketProtocol; aType: TFPSocketType);
begin
  FD:=aFD;
  Protocol:=aProtocol;
  SocketType:=aType;
end;

{ ESocketCodeError }

constructor ESocketCodeError.Create(ACode: Integer; const FunName: String);
begin
  inherited CreateFmt('[Socket Error: %d] %s call failed',  [ACode, FunName]);
  FCode := ACode;
end;

{ EFragmentedData }

constructor EFragmentedData.Create(const AFragment: TBytes; AExpected: SizeInt;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FFragment := AFragment;
  FExpectedSize := AExpected;
end;

end.

