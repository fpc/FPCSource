{! completely untested !}


{******************************************************************************
                          Import Socket Functions from nlmlib
******************************************************************************}

CONST SockLib = 'nlmlib.nlm';

Function _NWsocket(Domain,SocketType,Protocol:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'socket';
Function _NWSend(Sock:Longint;Const Addr;AddrLen,Flags:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'send';
Function _NWSendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint; Var Addr; AddrLen : longint):Longint; CDECL; EXTERNAL SockLib NAME 'sendto';
Function _NWRecv(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'recv';
Function _NWRecvFrom(Sock:Longint;Var Buf;BufLen,Flags:Longint; Var Addr; AddrLen : Longint):Longint; CDECL; EXTERNAL SockLib NAME 'recvfrom';
Function _NWBind(Sock:Longint;Const Addr;AddrLen:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'bind';
Function _NWListen(Sock,MaxConnect:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'listen';
Function _NWAccept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'accept';
Function _NWConnect(Sock:Longint;Const Addr;Addrlen:Longint): longint; CDECL; EXTERNAL SockLib NAME 'connect';
Function _NWShutdown(Sock:Longint;How:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'shutdown';
Function _NWGetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'getsocketname';
Function _NWGetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint; CDECL; EXTERNAL SockLib NAME 'getpeername';
Function _NWSetSockOpt(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint; CDECL; EXTERNAL SockLib NAME 'setsockopt';
Function _NWGetSockOpt(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint; CDECL; EXTERNAL SockLib NAME 'getsockopt';


{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket:=_NWSocket(Domain,SocketType,Protocol);
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
begin
  Send:=_NWSend(Sock,Buf,BufLen,Flags);
end;

Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
begin
  SendTo := _NWSendTo (Sock, Buf, BufLen, Flags, Addr, AddrLen);
end;


Function Recv(Sock:Longint;Var Buf; BufLen,Flags:Longint):Longint;
begin
  Recv:=_NWRecv(Sock,Buf,BufLen,Flags);
end;

Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr ; AddrLen : Integer) : longint;
begin
  RecvFrom := _NWRecvFrom (Sock, Buf, BufLen, Flags, Addr, AddrLen);
end;


Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
begin
  Bind:=(_NWBind(Sock,Addr,AddrLen)=0);
end;

Function Listen(Sock,MaxConnect:Longint):Boolean;
begin
  Listen:=(_NWListen(Sock,MaxConnect)=0);
end;

Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Accept:=_NWAccept(Sock,Addr,AddrLen);
  If Accept<0 Then
    Accept:=-1;
end;

Function Connect(Sock:Longint;Const Addr;Addrlen:Longint): boolean;
begin
  Connect:=_NWConnect(Sock,Addr,AddrLen)=0;
end;


Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=_NWShutdown(Sock,How);
end;


Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetSocketName:=_NWGetSocketName(Sock,Addr,AddrLen);
end;



Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetPeerName:=_NWGetPeerName(Sock,Addr,AddrLen);
end;



Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=_NWSetsockopt(Sock,Level,OptName,OptVal,OptLen);
end;



Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
begin
  GetSocketOptions:=_NWGetsockopt(Sock,Level,OptName,OptVal,OptLen);
end;



Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
  //SocketPair:=do_syscall(syscall_nr_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
  Socketpair := -1;
end;

{******************************************************************************
                               UnixSock
******************************************************************************}

Procedure Str2UnixSockAddr(const addr:string;var t:TUnixSockAddr;var len:longint);
begin
  Move(Addr[1],t.Path,length(Addr));
  t.Family:=AF_UNIX;
  t.Path[length(Addr)]:=#0;
  Len:=Length(Addr)+3;
end;


Function Bind(Sock:longint;const addr:string):boolean;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(addr,UnixAddr,AddrLen);
  Bind(Sock,UnixAddr,AddrLen);
  Bind:=(SocketError=0);
end;



Function DoAccept(Sock:longint;var addr:string):longint;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  AddrLen:=length(addr)+3;
  DoAccept:=Accept(Sock,UnixAddr,AddrLen);
  Move(UnixAddr.Path,Addr[1],AddrLen);
  SetLength(Addr,AddrLen);
end;



Function DoConnect(Sock:longint;const addr:string):Boolean;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(addr,UnixAddr,AddrLen);
  DoConnect:=Connect(Sock,UnixAddr,AddrLen);
end;

Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:text):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2Text(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;



Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:File):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2File(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;



Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:text):Boolean;
begin
  Connect:=DoConnect(Sock,addr);
  If Connect then
     Sock2Text(Sock,SockIn,SockOut);
end;



Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:file):Boolean;
begin
  Connect:=DoConnect(Sock,addr);
  if Connect then
     Sock2File(Sock,SockIn,SockOut);
end;


// fsread and fswrite are used in socket.inc
procedure fdwrite (Handle:longint; VAR Data; Len : LONGINT);
begin
  { this has to be checked: }
  _NWSend(Handle,Data,Len,0);
end;

function fdread (Handle:longint; VAR Data; Len : LONGINT) : LONGINT;
begin
  { this has to be checked: }
  fdread := _NWRecv(Handle,Data,Len,0);
end;
