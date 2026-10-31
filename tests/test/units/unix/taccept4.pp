{ %target=linux,freebsd,openbsd,dragonfly,solaris }
{ fpaccept4 and the SOCK_CLOEXEC and SOCK_NONBLOCK flags, issue #41990 }
program taccept4;

{$mode objfpc}{$h+}

uses
  BaseUnix, Sockets;

const
  cFdCloExec = 1;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg,' (errno ',SocketError,')');
    halt(aCode);
    end;
end;


// True if the close-on-exec flag of descriptor aFd is set
function IsCloseOnExec(aFd: cint): Boolean;

begin
  Result:=(FpFcntl(aFd,F_GETFD) and cFdCloExec)<>0;
end;


// True if the non-blocking flag of descriptor aFd is set
function IsNonBlocking(aFd: cint): Boolean;

begin
  Result:=(FpFcntl(aFd,F_GETFL) and O_NONBLOCK)<>0;
end;


var
  lListener, lClient, lPlain, lFlagged: cint;
  lAddr, lPeer: TInetSockAddr;
  lLen: TSockLen;

begin
  lListener:=fpSocket(AF_INET,SOCK_STREAM or SOCK_CLOEXEC,0);
  Check(1,lListener>=0,'fpSocket with SOCK_CLOEXEC');
  Check(2,IsCloseOnExec(lListener),'fpSocket with SOCK_CLOEXEC sets close-on-exec');
  Check(3,not IsNonBlocking(lListener),'fpSocket without SOCK_NONBLOCK is blocking');

  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  lAddr.sin_port:=0;
  Check(4,fpBind(lListener,@lAddr,SizeOf(lAddr))=0,'fpBind');
  lLen:=SizeOf(lAddr);
  Check(5,fpGetSockName(lListener,@lAddr,@lLen)=0,'fpGetSockName');
  Check(6,fpListen(lListener,4)=0,'fpListen');

  lClient:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(10,fpConnect(lClient,@lAddr,SizeOf(lAddr))=0,'first fpConnect');
  lLen:=SizeOf(lPeer);
  lPlain:=fpAccept4(lListener,@lPeer,@lLen,0);
  Check(11,lPlain>=0,'fpAccept4 without flags');
  Check(12,not IsCloseOnExec(lPlain) and not IsNonBlocking(lPlain),'fpAccept4 without flags sets no flags');
  CloseSocket(lClient);

  lClient:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(20,fpConnect(lClient,@lAddr,SizeOf(lAddr))=0,'second fpConnect');
  lLen:=SizeOf(lPeer);
  lFlagged:=fpAccept4(lListener,@lPeer,@lLen,SOCK_CLOEXEC or SOCK_NONBLOCK);
  Check(21,lFlagged>=0,'fpAccept4 with SOCK_CLOEXEC and SOCK_NONBLOCK');
  Check(22,IsCloseOnExec(lFlagged),'fpAccept4 with SOCK_CLOEXEC sets close-on-exec');
  Check(23,IsNonBlocking(lFlagged),'fpAccept4 with SOCK_NONBLOCK sets non-blocking');
  Check(24,lPeer.sin_family=AF_INET,'fpAccept4 returns the peer address');

  CloseSocket(lClient);
  CloseSocket(lPlain);
  CloseSocket(lFlagged);
  CloseSocket(lListener);
  writeln('ok');
end.
