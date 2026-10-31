{ %target=linux,freebsd,openbsd,netbsd,dragonfly,darwin,solaris }
{ Layout of the socket address records on BSD systems with a length byte, issue #41991 }
program tsockaddrlen;

{$mode objfpc}{$h+}

uses
  BaseUnix, Sockets;

{$if defined(freebsd) or defined(openbsd) or defined(netbsd) or defined(dragonfly) or defined(darwin)}
  {$define HASLENBYTE}
{$endif}


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg,' (errno ',SocketError,')');
    halt(aCode);
    end;
end;


var
  lListener, lClient: cint;
  lAddr, lName: TInetSockAddr;
  lLen: TSockLen;

begin
{$ifdef HASLENBYTE}
  Check(1,SizeOf(sa_family_t)=1,'sa_family_t is one byte');
  Check(2,PtrUInt(@PInetSockAddr(nil)^.sin_family)=1,'sin_family follows the length byte');
  Check(3,PtrUInt(@PSockAddr(nil)^.sa_family)=1,'sa_family follows the length byte');
{$else}
  Check(1,SizeOf(sa_family_t)=2,'sa_family_t is two bytes');
  Check(2,PtrUInt(@PInetSockAddr(nil)^.sin_family)=0,'sin_family is the first field');
{$endif}
  Check(4,SizeOf(TInetSockAddr)=16,'size of sockaddr_in');
  Check(5,PtrUInt(@PInetSockAddr(nil)^.sin_port)=2,'offset of sin_port');
  Check(6,PtrUInt(@PInetSockAddr(nil)^.sin_addr)=4,'offset of sin_addr');
  Check(7,SizeOf(TInetSockAddr6)=28,'size of sockaddr_in6');

  lListener:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(10,lListener>=0,'fpSocket');
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  Check(11,fpBind(lListener,@lAddr,SizeOf(lAddr))=0,'fpBind to 127.0.0.1');
  FillChar(lName,SizeOf(lName),0);
  lLen:=SizeOf(lName);
  Check(12,fpGetSockName(lListener,@lName,@lLen)=0,'fpGetSockName');
  Check(13,lName.sin_family=AF_INET,'fpGetSockName returns family AF_INET');
  Check(14,NetAddrToStr(lName.sin_addr)='127.0.0.1','fpGetSockName returns the bound address');
  Check(15,fpListen(lListener,1)=0,'fpListen');
  lClient:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(16,fpConnect(lClient,@lName,SizeOf(lName))=0,'fpConnect to the bound address');
  CloseSocket(lClient);
  CloseSocket(lListener);
  writeln('ok');
end.
