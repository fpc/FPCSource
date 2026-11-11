program tsocketclose;

{ A closed TSocketStream must not close its descriptor again (issue #42057).
  Relies on the lowest free descriptor being reused, as on Linux. }

{$mode objfpc}{$H+}

uses
{$IFDEF TEST_DOTTEDUNITS}
  System.SysUtils, System.Net.Sockets, System.Net.Ssockets;
{$ELSE}
  SysUtils, sockets, ssockets;
{$ENDIF}

type
  TRefuseHandler = class(TSocketHandler)
  public
    function Connect: Boolean; override;
  end;

  TCloseCounter = class
  public
    Count : Integer;
    procedure DoClose(Sender: TObject);
  end;

function TRefuseHandler.Connect: Boolean;

begin
  Result:=False;
end;


procedure TCloseCounter.DoClose(Sender: TObject);

begin
  Inc(Count);
end;


// Halts with exit code 1 when aCondition does not hold
procedure Check(aCondition: Boolean; const aMsg: String);

begin
  if not aCondition then
    begin
    Writeln('FAIL: ',aMsg);
    Halt(1);
    end;
end;


// True if aFD is an open socket
function SocketIsOpen(aFD: LongInt): Boolean;

var
  lType: LongInt;
  lLen: TSockLen;

begin
  lLen:=SizeOf(lType);
  Result:=fpgetsockopt(aFD,SOL_SOCKET,SO_TYPE,@lType,@lLen)=0;
end;


// Creates a socket listening on 127.0.0.1 and returns its port in aPort
function CreateListener(out aPort: Word): LongInt;

var
  lAddr: TInetSockAddr;
  lLen: TSockLen;

begin
  Result:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(Result>=0,'listener socket created');
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  Check(fpBind(Result,@lAddr,SizeOf(lAddr))=0,'listener bound');
  Check(fpListen(Result,8)=0,'listener listening');
  lLen:=SizeOf(lAddr);
  Check(fpGetSockName(Result,@lAddr,@lLen)=0,'listener address read');
  aPort:=NToHs(lAddr.sin_port);
end;


// A refused handler connect closes the socket early; Free must not close the descriptor reused since
procedure TestRefusedConnect(aPort: Word);

var
  lStream: TInetSocket;
  lCounter: TCloseCounter;
  lOldFD, lOther: LongInt;
  lRaised: Boolean;

begin
  lCounter:=TCloseCounter.Create;
  lStream:=TInetSocket.Create('127.0.0.1',aPort,TRefuseHandler.Create);
  lStream.OnClose:=@lCounter.DoClose;
  lOldFD:=lStream.Handle;
  lRaised:=False;
  try
    lStream.Connect;
  except
    on E: ESocketError do
      lRaised:=E.Code=seConnectFailed;
  end;
  Check(lRaised,'refused handler connect raises seConnectFailed');
  lOther:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(lOther=lOldFD,'new socket reuses the descriptor of the refused stream');
  lStream.Free;
  Check(SocketIsOpen(lOther),'freeing the refused stream leaves the new socket open');
  Check(lCounter.Count=1,'OnClose called once for the refused stream');
  CloseSocket(lOther);
  lCounter.Free;
end;


// Close followed by Free must close the descriptor only once
procedure TestCloseThenFree(aPort: Word);

var
  lStream: TInetSocket;
  lCounter: TCloseCounter;
  lOldFD, lOther: LongInt;

begin
  lCounter:=TCloseCounter.Create;
  lStream:=TInetSocket.Create('127.0.0.1',aPort,TSocketHandler.Create);
  lStream.OnClose:=@lCounter.DoClose;
  lOldFD:=lStream.Handle;
  lStream.Connect;
  lStream.Close;
  Check(lStream.Closed,'Closed is set after Close');
  lOther:=fpSocket(AF_INET,SOCK_STREAM,0);
  Check(lOther=lOldFD,'new socket reuses the descriptor of the closed stream');
  lStream.Free;
  Check(SocketIsOpen(lOther),'freeing a closed stream leaves the new socket open');
  Check(lCounter.Count=1,'OnClose called once for Close followed by Free');
  CloseSocket(lOther);
  lCounter.Free;
end;


var
  lListener: LongInt;
  lPort: Word;

begin
  lListener:=CreateListener(lPort);
  TestRefusedConnect(lPort);
  TestCloseThenFree(lPort);
  CloseSocket(lListener);
  Writeln('ok');
end.
