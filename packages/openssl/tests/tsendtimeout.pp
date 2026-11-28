{ Write on a TLS connection with IOTimeout: against a peer that stops reading it fails after the timeout,
  with the base and the duplex OpenSSL handler; against a slow reader a large write succeeds.
  Exit code is the number of failed checks. }
program tsendtimeout;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads, baseunix,{$endif}
  Classes, SysUtils, sockets, ssockets, sslbase, sslsockets, openssl, opensslsockets, opensslduplexsockets;

const
  ServerPort = 44399;
  Timeout = 1000;
  BlockSize = 16384;
  LargeSize = 16*1024*1024;

type
  TPeerMode = (pmStall, pmSlowRead);

  { TPeerServer }

  TPeerServer = class(TThread)
  private
    FServer: TInetServer;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  protected
    procedure Execute; override;
  public
    // Creates a TLS server on 127.0.0.1:aPort that handles one connection at a time as set by gPeerMode
    constructor Create(aPort: Word);
    // Stops accepting and frees the server
    destructor Destroy; override;
  end;

  { TWriter }

  TWriter = class(TThread)
  private
    FDuplex: Boolean;
    FLarge: Boolean;
  protected
    procedure Execute; override;
  public
    // Connects with the base or duplex handler and writes blocks until a write fails, or one large block
    constructor Create(aDuplex, aLarge: Boolean);
  end;

var
  gFailures: Integer;
  gPeerMode: TPeerMode;
  gReleasePeer: Boolean;
  gDone: Boolean;
  gResult: Integer;
  gLastError: Integer;
  gTotal: Int64;
  gLastProgress: QWord;
  gMessage: String;


{ TPeerServer }

constructor TPeerServer.Create(aPort: Word);

begin
  FServer:=TInetServer.Create('127.0.0.1',aPort,TOpenSSLSocketHandler.Create);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TPeerServer.Destroy;

begin
  gReleasePeer:=True;
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TPeerServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

begin
  aHandler:=TOpenSSLSocketHandler.Create;
end;


procedure TPeerServer.DoConnect(Sender: TObject; aData: TSocketStream);

var
  lBuf: array of Byte;
  lRead: Integer;
  lTotal, lChunk: Int64;

begin
  try
    if gPeerMode=pmStall then
      while not gReleasePeer do
        Sleep(20)
    else
      begin
      // 1 MB every 200 ms
      SetLength(lBuf,BlockSize);
      lTotal:=0;
      repeat
        Sleep(200);
        lChunk:=0;
        repeat
          lRead:=aData.Read(lBuf[0],Length(lBuf));
          if lRead>0 then
            Inc(lChunk,lRead);
        until (lRead<=0) or (lChunk>=1024*1024);
        Inc(lTotal,lChunk);
      until (lRead<=0) or (lTotal>=LargeSize) or gReleasePeer;
      end;
  finally
    aData.Free;
  end;
end;


procedure TPeerServer.Execute;

begin
  try
    FServer.StartAccepting;
  except
    on E: Exception do
      Writeln('server exception ',E.ClassName,': ',E.Message);
  end;
end;


{ TWriter }

constructor TWriter.Create(aDuplex, aLarge: Boolean);

begin
  FDuplex:=aDuplex;
  FLarge:=aLarge;
  FreeOnTerminate:=True;
  inherited Create(False);
end;


procedure TWriter.Execute;

var
  lHandler: TSocketHandler;
  lSocket: TInetSocket;
  lBuf: array of Byte;

begin
  try
    if FDuplex then
      lHandler:=TOpenSSLDuplexSocketHandler.Create
    else
      lHandler:=TOpenSSLSocketHandler.Create;
    lSocket:=TInetSocket.Create('127.0.0.1',ServerPort,lHandler);
    try
      lSocket.Connect;
      lSocket.IOTimeout:=Timeout;
      if FLarge then
        SetLength(lBuf,LargeSize)
      else
        SetLength(lBuf,BlockSize);
      FillChar(lBuf[0],Length(lBuf),65);
      gLastProgress:=GetTickCount64;
      repeat
        gResult:=lSocket.Write(lBuf[0],Length(lBuf));
        if gResult>0 then
          begin
          Inc(gTotal,gResult);
          gLastProgress:=GetTickCount64;
          end;
      until (gResult<=0) or FLarge;
      gLastError:=lHandler.LastError;
    finally
      lSocket.Free;
    end;
  except
    on E: Exception do
      gMessage:=E.ClassName+': '+E.Message;
  end;
  gDone:=True;
end;


// Records the outcome of one check
procedure Check(aOK: Boolean; const aMessage: String);

begin
  if aOK then
    Writeln('ok    ',aMessage)
  else
    begin
    Writeln('FAIL  ',aMessage);
    Inc(gFailures);
    end;
end;


// Runs a writer against a peer in mode aMode; False when it does not finish within 20 seconds
function RunWriter(aMode: TPeerMode; aDuplex, aLarge: Boolean): Boolean;

var
  lStart: QWord;

begin
  gPeerMode:=aMode;
  gReleasePeer:=False;
  gDone:=False;
  gResult:=0;
  gLastError:=0;
  gTotal:=0;
  gMessage:='';
  TWriter.Create(aDuplex,aLarge);
  lStart:=GetTickCount64;
  while not gDone and (GetTickCount64-lStart<20000) do
    Sleep(20);
  Result:=gDone;
  gReleasePeer:=True;
  if gMessage<>'' then
    Writeln('      writer exception ',gMessage);
end;


// Checks a writer against a peer that stops reading
procedure CheckStalledPeer(aDuplex: Boolean);

var
  lName: String;
  lIdle: QWord;

begin
  if aDuplex then
    lName:='duplex handler'
  else
    lName:='base handler';
  if not RunWriter(pmStall,aDuplex,False) then
    begin
    Check(False,lName+': write against a stalled peer still blocked after 20 s ('+IntToStr(gTotal)+' bytes)');
    Exit;
    end;
  lIdle:=GetTickCount64-gLastProgress;
  Check(gResult=-1,lName+': write against a stalled peer returns -1 (got '+IntToStr(gResult)+')');
  Check(lIdle<Timeout+3000,lName+': write fails within the timeout ('+IntToStr(lIdle)+' ms after the last progress)');
  Check(gLastError=EsockEWOULDBLOCK,lName+': LastError is EsockEWOULDBLOCK (got '+IntToStr(gLastError)+')');
  Sleep(100);
end;


var
  lServer: TPeerServer;

begin
  {$ifdef unix}
  fpSignal(SIGPIPE,SignalHandler(SIG_IGN));
  {$endif}
  lServer:=TPeerServer.Create(ServerPort);
  try
    Sleep(200);
    CheckStalledPeer(False);
    CheckStalledPeer(True);
    if RunWriter(pmSlowRead,False,True) then
      Check(gResult=LargeSize,'base handler: large write to a slow reader succeeds (got '+IntToStr(gResult)+')')
    else
      Check(False,'base handler: large write to a slow reader still blocked after 20 s');
  finally
    lServer.Free;
  end;
  Writeln(gFailures,' failure(s)');
  Halt(gFailures);
end.
