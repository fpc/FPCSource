{ TInetServer.HandshakeTimeout with TOpenSSLSocketHandler: an idle TCP connection that never starts the TLS
  handshake is closed after the timeout and does not block later clients. Exit code is the number of failed checks. }
program thandshaketimeout;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads, baseunix,{$endif}
  Classes, SysUtils, ssockets, sslbase, sslsockets, openssl, opensslsockets;

const
  ServerPort = 44398;
  Timeout = 1000;

type

  { TTimeoutServer }

  TTimeoutServer = class(TThread)
  private
    FServer: TInetServer;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  protected
    procedure Execute; override;
  public
    // Creates a TLS server on 127.0.0.1:aPort with HandshakeTimeout set to Timeout
    constructor Create(aPort: Word);
    // Stops accepting and frees the server
    destructor Destroy; override;
  end;

var
  gFailures: Integer;
  gConnectionTimeout: Integer = -1;


{ TTimeoutServer }

constructor TTimeoutServer.Create(aPort: Word);

begin
  FServer:=TInetServer.Create('127.0.0.1',aPort,TOpenSSLSocketHandler.Create);
  FServer.ReuseAddress:=True;
  FServer.HandshakeTimeout:=Timeout;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TTimeoutServer.Destroy;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TTimeoutServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

begin
  aHandler:=TOpenSSLSocketHandler.Create;
end;


procedure TTimeoutServer.DoConnect(Sender: TObject; aData: TSocketStream);

begin
  gConnectionTimeout:=aData.IOTimeout;
  aData.Free;
end;


procedure TTimeoutServer.Execute;

begin
  try
    FServer.StartAccepting;
  except
    on E: Exception do
      Writeln('server exception ',E.ClassName,': ',E.Message);
  end;
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


// Connects with TLS within 5 seconds; aMilliseconds is the time taken
function TLSConnect(out aMilliseconds: QWord): Boolean;

var
  lSocket: TInetSocket;
  lStart: QWord;

begin
  lStart:=GetTickCount64;
  lSocket:=TInetSocket.Create('127.0.0.1',ServerPort,TOpenSSLSocketHandler.Create);
  try
    lSocket.IOTimeout:=5000;
    try
      lSocket.Connect;
      Result:=True;
    except
      Result:=False;
    end;
  finally
    lSocket.Free;
  end;
  aMilliseconds:=GetTickCount64-lStart;
end;


var
  lServer: TTimeoutServer;
  lIdle: TInetSocket;
  lTime: QWord;
  lBuf: Byte;
  lRead: Integer;

begin
  {$ifdef unix}
  fpSignal(SIGPIPE,SignalHandler(SIG_IGN));
  {$endif}
  lServer:=TTimeoutServer.Create(ServerPort);
  try
    Sleep(200);
    Check(TLSConnect(lTime),'handshake without idle connection');
    lTime:=GetTickCount64;
    while (gConnectionTimeout=-1) and (GetTickCount64-lTime<2000) do
      Sleep(10);
    Check(gConnectionTimeout=0,'connection IOTimeout restored to 0 after the handshake (got '+IntToStr(gConnectionTimeout)+')');
    lIdle:=TInetSocket.Create('127.0.0.1',ServerPort);
    try
      Sleep(100);
      Check(TLSConnect(lTime),'handshake while an idle TCP connection is open');
      Check(lTime<Timeout+3000,'handshake done within the handshake timeout ('+IntToStr(lTime)+' ms)');
      lIdle.IOTimeout:=3000;
      lRead:=lIdle.Read(lBuf,1);
      Check(lRead=0,'idle connection closed by the server (read returned '+IntToStr(lRead)+')');
    finally
      lIdle.Free;
    end;
    Check(TLSConnect(lTime),'handshake after the idle connection');
  finally
    lServer.Free;
  end;
  Writeln(gFailures,' failure(s)');
  Halt(gFailures);
end.
