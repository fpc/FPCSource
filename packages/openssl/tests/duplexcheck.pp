program duplexcheck;

{ TOpenSSLDuplexSocketHandler: a client thread reads while another writes, under full load in both
  directions, while the server requests a TLS 1.3 KeyUpdate every few blocks.
  usage: duplexcheck [runs, default 3] [duplex|base, default duplex; base expects failures] }

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, DateUtils, dynlibs, ssockets, sslbase, sslsockets, openssl,
  opensslsockets, opensslduplexsockets;

const
  TestPort = 44393;
  BlockSize = 4096;
  BlockCount = 2048;
  TotalBytes = BlockSize*BlockCount;
  KeyUpdateEvery = 2;
  RunTimeoutMs = 60000;
  ClientIOTimeout = 10000;
  SslKeyUpdateRequested = 1;

type
  TSslKeyUpdate = function(aSSL: PSSL; aUpdateType: Integer): Integer; cdecl;

  TStreamCheck = record
    Offset: Int64;
    Bad: Int64;
  end;

  { TKeyUpdateServer }

  TKeyUpdateServer = class(TThread)
  private
    FServer: TInetServer;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  protected
    procedure Execute; override;
  public
    // Set by the client when it has read everything; the server keeps the connection open until then
    ClientFinished: Boolean;
    // Result of the last connection, written by the server thread
    Done: Boolean;
    Report: String;
    Received: TStreamCheck;
    Sent: Int64;
    KeyUpdates: Integer;
    // Creates the listening server and starts accepting
    constructor Create;
    // Stops accepting and frees the server
    destructor Destroy; override;
  end;

  { TReaderThread }

  TReaderThread = class(TThread)
  private
    FStream: TSocketStream;
  protected
    procedure Execute; override;
  public
    // What the thread read, and the reason it stopped early
    Received: TStreamCheck;
    Report: String;
    // Creates a started thread that reads TotalBytes from aStream
    constructor Create(aStream: TSocketStream);
  end;

var
  gKeyUpdate: TSslKeyUpdate;
  gClientHandlerClass: TSSLSocketHandlerClass;


// Fills aBuffer with the test stream bytes starting at aOffset
procedure FillBlock(var aBuffer: array of Byte; aOffset: Int64);

var
  I: Integer;

begin
  for I:=0 to High(aBuffer) do
    aBuffer[I]:=Byte((aOffset+I) mod 251);
end;


// Checks aCount received bytes against the test stream and advances aCheck
procedure CheckBytes(var aCheck: TStreamCheck; const aBuffer: array of Byte; aCount: Integer);

var
  I: Integer;

begin
  for I:=0 to aCount-1 do
    if aBuffer[I]<>Byte((aCheck.Offset+I) mod 251) then
      Inc(aCheck.Bad);
  Inc(aCheck.Offset,aCount);
end;


{ TKeyUpdateServer }

constructor TKeyUpdateServer.Create;

begin
  FServer:=TInetServer.Create('127.0.0.1',TestPort,TOpenSSLSocketHandler.Create);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TKeyUpdateServer.Destroy;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TKeyUpdateServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

var
  lHandler: TOpenSSLSocketHandler;

begin
  lHandler:=TOpenSSLSocketHandler.Create;
  lHandler.CertificateData.HostName:='localhost';
  aHandler:=lHandler;
end;


procedure TKeyUpdateServer.DoConnect(Sender: TObject; aData: TSocketStream);

var
  lHandler: TOpenSSLSocketHandler;
  lOut, lIn: array[0..BlockSize-1] of Byte;
  lGot: Integer;
  lBlock: Integer;
  lStart: TDateTime;

begin
  lHandler:=TOpenSSLSocketHandler(aData.Handler);
  lBlock:=0;
  lStart:=Now;
  try
    aData.IOTimeout:=100;
    while ((Sent<TotalBytes) or (Received.Offset<TotalBytes)) and (Report='') and not ClientFinished do
      begin
      if MilliSecondsBetween(Now,lStart)>RunTimeoutMs then
        Report:=Format('server timeout: sent %d, received %d',[Sent,Received.Offset])
      else if Sent<TotalBytes then
        begin
        if (lBlock>0) and (lBlock mod KeyUpdateEvery=0) and (gKeyUpdate(lHandler.SSL.SSL,SslKeyUpdateRequested)=1) then
          Inc(KeyUpdates);
        FillBlock(lOut,Sent);
        if aData.Write(lOut,BlockSize)<>BlockSize then
          Report:=Format('server write failed after %d bytes: %s',[Sent,lHandler.GetLastErrorDescription])
        else
          begin
          Inc(Sent,BlockSize);
          Inc(lBlock);
          end;
        end;
      while (Report='') and (Received.Offset<TotalBytes)
        and ((Sent>=TotalBytes) or (lHandler.BytesAvailable>0) or aData.CanRead(0)) do
        begin
        lGot:=aData.Read(lIn,BlockSize);
        if lGot>0 then
          begin
          CheckBytes(Received,lIn,lGot);
          // While sending, at most one block is read per written block
          if Sent<TotalBytes then
            Break;
          end
        else if lGot<0 then
          Report:=Format('server read failed after %d bytes: %s',[Received.Offset,lHandler.GetLastErrorDescription])
        else if Sent<TotalBytes then
          Break
        else if MilliSecondsBetween(Now,lStart)>RunTimeoutMs then
          Break;
        end;
      end;
    while not ClientFinished and (MilliSecondsBetween(Now,lStart)<=RunTimeoutMs) do
      if aData.CanRead(50) then
        aData.Read(lIn,BlockSize);
  except
    on E: Exception do
      Report:='server exception '+E.ClassName+': '+E.Message;
  end;
  Done:=True;
  aData.Free;
end;


procedure TKeyUpdateServer.Execute;

begin
  FServer.StartAccepting;
end;


{ TReaderThread }

constructor TReaderThread.Create(aStream: TSocketStream);

begin
  FStream:=aStream;
  inherited Create(False);
end;


procedure TReaderThread.Execute;

var
  lBuffer: array[0..BlockSize-1] of Byte;
  lGot: Integer;

begin
  try
    while Received.Offset<TotalBytes do
      begin
      lGot:=FStream.Read(lBuffer,BlockSize);
      if lGot<=0 then
        begin
        Report:=Format('client read returned %d after %d bytes: %s',
          [lGot,Received.Offset,FStream.Handler.GetLastErrorDescription]);
        Exit;
        end;
      CheckBytes(Received,lBuffer,lGot);
      end;
  except
    on E: Exception do
      Report:='client read exception '+E.ClassName+': '+E.Message;
  end;
end;


// Runs one connection; True when both directions arrived complete and intact
function RunOnce(aServer: TKeyUpdateServer; out aLine: String): Boolean;

var
  lClient: TInetSocket;
  lHandler: TSSLSocketHandler;
  lReader: TReaderThread;
  lBlock: array[0..BlockSize-1] of Byte;
  lSent: Int64;
  lWait: Integer;
  lWriteReport: String;

begin
  aServer.ClientFinished:=False;
  aServer.Done:=False;
  aServer.Report:='';
  aServer.Received:=Default(TStreamCheck);
  aServer.Sent:=0;
  aServer.KeyUpdates:=0;
  lWriteReport:='';
  lSent:=0;
  lHandler:=gClientHandlerClass.Create;
  lHandler.CertificateData.HostName:='localhost';
  lClient:=TInetSocket.Create('127.0.0.1',TestPort,lHandler);
  try
    lClient.IOTimeout:=ClientIOTimeout;
    lClient.Connect;
    lReader:=TReaderThread.Create(lClient);
    try
      try
        while (lSent<TotalBytes) and (lWriteReport='') do
          begin
          FillBlock(lBlock,lSent);
          if lClient.Write(lBlock,BlockSize)<>BlockSize then
            lWriteReport:=Format('client write failed after %d bytes: %s',[lSent,lHandler.GetLastErrorDescription])
          else
            Inc(lSent,BlockSize);
          end;
      except
        on E: Exception do
          lWriteReport:='client write exception '+E.ClassName+': '+E.Message;
      end;
      lReader.WaitFor;
      aServer.ClientFinished:=True;
      lWait:=0;
      while not aServer.Done and (lWait<RunTimeoutMs) do
        begin
        Sleep(10);
        Inc(lWait,10);
        end;
      aLine:=Format('client sent %d, received %d (%d bad) | server sent %d, received %d (%d bad), keyupdates %d',
        [lSent,lReader.Received.Offset,lReader.Received.Bad,aServer.Sent,aServer.Received.Offset,
         aServer.Received.Bad,aServer.KeyUpdates]);
      Result:=(lSent=TotalBytes) and (lReader.Received.Offset=TotalBytes) and (lReader.Received.Bad=0)
        and (aServer.Received.Offset=TotalBytes) and (aServer.Received.Bad=0) and aServer.Done
        and (aServer.KeyUpdates>0);
      if lWriteReport<>'' then
        aLine:=aLine+' | '+lWriteReport;
      if lReader.Report<>'' then
        aLine:=aLine+' | '+lReader.Report;
      if aServer.Report<>'' then
        aLine:=aLine+' | '+aServer.Report;
      if not aServer.Done then
        aLine:=aLine+' | server did not finish';
    finally
      lReader.Free;
    end;
  finally
    lClient.Free;
  end;
  // Waits until the server has ended the connection
  lWait:=0;
  while not aServer.Done and (lWait<RunTimeoutMs) do
    begin
    Sleep(10);
    Inc(lWait,10);
    end;
end;


var
  lServer: TKeyUpdateServer;
  lRuns, lFailed, I: Integer;
  lLine, lMode: String;

begin
  lRuns:=StrToIntDef(ParamStr(1),3);
  lMode:=LowerCase(ParamStr(2));
  if lMode='' then
    lMode:='duplex';
  if lMode='duplex' then
    gClientHandlerClass:=TOpenSSLDuplexSocketHandler
  else if lMode='base' then
    gClientHandlerClass:=TOpenSSLSocketHandler
  else
    begin
    Writeln('usage: duplexcheck [runs] [duplex|base]');
    Halt(2);
    end;
  if not InitSSLInterface then
    begin
    Writeln('SKIP: OpenSSL could not be loaded');
    Halt(0);
    end;
  Pointer(gKeyUpdate):=GetProcAddress(SSLLibHandle,'SSL_key_update');
  if not Assigned(gKeyUpdate) then
    begin
    Writeln('SKIP: OpenSSL has no SSL_key_update (needs 1.1.1 or later)');
    Halt(0);
    end;
  Writeln('client handler: ',gClientHandlerClass.ClassName,', ',TotalBytes,' bytes each way, KeyUpdate every ',
    KeyUpdateEvery,' server blocks');
  lServer:=TKeyUpdateServer.Create;
  try
    Sleep(200);
    lFailed:=0;
    for I:=1 to lRuns do
      begin
      if not RunOnce(lServer,lLine) then
        Inc(lFailed);
      Writeln(Format('run %d: %s',[I,lLine]));
      end;
  finally
    lServer.Free;
  end;
  if lFailed=0 then
    Writeln('PASS')
  else
    Writeln(Format('FAIL: %d of %d runs failed',[lFailed,lRuns]));
  if lFailed<>0 then
    ExitCode:=1;
end.
