program h2stop;

{ Stops the HTTP/2 handler of a TFPHttpServer while an idle h2c connection is open (issue #42059).
  Build with -gh, run with HEAPTRC=keepreleased; expect PASS, no "changed after free" and 0 unfreed blocks.
  usage: h2stop [hold|eof, default hold] [port, default 28471] }

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, DateUtils, ssockets, fphttpserver, fphttp2server, httpdefs;

const
  StopLimitMs = 2000;
  ClientMaxSeconds = 15;

type
  TServerThread = class(TThread)
  private
    FServer: TFPHttpServer;
    FError: String;
  public
    // Starts a thread that runs aServer until it is deactivated
    constructor Create(aServer: TFPHttpServer);
    // Sets the server active and records an exception message
    procedure Execute; override;
    // Exception message raised by Active:=True, empty when none
    property Error: String read FError;
  end;

  TClientThread = class(TThread)
  private
    FPort: Word;
    FCloseOnEOF: Boolean;
    FRelease: Boolean;
    FConnected: Boolean;
    FBytes: Integer;
    FSawEOF: Boolean;
    FError: String;
  public
    // Starts a client for 127.0.0.1:aPort; with aCloseOnEOF it ends as soon as the server closes
    constructor Create(aPort: Word; aCloseOnEOF: Boolean);
    // Connects, sends the HTTP/2 preface and an empty SETTINGS frame, then reads until released or EOF
    procedure Execute; override;
    // Lets a holding client close its connection and end
    procedure ReleaseConnection;
    // True once the preface has been sent
    property Connected: Boolean read FConnected;
    // Number of bytes received from the server
    property Bytes: Integer read FBytes;
    // True if the server closed or shut down the connection
    property SawEOF: Boolean read FSawEOF;
    // Exception message of the client, empty when none
    property Error: String read FError;
  end;


constructor TServerThread.Create(aServer: TFPHttpServer);

begin
  FServer:=aServer;
  inherited Create(False);
end;


procedure TServerThread.Execute;

begin
  try
    FServer.Active:=True;
  except
    on E: Exception do
      FError:=E.ClassName+': '+E.Message;
  end;
end;


constructor TClientThread.Create(aPort: Word; aCloseOnEOF: Boolean);

begin
  FPort:=aPort;
  FCloseOnEOF:=aCloseOnEOF;
  inherited Create(False);
end;


procedure TClientThread.ReleaseConnection;

begin
  FRelease:=True;
end;


procedure TClientThread.Execute;

const
  cPreface: AnsiString = 'PRI * HTTP/2.0'#13#10#13#10'SM'#13#10#13#10;
  cEmptySettings: array[0..8] of Byte = (0, 0, 0, 4, 0, 0, 0, 0, 0);

var
  lSocket: TInetSocket;
  lBuffer: array[0..4095] of Byte;
  lStart: TDateTime;
  lCount, lTry: Integer;

begin
  lSocket:=nil;
  try
    lTry:=0;
    repeat
      try
        lSocket:=TInetSocket.Create('127.0.0.1',FPort);
      except
        on E: ESocketError do
          begin
          Inc(lTry);
          if lTry>=50 then
            raise;
          Sleep(100);
          end;
      end;
    until Assigned(lSocket);
    lSocket.WriteBuffer(cPreface[1],Length(cPreface));
    lSocket.WriteBuffer(cEmptySettings,SizeOf(cEmptySettings));
    FConnected:=True;
    lStart:=Now;
    while not FRelease and (SecondsBetween(Now,lStart)<ClientMaxSeconds) do
      begin
      if FSawEOF then
        begin
        if FCloseOnEOF then
          Break;
        Sleep(100);
        end
      else if lSocket.CanRead(100) then
        begin
        lCount:=lSocket.Read(lBuffer,SizeOf(lBuffer));
        if lCount<=0 then
          FSawEOF:=True
        else
          Inc(FBytes,lCount);
        end;
      end;
  except
    on E: Exception do
      FError:=E.ClassName+': '+E.Message;
  end;
  lSocket.Free;
end;


// Writes aMsg and flushes Output
procedure Log(const aMsg: String);

begin
  Writeln(aMsg);
  Flush(Output);
end;


var
  lServer: TFPHttpServer;
  lHandler: TFPHTTP2Handler;
  lServerThread: TServerThread;
  lClient: TClientThread;
  lPort: Word;
  lHold: Boolean;
  lStart: TDateTime;
  lStopMs: Int64;
  lLive, lWait: Integer;
  lOK: Boolean;

begin
  lHold:=not SameText(ParamStr(1),'eof');
  lPort:=StrToIntDef(ParamStr(2),28471);
  lServer:=TFPHttpServer.Create(nil);
  lServer.Port:=lPort;
  lServer.Address:='127.0.0.1';
  lServer.ThreadMode:=tmThread;
  lHandler:=TFPHTTP2Handler.Create(nil);
  lHandler.WebServer:=lServer;
  lHandler.Active:=True;
  lServerThread:=TServerThread.Create(lServer);
  if lHold then
    Log('client mode: hold (keeps the connection open)')
  else
    Log('client mode: eof (closes when the server closes)');
  lClient:=TClientThread.Create(lPort,not lHold);
  lWait:=0;
  while (lHandler.ConnectionCount=0) and (lWait<100) and not lClient.Finished and not lServerThread.Finished do
    begin
    Sleep(100);
    Inc(lWait);
    end;
  Sleep(200);
  lLive:=lHandler.ConnectionCount;
  Log('live HTTP/2 connections before stop: '+IntToStr(lLive));
  lStart:=Now;
  lHandler.Active:=False;
  lStopMs:=MilliSecondsBetween(Now,lStart);
  Log('h2 handler stopped after '+IntToStr(lStopMs)+' ms');
  Sleep(300);
  lClient.ReleaseConnection;
  lClient.WaitFor;
  Log('client: received '+IntToStr(lClient.Bytes)+' bytes, EOF seen: '+BoolToStr(lClient.SawEOF,True));
  if lClient.Error<>'' then
    Log('client error: '+lClient.Error);
  lServer.Active:=False;
  lServerThread.WaitFor;
  if lServerThread.Error<>'' then
    Log('server thread error: '+lServerThread.Error);
  lOK:=(lLive=1) and (lStopMs<StopLimitMs) and lClient.SawEOF and (lServerThread.Error='');
  lClient.Free;
  lServerThread.Free;
  lHandler.Free;
  lServer.Free;
  if lOK then
    Log('PASS, provided heaptrc below reports no "changed after free" and 0 unfreed memory blocks')
  else if lLive<>1 then
    Log('FAIL: no HTTP/2 connection was open at the stop, this run does not test the fix')
  else
    Log('FAIL: the stop took too long or the client was not shut down');
  if not lOK then
    ExitCode:=1;
end.
