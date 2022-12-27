{
  Sample program by Andrew Haines to test echo clients. 
}
program echoclient;

// Define this if you want to test the woSendErrClosesConn flag.

{ $DEFINE USE_NEW_CLOSE_FLAG}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  URIParser,
  fpwebsocket,
  fpwebsocketclient,
  opensslsockets,
  sysutils;

const
  addresses: array[0..4] of String = (
             'ws://127.0.0.1:8080/',
             'ws://ws.vi-server.org:80/mirror',
             'wss://ws.vi-server.org:443/mirror',
             'ws://vi-server.org:1939/',
             '' // <-- used for paramStr(1) if present
  );

type

  { TRunner }

  TRunner = class(TComponent)
  private type
    TStdInThread = class(TThread)
    protected
      FRunner: TRunner;
      StrToSend: String;
      procedure Execute; override;
    public
      constructor Create(ARunner: TRunner);
    end;
  private
    FQuit: Boolean;
    FPump: TWSThreadMessagePump;
    FClient: TWebsocketClient;
    FUri: String;
    FStdInRead: TStdInThread;
    procedure DoConnect(Sender: TObject);
    procedure DoControl(Sender: TObject; aType: TFrameType; const aData: TBytes      );
    procedure DoDisconnect(Sender: TObject);
    procedure DoHandShakeResponse(Sender: TObject;
      aResponse: TWSHandShakeResponse; var aAllow: Boolean);
    procedure DoMessage(Sender: TObject; const aMessage: TWSMessage);
    procedure DoPumpError(Sender: TObject; E: Exception);
    procedure DoSendHandshake(Sender: TObject; aHeaders: TStrings);
    procedure SyncReadLine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function GetAddress: String;
var
  i: Integer;
  lRead: String;
  lHappy: Boolean = False;
  lNum: Longint;
  lDefault: Integer = 0;
begin
  if ParamCount > 0 then
  begin
    lDefault := High(addresses);
    addresses[lDefault] := ParamStr(1);
  end;
  while not lHappy do
  begin
    WriteLn('Type an address or select a server number. "Q" quits');
    for i := 0 to High(addresses) do
      if addresses[i] <> '' then
        WriteLn(Format('   %d) %s',[i, addresses[i]]));
    Write(Format('Leave empty for default [%d] :', [lDefault]));

    lRead := '';
    ReadLn(lRead);
    if lRead = 'Q' then
    begin
      Result := '';
      lHappy := True;
    end
    else if lRead = '' then
    begin
      lHappy:=True;
      Result := addresses[lDefault];
    end
    else if TryStrToInt(lRead, lNum)
         and ((lNum in [0..High(addresses)-1]) or
             (lNum = High(addresses)) and (addresses[High(addresses)] <> '')) then
    begin
      Result := addresses[lNum];
      lHappy := True;
    end
    else if pos('ws', lRead) = 1 then
    begin
      Result := lRead;
      lHappy := True;
    end
    else
    begin
      WriteLn('Invalid address. Must be ws[s]://<site>[:port]/');
      lHappy := False;
    end;
  end;
end;

{ TRunner.TReadThread }

procedure TRunner.TStdInThread.Execute;
var
  lRead: String;
begin
  while not Terminated do
  begin
    Write('Enter text to send. "Q" quits. : ');
    ReadLn(lRead);
    StrToSend:=Copy(lRead, 1, MaxInt);
    Synchronize(@FRunner.SyncReadLine);
    Sleep(500); // only for prettier text lines if we get a reply
  end;
end;

constructor TRunner.TStdInThread.Create(ARunner: TRunner);
begin
  FRunner := ARunner;
  inherited Create(False);
end;

{ TRunner }

procedure TRunner.DoDisconnect(Sender: TObject);
begin
  WriteLn(' -- Disconnect --');
end;

procedure TRunner.DoHandShakeResponse(Sender: TObject;
  aResponse: TWSHandShakeResponse; var aAllow: Boolean);
begin
  aAllow:=aAllow; // <-- silence compiler error
  WriteLn(' -- Handshake Response -- ');
  WriteLn(aResponse.RawHeaders.Text);
end;

procedure TRunner.DoMessage(Sender: TObject; const aMessage: TWSMessage);
begin
  if aMessage.IsText then
  begin
    WriteLn('<<< ', aMessage.AsString);
  end;
end;

procedure TRunner.DoPumpError(Sender: TObject; E: Exception);
begin
  WriteLn(' -- Pump Error: ', E.Message);
  Fclient.Active := False;
  FQuit:=True;
end;

procedure TRunner.DoSendHandshake(Sender: TObject; aHeaders: TStrings);
begin
  {
  aHeaders.Values['Accept-Encoding'] := '';
  aHeaders.Values['Origin'] := '';
  aHeaders.Values['Cache-Control'] := 'no-cache';
  aHeaders.Values['Pragma'] := 'no-cache';
  aHeaders.Values['Sec-WebSocket-Extensions'] := 'client_max_window_bits';
  aHeaders.Values['User-Agent'] := 'My Special Websocket';
  }

  WriteLn(' -- Sending Handshake ... ---');
  WriteLn(aHeaders.Text);
end;

procedure TRunner.DoConnect(Sender: TObject);
begin
  WriteLn(Format(' -- Connected to %s --', [FUri]));
end;

procedure TRunner.DoControl(Sender: TObject; aType: TFrameType;
  const aData: TBytes);
begin
  WriteLn;
  WriteLn(' -- Control message: ', aType);
  case aType of
    ftPing:
      begin
        if not (woPongExplicit in FClient.Options) then
          WriteLn(' -- Pong was sent implicitly ---')
        else
        begin
          WriteLn(' -- Sending Pong Explicitly --- ');
          FClient.Pong(Copy(PChar(@aData[0]), 0, Length(aData)));
        end;
      end;
    ftClose:
      begin
        WriteLn(' -- Close recieved --- ');
        WriteLn(' -- Reason: ', TEncoding.UTF8.GetAnsiString(aData));
        FClient.Active := False;
      end
  else
    // the compiler is quiet
  end;
  WriteLn;

end;

procedure TRunner.SyncReadLine;
begin
  if (FStdInRead.StrToSend = 'Q') or (not FClient.Active) then
  begin
    if not FClient.Active then
      WriteLn(' --- Client is not active. Quitting ---')
    else
      WriteLn(' --- Quitting ---');
    FStdInRead.Terminate;
    FClient.Active:=False;
    FQuit := True;
    Exit;
  end;
  WriteLn('>>> ', FStdInRead.StrToSend);

  FClient.SendMessage(FStdInRead.StrToSend);
end;

constructor TRunner.Create(AOwner: TComponent);
var
  lURI: TURI;
begin
  inherited Create(AOwner);
  FPump := TWSThreadMessagePump.Create(Self);
  FPump.OnError:=@DoPumpError;
  FPump.Execute;

  FClient := TWebsocketClient.Create(Self);
  // Clients must have a mask defined
  FClient.OutGoingFrameMask:=Random(MaxInt);
  FClient.OnConnect:=@DoConnect;
  FCLient.OnControl:=@DoControl;
  FClient.OnDisconnect:=@DoDisconnect;
  FClient.OnMessageReceived:=@DoMessage;
  FClient.OnSendHandShake:=@DoSendHandshake;
  Fclient.OnHandshakeResponse:=@DoHandShakeResponse;
  FClient.MessagePump := FPump;
  {$IFDEF USE_NEW_CLOSE_FLAG}
  FClient.Options:=[woSendErrClosesConn];
  {$ENDIF}

  FUri :=  GetAddress;
  if FUri = '' then
  begin
    FQuit:=True;
    Exit;
  end;
  lUri := ParseURI(FUri);
  if (lURI.Port = 0) then
  begin
    if lURI.Protocol = 'ws' then
      lURI.Port:=80
    else
      lURI.Port:=443;
  end;

  FClient.HostName:=lURI.Host;
  FClient.Port:=lURI.Port;
  FClient.Resource:=lURI.Path;
  FClient.UseSSL:=lURI.Protocol='wss';
  FClient.Active:=True;
  FStdInRead := TStdInThread.Create(Self);
end;

destructor TRunner.Destroy;
begin
  if Assigned(FStdInRead) then
  begin
    FStdInRead.Terminate;
    FStdInRead.WaitFor;
    FreeAndNil(FStdInRead);
  end;
  inherited Destroy;
end;

var
  Runner: TRunner;

begin
  Runner := TRunner.Create(nil);
  try
    while not Runner.FQuit do
      CheckSynchronize(5000);
  finally
    Runner.Free;
  end;
end.

