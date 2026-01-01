program PostManEcho;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  opensslsockets,
  fpwebsocketclient,
  fpwebsocket;

type
  TWebSocketDemo = class
  private
    FClient: TWebsocketClient;
    FConnected: Boolean;

    procedure HandleConnect(Sender: TObject);
    procedure HandleDisconnect(Sender: TObject);
    procedure HandleMessageReceived(Sender: TObject; const AMessage: TWSMessage);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect(const AHost: string; APort: Integer; const AResource: string);
    procedure Disconnect;
    procedure Send(const AMessage: string);
    procedure WaitForMessages(ASeconds: Integer);
  end;

{ TWebSocketDemo }

constructor TWebSocketDemo.Create;
var
  LMessagePump: TWSThreadMessagePump;
begin
  inherited Create;
  FConnected := False;

  // Create message pump for async message handling
  LMessagePump := TWSThreadMessagePump.Create(nil);
  LMessagePump.Interval := 50; // Check every 50ms

  // Create client
  FClient := TWebsocketClient.Create(nil);
  FClient.MessagePump := LMessagePump;
  FClient.ConnectTimeout := 10000;
  FClient.CheckTimeOut := 100;

  // Wire events
  FClient.OnConnect := @HandleConnect;
  FClient.OnDisconnect := @HandleDisconnect;
  FClient.OnMessageReceived := @HandleMessageReceived;
end;

destructor TWebSocketDemo.Destroy;
begin
  WriteLn('Destroying WebSocketDemo...');

  // Disconnect if still connected
  if FConnected then
    Disconnect;

  // Free message pump first
  if Assigned(FClient.MessagePump) then
  begin
    WriteLn('  Freeing message pump...');
    FClient.MessagePump.Free;
  end;

  // Free client
  WriteLn('  Freeing client...');
  FClient.Free;

  WriteLn('  Done.');
  inherited;
end;

procedure TWebSocketDemo.HandleConnect(Sender: TObject);
begin
  FConnected := True;
  WriteLn('Connected!');
end;

procedure TWebSocketDemo.HandleDisconnect(Sender: TObject);
begin
  FConnected := False;
  WriteLn('Disconnected!');
end;

procedure TWebSocketDemo.HandleMessageReceived(Sender: TObject; const AMessage: TWSMessage);
begin
  if AMessage.IsText then
    WriteLn('Received: ', AMessage.AsString)
  else
    WriteLn('Received binary data: ', Length(AMessage.PayLoad), ' bytes');
end;

procedure TWebSocketDemo.Connect(const AHost: string; APort: Integer; const AResource: string);
begin
  WriteLn('Connecting to ', AHost, ':', APort, AResource);

  FClient.HostName := AHost;
  FClient.Port := APort;
  FClient.Resource := AResource;
  FClient.UseSSL := (APort = 443);

  // Start message pump
  if Assigned(FClient.MessagePump) then
  begin
    WriteLn('Starting message pump...');
    FClient.MessagePump.Execute;
  end;

  // Connect
  FClient.Connect;
end;

procedure TWebSocketDemo.Disconnect;
begin
  if not FClient.Active then
    Exit;

  WriteLn('Disconnecting...');

  // Stop message pump first
  if Assigned(FClient.MessagePump) then
  begin
    try
      FClient.MessagePump.Terminate;
    except
      // Ignore
    end;
  end;

  FClient.Disconnect;
end;

procedure TWebSocketDemo.WaitForMessages(ASeconds: Integer);
var
  I: Integer;
begin
  WriteLn('Waiting ', ASeconds, ' seconds for messages...');
  for I := 1 to ASeconds do
  begin
    Sleep(1000);
    Write('.');
  end;
  WriteLn;
end;

procedure TWebSocketDemo.Send(const AMessage: string);
begin
  WriteLn('Sending: ', AMessage);
  FClient.SendMessage(AMessage);
end;

var
  Demo: TWebSocketDemo;

begin
  WriteLn('=== Postman echo Demo ===');
  WriteLn;

  Demo := TWebSocketDemo.Create;
  try
    // Connect to a public WebSocket echo server
    // For testing, we'll just connect briefly
    try
      // Use a working echo server (wss://ws.postman-echo.com/raw)
      Demo.Connect('ws.postman-echo.com', 443, '/raw');

      // Wait for connection
      Sleep(2000);

      // Send a test message
      Demo.Send('Hello from FPC WebSocket Leak Demo!');

      // Wait for echo response
      Demo.WaitForMessages(3);
    except
      on E: Exception do
        WriteLn('Connection error (expected if server unavailable): ', E.Message);
    end;
  finally
    Demo.Free;
  end;

  WriteLn;
  WriteLn('Demo complete.');
end.
