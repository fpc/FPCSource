program serverapp;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Classes, SysUtils, enet, uenetclass, custapp;

type

  { TServerApplication }

  TServerApplication = class(TCustomApplication)
  Private
    myServer : TENetClass;
    FIdleCount : Int64;
    procedure ProcessLoop;
    procedure OnConnect(const Event:ENetEvent);
    procedure OnReceive(const Event:ENetEvent; var BroadcastMsg : Boolean; var BroadcastChannel : Byte);
    procedure ResetIdle;
    procedure StartServer(APort, ATimeout: Integer);
    procedure WriteHelp;
  protected
    Procedure DoRun; override;
  public
    constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    { public declarations }
  end;



constructor TServerApplication.Create(AOwner: TComponent);
begin
  Inherited;
  StopOnException:=True;
end;

procedure TServerApplication.StartServer(APort,ATimeout : Integer);

begin
  myServer := TENetClass.Create(APort,True);
  myServer.OnReceive:=@OnReceive;
  myServer.OnConnect:=@OnConnect;
  myServer.OnDisconnect:=@OnConnect;
  myServer.OnNone:=@OnConnect;
  myServer.MessageTimeout:=ATimeOut; // ideal for application idle?
  myServer.InitHost;
end;

procedure TServerApplication.WriteHelp;

begin
  Writeln('Usage ',ExtractFileName(Self.ExeName),' [options]');
  Writeln('Where options is one or more of');
  Writeln('-h --help          This message');
  Writeln('-p --port=portno   Port to listen on (default 30000)');
  Writeln('-t --timeout=msec  Timeout for listening to messages (milliseconds, default 1000)');
end;

procedure TServerApplication.DoRun;

Var
  T,P : Integer;

begin
  if HasOption('h','help') then
    begin
    WriteHelp;
    Terminate;
    exit;
    end;
  P:=StrToIntDef(GetoptionValue('p','port'),0);
  T:=StrToIntDef(GetoptionValue('t','timeout'),0);
  if P=0 then
    P:=30000;
  if t=0 then
    T:=1000; // 1 second timeout,
  StartServer(P,T);
  FIdleCount:=0;
  repeat
    ProcessLoop;
    if FIdleCount=0 then
      Write('[idle');
    Write('.');
    Inc(FIdleCount);
  until terminated;
end;

procedure TServerApplication.ProcessLoop;
begin
  myServer.ProcessMsg;
end;


destructor TServerApplication.Destroy;
begin
  myServer.Free;
end;

procedure TServerApplication.ResetIdle;

begin
  if FIdleCount>0 then
    Writeln(']');
  FIdleCount:=0;
end;

procedure TServerApplication.OnConnect(const Event: ENetEvent);
begin
  ResetIdle;
  if (Event.Kind=ENet_Event_type_Connect) then
    Writeln('Connect on channel : ',Event.channelID,' connected ID ,',Event.peer^.connectID,' (peer : ',Event.peer^.address.host,' port ',Event.peer^.address.port,')')
  else if Event.Kind=ENet_Event_type_DisConnect then
    Writeln('Disonnect on channel : ',Event.channelID,' connected ID ,',Event.peer^.connectID,' (peer : ',Event.peer^.address.host,' port ',Event.peer^.address.port,')')
  else
    Writeln('Unspecified connect event');
end;

procedure TServerApplication.OnReceive(const Event: ENetEvent; var BroadcastMsg: Boolean;
  var BroadcastChannel: Byte);
var
  msg : string;
begin
  ResetIdle;
  msg := PChar(Event.packet^.data);
  Writeln('Received message on channel : ',Event.channelID,' connected ID ,',Event.peer^.connectID,' (peer : ',Event.peer^.address.host,' port ',Event.peer^.address.port,')');
  Writeln('Message reads : "',Msg,'"');
end;

Var
  Application : TServerApplication;

begin
  Application:=TServerApplication.Create(Nil);
  try
    Application.Initialize;
    Application.Run;
  finally
    Application.Free;
  end;
end.

