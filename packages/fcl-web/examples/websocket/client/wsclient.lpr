program wsclient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, jsonparser, fpJSON,SysUtils, StrUtils, CustApp, uriparser, httpprotocol, fpwebsocketclient, fpwebsocket;

type

  { TWebsocketClientApplication }

  TWebsocketClientApplication = class(TCustomApplication)
  private
    FUri : TUri;
    FLastRecipient : string;
    FAlias : String;
    FClient: TWebsocketClient;
    FPump : TWSMessagePump;
    FMsgCount : Integer;
    FUsePump : Boolean;
    procedure DoControl(Sender: TObject; aType: TFrameType; const aData: TBytes);
    procedure DoDisconnect(Sender: TObject);
    procedure DoIncomingMessage(Sender: TObject; const aMessage: TWSMessage);
    function SendMessage(const aTo, aLine: string): Boolean;
    procedure ShowHelp;
  Protected
    function AskAlias: String;
    function CheckMessages: boolean;
    function ConnectToServer: Boolean;
    function GetCommandOrMessage: Boolean;
    function ParseOptions: String;
    function QueryUser(Prompt: String; aDefault: String): String;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aError: string); virtual;
  end;

{ TWebsocketClientApplication }

function TWebsocketClientApplication.ParseOptions : String;

begin
  if not HasOption('u','url') then
    Exit('Need URL option');
  FUri:=ParseURI(GetOptionValue('u','url'));
  if IndexText(FURI.Protocol,['ws','wss'])<0 then
    Exit('Invalid protocol in uri: need one of ws,wss');
  if (FURI.Port=0) then
    FURI.Port:=8080;
  FAlias:=GetOptionValue('a','alias');
  FUsePump:=HasOption('p','pump');
end;

Function TWebsocketClientApplication.QueryUser(Prompt : String; aDefault : String) : String;

begin
  if aDefault<>'' then
    Prompt:=Prompt+' ['+aDefault+']';
  Write(Prompt+'> ');
  ReadLn(Result);
  if Result='' then
    Result:=aDefault;
end;

Function TWebsocketClientApplication.AskAlias : String;

begin
  Repeat
    Result:=QueryUser('Please give your alias for the chat','');
  Until (Result<>'');
end;

procedure TWebsocketClientApplication.DoRun;
var
  ErrorMsg: String;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hu:a:p', ['help','url','alias','pump']);
  if (ErrorMsg='') and not HasOption('h', 'help') then
    ErrorMsg:=ParseOptions;
  if (ErrorMsg<>'') or HasOption('h', 'help') then
    begin
    Usage(ErrorMsg);
    Terminate;
    Exit;
    end;
  if FAlias='' then
    FAlias:=AskAlias;
  if ConnectToServer then
    Writeln('Enter message or command (/stop /help), empty message will just check for incoming messages');
  SendMessage(FAlias,'Hello, this is a friendly greeting message from the client');
  CheckMessages;
  While not Terminated do
    begin
    GetCommandOrMessage;
    CheckMessages;
    end;
  Terminate;
end;

constructor TWebsocketClientApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FClient:=TWebsocketClient.Create(Self);
  FClient.OnDisconnect:=@DoDisconnect;
  FClient.OnMessageReceived:=@DoIncomingMessage;
  FClient.OnControl:=@DoControl;
end;

destructor TWebsocketClientApplication.Destroy;
begin
  FreeAndNil(FClient);
  inherited Destroy;
end;

procedure TWebsocketClientApplication.Usage(const aError : string);
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('where options is one or more of:');
  Writeln('-h  --help             this help text');
  Writeln('-u --url=URL           the URL to connect to. Mandatory');
  Writeln('-a --alias=nick        your nick name in the chat');
  Writeln('-p --pump              use message pump');
  ExitCode:=Ord(aError<>'');
end;

Function TWebsocketClientApplication.ConnectToServer : Boolean;

Var
  Res : string;

begin
  FClient.HostName:=FURI.Host;
  FClient.Port:=FURI.Port;
  Res:=FURI.Path;
  if (FURI.Document<>'') then
    Res:=IncludeHTTPPathDelimiter(Res)+FURI.Document;
  FClient.Resource:=Res;
  if FUsePump then
    begin
    FPump:=TWSThreadMessagePump.Create(Self);
    FPump.Interval:=50;
    FClient.MessagePump:=FPump;
    FPump.Execute;
    end;
  try
    FClient.Connect;
    Result:=True;
  except
    on E : Exception do
      begin
      ShowException(E);
      terminate;
      end;
  end;
end;

Procedure TWebsocketClientApplication.ShowHelp;

begin
  Writeln('Enter a command or a message text. Commands start with / and can be one of:');
  Writeln('/help  - this text');
  Writeln('/quit  - stop the program.');
  Writeln('/stop  - stop the program.');
  Writeln('/ping [ping text] - send a ping.');
  Writeln('/pong [pong text] - send a pong.');
end;

Function TWebsocketClientApplication.GetCommandOrMessage : Boolean;

Var
  aCmd,aLine,aTo : String;

begin
  aLine:=QueryUser(FAlias,'');
  Result:=aLine<>'';
  if not Result then
    exit;
  if Copy(aLine,1,1)='/' then
    begin
    aCmd:=ExtractWord(1,aLine,[' ']);
    System.Delete(aLine,1,length(aCmd)+1);
    aCmd:=Copy(aCmd,2,Length(aCmd)-1);
    case lowercase(aCmd) of
      'quit',
      'stop' :
        begin
        Result:=False;
        Terminate;
        end;
      'help':
        begin
        Result:=False;
        ShowHelp;
        end;
      'ping':
        begin
        FClient.Ping(aLine);
        end;
      'pong':
        begin
        FClient.Pong(aLine);
        end;
    end
    end
  else if (aLine<>'') then
    begin
    aTo:=QueryUser('Recipient',FLastRecipient);
    if (aTo<>'*') and (aTo<>'') then
      FLastRecipient:=aTo;
    if aTo='*' then
      aTo:='';
    SendMessage(aTo,aLine)
    end;
end;

Function TWebsocketClientApplication.SendMessage(const aTo,aLine : string) : Boolean;

Var
  aJSON : TJSONObject;
  Msg : String;

begin
  Result:=False;
  aJSON:=TJSONObject.Create(['from',FAlias,'msg',aLine,'to',aTo]);
  try
    Msg:=aJSON.asJSON;
    try
      FClient.SendMessage(msg);
      Result:=True;
    except
      on E : Exception do
        ShowException(E);
    end;
  finally
    aJSON.Free;
  end;
end;

procedure TWebsocketClientApplication.DoControl(Sender: TObject; aType: TFrameType; const aData: TBytes);

var
  aReason : String;
  aCode : Integer;

begin
  inc(fMsgCount);
  Case aType of
  ftClose:
    begin
    aCode:=TWSConnection(Sender).GetCloseData(aData,aReason);
    Writeln('Close code ',aCode,' received with readon: ',aReason);
    end;
  ftPing:
    begin
    Writeln('Ping received');
    end;
  ftPong:
    begin
    Writeln('Pong received');
    end;
  else
    Writeln('Unknown control code: ',aType);
  end;
end;

procedure TWebsocketClientApplication.DoDisconnect(Sender: TObject);
begin
  Writeln('Connection closed, terminating');
  Terminate;
end;

procedure TWebsocketClientApplication.DoIncomingMessage(Sender: TObject; const aMessage: TWSMessage);
Var
  S,From,Recip : String;
  D : TJSONData;
  Msg : TJSONObject absolute D;

begin
  inc(fMsgCount);
  if not aMessage.IsText then
    begin
    Writeln('Incoming message is not text');
    exit;
    end;
  S:=aMessage.AsString;
  try
    D:=GetJSON(S);
    try
      if Not (D is TJSONOBject) then
        Raise EJSON.Create('Not an object: '+S);
      From:=Msg.Get('from','');
      Recip:=Msg.Get('to','');
      Write('From <',From,'>');
      if SameText(Recip,FAlias) then
        Writeln(' to you:')
      else
        Writeln(' to all:');
      Writeln(Msg.Get('msg',''));
    finally
      FreeAndNil(D)
    end;
  except
    Writeln('Incoming message is not valid JSON: ',S);
  end;
end;

Function TWebsocketClientApplication.CheckMessages: boolean;

begin
  FMsgCount:=0;
  if FUsePump then
    CheckSynchronize()
  else
    while FClient.CheckIncoming=irOK do
      ;

  Result:=(FMsgCount>0);
end;


var
  Application: TWebsocketClientApplication;
begin
  Application:=TWebsocketClientApplication.Create(nil);
  Application.Title:='Websocket Client Application';
  Application.Run;
  Application.Free;
end.

