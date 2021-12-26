program httpupgrader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, jsonparser,fpmimetypes, fphttpserver, fpwebfile, httproute, wschat, wsupgrader, fpwebsocket, fpcustwsserver;

type

  { THTTPUpgradeApplication }

  THTTPUpgradeApplication = class(TCustomApplication)
  private
    procedure DoChatLog(Sender: TObject; const Msg: String);
    procedure DoRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  protected
    FServer : TFPHttpServer;
    FUpgrader : TWebsocketUpgrader;
    FChat: TWebsocketChat;
    procedure DoRun; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(Msg : string); virtual;
  end;

{ THTTPUpgradeApplication }

procedure THTTPUpgradeApplication.DoChatLog(Sender: TObject; const Msg: String);
begin
  Writeln(Msg);
end;

procedure THTTPUpgradeApplication.DoRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  HTTPRouter.RouteRequest(aRequest,aResponse);
end;

procedure THTTPUpgradeApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hH:d:p:r:st:', ['help','host:','directory:','port:','resource:','ssl','thread:']);
  if (ErrorMsg<>'') or HasOption('h', 'help') then
    begin
    Usage(ErrorMsg);
    Terminate;
    Exit;
    end;
  TSimpleFileModule.BaseDir:=GetOptionValue('d','directory');
  TSimpleFileModule.IndexPageName:='index.html';
  TSimpleFileModule.RegisterDefaultRoute;
  MimeTypes.LoadKnownTypes;
  FServer.UseSSL:=Hasoption('s','ssl');
  FServer.Port:=StrToIntDef(GetOptionValue('p','port'),3030);
  FUpgrader.Host:=GetOptionValue('H','host');
  FUpgrader.Resource:=GetOptionValue('r','resource');
  Case LowerCase(getOptionValue('t','thread')) of
    'none' :
       begin
       FServer.ThreadMode:=tmNone;
       FUpgrader.ThreadMode:=wtmNone;
       end;
    'pool' :
       begin
       FServer.ThreadMode:=tmThreadPool;
       FUpgrader.ThreadMode:=wtmThreadPool;
       end;
  else
    FServer.ThreadMode:=tmThread;
    FUpgrader.ThreadMode:=wtmThread;
  end;
  FUpgrader.Active:=True;
  FServer.Active:=True;
  Terminate;
end;

constructor THTTPUpgradeApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FServer:=TFPHttpServer.Create(Self);
  FServer.OnRequest:=@DoRequest;
  FUpgrader:=TWebsocketUpgrader.Create(Self);
  FUpgrader.WebServer:=FServer;
  FUpgrader.name:='UWebSocket';
  FChat:=TWebsocketChat.Create(Self);
  FChat.WebsocketServer:=FUpgrader;
  FChat.OnLog:=@DoChatLog;
  // Must do this here, because the events are protected
  FUpgrader.OnMessageReceived:=@FChat.DoMessageReceived;
  FUpgrader.OnControlReceived:=@FChat.DoControlReceived;
  FUpgrader.OnDisconnect:=@FChat.DoDisconnect;
end;

destructor THTTPUpgradeApplication.Destroy;
begin
  FreeAndNil(FUpgrader);
  FreeAndNil(FServer);
  inherited Destroy;
end;

procedure THTTPUpgradeApplication.Usage(Msg: string);
begin
  if Msg<>'' then
    Writeln('Error: ',Msg);
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where [options] is one or more of:');
  Writeln('-d --directory=DIRECTORY  Directory to serve files from');
  Writeln('-h --help                 This message');
  Writeln('-H --host=HOST            The hostname to use for accepting websockets.');
  Writeln('-p --port=PORT            Port nr to listen on');
  Writeln('-r --resource=PATH        Resource to use for accepting websockets');
  Writeln('-s --ssl                  Use SSL');
  Writeln('-t --thread=MODEL         Threading model to use (one of: none,thread,pool)');
end;

var
  Application: THTTPUpgradeApplication;
begin
  Application:=THTTPUpgradeApplication.Create(nil);
  Application.Title:='HTTP Server & Websocket chat Application';
  Application.Run;
  Application.Free;
end.

