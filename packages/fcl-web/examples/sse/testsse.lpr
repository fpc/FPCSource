program testsse;
{$ifdef mswindows}
{$apptype console}
{$endif}
uses sysutils, custApp, httpdefs, fphttpclient, httproute, fphttpserver;

Type

  { TEventSourceApp }

  TEventSourceApp = class(TCustomApplication)
  private
    FPort : Integer;
    FSource: TCustomHTTPEventSource;
    procedure DoHTTPRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    procedure DoServerEvents(ARequest: TRequest; AResponse: TResponse);
    procedure ReadEvents(aSender: TObject; aSource: TCustomHTTPEventSource);
    procedure RunEventLoop;
  Protected
    Procedure StartServer;
    procedure StartClient;
    Procedure Usage(const aMsg : string);
    procedure DoRun; override;
  end;

procedure TEventSourceApp.DoServerEvents(ARequest: TRequest; AResponse: TResponse);
var
  lEvent : THTTPServerEvent;
  i,lStartID : Integer;
  lAccept : String;
begin
  lAccept:=aRequest.Accept;
  if Pos('text/event-stream',lAccept)<>0 then
    begin
    aResponse.StartServerEvents;
    LStartID:=StrToIntDef(aRequest.CustomHeaders.Values['Last-Event-ID'],0);
    for I:=lStartID+1 to lStartID+10 do
      begin
      lEvent:=Default(THTTPServerEvent);
      lEvent.Data:=['Ping event '+IntToStr(I)];
      lEvent.Event:='ping';
      lEvent.ID:=IntToStr(i);
      aResponse.SendServerEvent(lEvent);
      sleep(500);
      end;
    end;

end;

procedure TEventSourceApp.DoHTTPRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  HTTPRouter.RouteRequest(aRequest,aResponse);
end;

procedure TEventSourceApp.ReadEvents(aSender: TObject; aSource: TCustomHTTPEventSource);
begin
  FSource:=aSource;
end;

Procedure TEventSourceApp.RunEventLoop;
var
  lEvent : THTTPServerEvent;
begin
  while not FSource.EOF do
    begin
    if FSource.ReadEvent(lEvent) then
       begin
       Write('Got event');
       if lEvent.Event<>'' then
         Write(' of type: ',lEvent.Event);
       if lEvent.Event<>'' then
         Write(' with id: ',lEvent.Id);
       Writeln('');
       Writeln(lEvent.Data[0]);
       Writeln('');
       end;
    end;
  FSource.Free;
  Terminate;
end;

procedure TEventSourceApp.StartServer;

var
  lServer : TFPHttpServer;
begin
  Writeln('Starting server, listening for requests on port ',FPort);
  Writeln('Send requests to http://localhost:',FPort,'/events');
  HTTPRouter.RegisterRoute('/events',rmAll,@DoServerEvents);
  lServer:=TFPHttpServer.Create(Self);
  try
    lServer.OnRequest:=@DoHTTPRequest;
    lServer.Port:=FPort;
    lServer.Active:=True;
  finally
    lServer.Free;
  end;
end;

Procedure TEventSourceApp.StartClient;

var
  lHTTP : TFPHTTPClient;
  lLast : Integer;
  lUrl,lResult : string;
begin
  lHTTP:=TFPHTTPClient.Create(Self);
  lUrl:=GetOptionValue('u','url');
  if lUrl='' then
    lUrl:=Format('http://localhost:%d/events',[FPort]);
  // lHTTP.OnEventStream:=@ReadEvents;
  lHTTP.AddHeader('Accept','text/event-stream');
  lLast:=StrToIntDef(GetOptionValue('l','last-id'),0);
  if lLast<>0 then
    lHTTP.AddHeader('Last-Event-Id',IntToStr(lLast));
  FSource:=lHTTP.GetEventSource('GET',lURL);
  // lResult:=lHTTP.Get(lUrl);
  if FSource=nil then
    Writeln('Get returned (Result=',lResult,')')
  else
    begin
    Writeln('Have event source. Listening for events...');
    RunEventLoop;
    end;
end;

Procedure TEventSourceApp.Usage(const aMsg : String);
begin
  if aMsg<>'' then
    Writeln('Error: ',aMsg);
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [options]');
  Writeln('Where options is one of :');
  Writeln('-h --help       This help');
  Writeln('-c --client     Run in client mode');
  Writeln('-p --port=PORT  Set server HTTP port number (both client and server)');
  Writeln('-l --last-id=ID Set last event ID (client)');
  Writeln('-s --server     Run in server mode');
  Writeln('-u --url:URL    Connect to this URL.');
  Writeln('                If not set, the default URL for testsse -r is used, taking -p in account');
  ExitCode:=Ord(aMsg<>'');
end;

Procedure TEventSourceApp.DoRun;

var
  Err : string;
begin
  Terminate;
  Err:=CheckOptions('chsp:l:u:',['client','help','server','port:','last-id:','url:']);
  if (Err<>'') or HasOption('h','help') then
    begin
    Usage(Err);
    exit;
    end;
  FPort:=StrToIntDef(GetOptionValue('p','port'),8080);
  if HasOption('s','server') then
    StartServer
  else if HasOption('c','client') then
    StartClient
  else
    Usage('Need run mode');
end;

begin
  CustomApplication:=TEventSourceApp.Create(Nil);
  CustomApplication.Initialize;
  CustomApplication.Run;
  CustomApplication.Free;
end.

