{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Sample HTTP server application

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

{ $DEFINE USEGNUTLS}
{ $DEFINE USEMICROHTTP}

program simpleserver;

{$IFDEF USEMICROHTTP}
{$UNDEF USEGNUTLS}
{$ENDIF}

uses
{$IFDEF UNIX}
  cwstring,
  cthreads,
{$ENDIF}
{$IFNDEF USEMICROHTTP}
{$ifdef USEGNUTLS}
  gnutlssockets,
{$else}
  opensslsockets,
{$endif}
  custhttpapp,
{$ELSE}
  custmicrohttpapp,
{$ENDIF}
  {$ifdef unix}
  baseunix,
  {$endif}
  sysutils, Classes, jsonparser, fpjson, strutils, inifiles, sslbase, httproute, httpdefs, fpmimetypes, fpwebfile, fpwebproxy,
  webutil, fpdebugcapturesvc;

Const
  ServerVersion = '1.0';

Type

  { THTTPApplication }
{$IFDEF USEMICROHTTP}
  TParentApp = TCustomMicroHTTPApplication;
{$ELSE}
  TParentApp = TCustomHTTPApplication;
{$ENDIF}

  { TMySimpleFileModule }

  TMySimpleFileModule = class(TSimpleFileModule)
  Public
    Procedure SendFile(const AFileName: String; AResponse: TResponse); override;
  end;

  THTTPApplication = Class(TParentApp)
  private
    FAPISecret : String;
    FBaseDir: string;
    FIndexPageName: String;
    FInterfaceAddress: String;
    FMimeFile: String;
    FNoIndexPage: Boolean;
    FQuiet: Boolean;
    FBackground : Boolean;
    FPassword : string;
    FEcho : Boolean;
    FMaxAge : Integer;
    FCrossOriginIsolation : Boolean;
    procedure AddProxy(const aProxyDef: String);
    procedure DoEcho(ARequest: TRequest; AResponse: TResponse);
    procedure DoProxyLog(Sender: TObject; const aMethod, aLocation, aFromURL, aToURL: String);
    procedure DoQuit(ARequest: TRequest; AResponse: TResponse);
    procedure LoadMimeTypes;
    procedure ProcessOptions;
    procedure ReadConfigFile(const ConfigFile: string);
    procedure SetupCapture;
    procedure Usage(Msg: String);
    procedure Writeinfo;
  Public
    Destructor Destroy; override;
  published
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    Procedure DoRun; override;
    property Quiet : Boolean read FQuiet Write FQuiet;
    Property MimeFile : String Read FMimeFile Write FMimeFile;
    Property BaseDir : string Read FBaseDir Write FBaseDir;
    Property NoIndexPage : Boolean Read FNoIndexPage Write FNoIndexPage;
    Property IndexPageName : String Read FIndexPageName Write FIndexPageName;
    Property InterfaceAddress : String Read FInterfaceAddress Write FInterfaceAddress;
  end;

Var
  Application : THTTPApplication;

{ TMySimpleFileModule }

procedure TMySimpleFileModule.SendFile(const AFileName: String; AResponse: TResponse);
begin
  AResponse.SetCustomHeader('Cross-Origin-Embedder-Policy','require-corp');
  AResponse.SetCustomHeader('Cross-Origin-Opener-Policy','same-origin');
  inherited SendFile(AFileName, AResponse);
end;

{ THTTPApplication }

procedure THTTPApplication.DoEcho(ARequest: TRequest; AResponse: TResponse);

Var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    L.AddStrings(['<!doctype html>',
      '<html>',
      '<head>',
      '<title>Echo request</title>',
      '</head>',
      '<body>'
    ]);
    DumpRequest(aRequest,L);
    L.AddStrings(['</body>','</html>']);
    AResponse.Content:=L.Text;
    AResponse.ContentLength:=Length(AResponse.Content);
    AResponse.SendResponse;
  finally
    L.Free;
  end;
end;


procedure THTTPApplication.Doquit(ARequest: TRequest; AResponse: TResponse);

Var
  PWD : String;

begin
  PWD:=ARequest.QueryFields.Values['password'];
  if PWD='' then
    ARequest.ContentFields.Values['password'];
  if PWD=FPassword then
    begin
    AResponse.Content:='OK';
    AResponse.SendContent;
    Terminate;
    end
  else
    begin
    AResponse.Code:=403;
    AResponse.CodeText:='Forbidden';
    AResponse.SendContent;
    end;
end;

procedure THTTPApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if Quiet then
    exit;
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
  else
    inherited DoLog(EventType, Msg);
end;

procedure THTTPApplication.DoProxyLog(Sender: TObject; const aMethod, aLocation, aFromURL, aToURL: String);

Var
  Msg : String;

begin
  if Quiet then
    exit;
  Msg:=Format('(Proxy redirect) location: %s, Method: %s, From: %s, to: %s',[aLocation,aMethod,aFromURl,atoURL]);
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',etInfo,'] ',Msg)
  else
    inherited DoLog(etInfo, Msg);
end;

procedure THTTPApplication.Usage(Msg : String);

begin
  if (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] ');
  Writeln('Version : ',ServerVersion);
  Writeln('Where options is one or more of : ');
  Writeln('-A --api=path,secret  Activate location API on path, using secret as accepted bearer token.');
  Writeln('-a --max-age=age      Set max-age expiry header on returned file requests.');
  Writeln('-c --config=file      Ini configuration file (default: simpleserver.ini)');
{$ifdef unix}
  Writeln('-b --background       fork to background');
{$endif}
  Writeln('-d --directory=dir    Base directory from which to serve files.');
  Writeln('                      Default is current working directory: ',GetCurrentDir);
  Writeln('-e --echo             Activate /echo URL.');
  Writeln('-h --help             This help text');
  Writeln('-H --hostname=NAME    Set hostname for self-signed SSL certificate');
  Writeln('-i --indexpage=name   Directory index page to use (default: index.html)');
  Writeln('-I --interface=IP     Listen on this interface address only.');
  Writeln('-m --mimetypes=file   Path of mime.types. Loaded in addition to OS known types');
  Writeln('-n --noindexpage      Do not allow index page.');
  Writeln('-o --coi              Enable Cross-Origin Isolation headers');
  Writeln('-p --port=NNNN        TCP/IP port to listen on (default is 3000)');
  Writeln('-q --quiet            Do not write diagnostic messages');
  Writeln('-Q --quit=PWD         Register /quit URL. Send request with password variable equal to PWD to stop');
  Writeln('-s --ssl              Use SSL');
  Writeln('-u --capture[=FILE]   Set up /debugcapture route to capture output sent by browser.');
  Writeln('                      If FILE is specified, write to file. If not specified, writes to STDOUT.');
  Writeln('-V --version          Display server version and exit');         
  Writeln('-x --proxy=proxydef   Add proxy definition. Definition is of form:');
  Writeln('                      name:BaseURL');
  Writeln('');
  Writeln('Config file is ini file, section [Server]. Key names are long option names');
  Writeln('Proxies are defined in section [Proxy], Key is name, value is URL');
  Writeln('Locations are defined in section [Locations], Key is location name, value is path');
  Halt(Ord(Msg<>''));
end;

procedure THTTPApplication.LoadMimeTypes;

begin
  MimeTypes.LoadKnownTypes;
  if MimeFile<>'' then
    begin
    MimeTypesFile:=MimeFile;
    if (MimeTypesFile<>'') and not FileExists(MimeTypesFile) then
      begin
      Log(etWarning,'mimetypes file not found: '+MimeTypesFile);
      MimeTypesFile:='';
      end;
    end;
  If MimeTypesFile<>'' then
    MimeTypes.LoadFromFile(MimeTypesFile);  
end;

procedure THTTPApplication.AddProxy(const aProxyDef: String);

Var
  P : Integer;
  N,URL : String;

begin
  P:=Pos(':',aProxyDef);
  If P=0 then Raise
    EHTTP.CreateFmt('Invalid proxy definition: %s',[aProxyDef]);
  N:=Copy(aProxyDef,1,P-1);
  URL:=Copy(aProxyDef,P+1,Length(aProxyDef));
  ProxyManager.RegisterLocation(N,URL).AppendPathInfo:=True;
end;

Const
  SCaptureRoute = '/debugcapture';

Const
  SConfig  = 'Server';
  SProxy = 'Proxy';
  SLocations = 'Locations';

  KeyPort  = 'Port';
  KeyInterface = 'Interface';
  KeyDir   = 'Directory';
  KeyIndexPage = 'IndexPage';
  KeyHostName = 'hostname';
  keyMimetypes = 'mimetypes';
  KeySSL = 'SSL';
  KeyQuiet = 'quiet';
  KeyQuit = 'quit';
  KeyEcho = 'echo';
  KeyNoIndexPage = 'noindexpage';
  KeyBackground = 'background';
  KeyMaxAge = 'MaxAge';
  KeyAPI = 'API';
  KeyCOI = 'CrossOriginIsolation';
  KeyCapture = 'DebugCapture';

procedure THTTPApplication.ReadConfigFile(const ConfigFile: string);



Var
  L : TStringList;
  P,U : String;
  I : Integer;

begin
  if (ConfigFile='') or Not FileExists(ConfigFile) then exit;
  L:=Nil;
  With TMemIniFile.Create(ConfigFile) do
    try
      BaseDir:=ReadString(SConfig,KeyDir,BaseDir);
      Port:=ReadInteger(SConfig,KeyPort,Port);
      InterfaceAddress:=ReadString(SConfig,KeyInterface,InterfaceAddress);
      Quiet:=ReadBool(SConfig,KeyQuiet,Quiet);
      MimeFile:=ReadString(SConfig,keyMimetypes,MimeFile);
      NoIndexPage:=ReadBool(SConfig,KeyNoIndexPage,NoIndexPage);
      IndexPageName:=ReadString(SConfig,KeyIndexPage,IndexPageName);
      HostName:=ReadString(SConfig,KeyHostName,HostName);
      UseSSL:=ReadBool(SConfig,KeySSL,UseSSL);
      FBackground:=ReadBool(SConfig,Keybackground,FBackGround);
      FPassword:=ReadString(SConfig,KeyQuit,FPassword);
      FEcho:=ReadBool(SConfig,KeyEcho,FEcho);
      FMaxAge:=ReadInteger(SConfig,KeyMaxAge,FMaxAge);
      FAPISecret:=ReadString(SConfig,KeyAPI,'');
      FCrossOriginIsolation:=ReadBool(SConfig,KeyCOI,FCrossOriginIsolation);
      if ValueExists(SConfig,KeyCapture) then
        begin
        TDebugCaptureService.Instance.LogFileName:=ReadString(SConfig,keyCapture,'');
        end;
      L:=TstringList.Create;
      ReadSectionValues(SProxy,L,[]);
      For I:=0 to L.Count-1 do
        begin
        L.GetNameValue(I,P,U);
        if (P<>'') and (U<>'') then
          ProxyManager.RegisterLocation(P,U).AppendPathInfo:=true;
        end;
      L.Clear;
      ReadSectionValues(SLocations,L,[]);
      For I:=0 to L.Count-1 do
        begin
        L.GetNameValue(I,P,U);
        if (P<>'') and (U<>'') then
          RegisterFileLocation(P,U);
        end;
    finally
      L.Free;
      Free;
    end;
end;

procedure THTTPApplication.ProcessOptions;

  procedure HasGetOptionValue(var aValue: string; Const C: Char; Const S : String);
  begin
    if HasOption(C,S) then
      aValue:=GetOptionValue(C,S);
  end;

Var
  S : String;

begin
  for S in GetOptionValues('x','proxy') do
    AddProxy(S);
  HasGetOptionValue(FAPISecret,'A','api');
  if HasOption('e','echo') then
    FEcho:=true;
  if HasOption('q','quiet') then
    Quiet:=true;
  HasGetOptionValue(FPassword,'Q','quit');
  if HasOption('p','port') then
    Port:=StrToIntDef(GetOptionValue('p','port'),Port);
  LoadMimeTypes;
  if HasOption('d','directory') then
    BaseDir:=GetOptionValue('d','directory');
  UseSSL:=HasOption('s','ssl');
  if HasOption('H','hostname') then
    HostName:=GetOptionValue('H','hostname');
  if HasOption('n','noindexpage') then
    NoIndexPage:=True
  else
    IndexPageName:=GetOptionValue('i','indexpage');
  if HasOption('I','interface') then
    InterfaceAddress:=GetOptionValue('I','interface');
  if HasOption('a','max-age') then
    FMaxAge:=StrToIntDef(GetOptionValue('a','max-age'),FMaxAge);
  if HasOption('b','background') then
    FBackground:=true;
  if hasOption('o','coi') then
    FCrossOriginIsolation:=true;
  if HasOption('u','capture') then
    begin
    S:=GetOptionValue('u','capture');
    if S='' then
      TDebugCaptureService.Instance.LogToConsole:=True
    else
      TDebugCaptureService.Instance.LogFileName:=S;
    end;
end;

procedure THTTPApplication.Writeinfo;

Var
  I : Integer;

begin
  Log(etInfo,'Listening on port %d, serving files from directory: %s (using SSL: %s)',[Port,BaseDir,BoolToStr(UseSSL,'true','false')]);
  For I:=0 to ProxyManager.LocationCount-1 do
    with ProxyManager.Locations[i] do
      Log(etInfo,'Proxy location /proxy/%s redirects to %s',[Path,URL]);
  if not NoIndexPage then
    Log(etInfo,'Using index page %s',[IndexPageName]);
  if (Self.FPassword<>'') then
    DoLog(etInfo,'/quit route set up');
  if FEcho then
    DoLog(etInfo,'Setting up /echo route');
  Log(etInfo,'Location REST API '+IfThen(FAPISecret<>'','','NOT ')+'activated.');
end;

destructor THTTPApplication.Destroy;
begin
  inherited Destroy;
end;

procedure THTTPApplication.SetupCapture;

Var
  Dest : String;
  Svc : TDebugCaptureService;

begin
  Svc:=TDebugCaptureService.Instance;
  Dest:=Svc.LogFileName;
  if (Dest='') and Svc.LogToConsole then
    Dest:='Console';
  if Dest<>'' then
    begin
    DoLog(etInfo,Format('Setting up capture on route "%s", writing to %s',[SCaptureRoute,Dest]));
    HTTPRouter.RegisterRoute(SCaptureRoute,rmPost,@Svc.HandleRequest,False);
    end;
end;

procedure THTTPApplication.DoRun;

Var
  S,ConfigFile : String;

begin
  FMaxAge:=31557600;
  S:=Checkoptions('hqd:ni:p:sH:m:x:c:beQ:a:A:ou::VI',['help','quiet','noindexpage','directory:','port:','indexpage:','ssl','hostname:','mimetypes:','proxy:','config:','background','echo','quit:','max-age:','api:','coi','capture','version','interface']);
  if (S<>'') or HasOption('h','help') then
    usage(S);
  if HasOption('V','version') then
    begin
    Terminate;
    Writeln(ServerVersion);
    Exit;
    end;
  if HasOption('c','config') then
    ConfigFile:=GetOptionValue('c','config')
  else
    ConfigFile:='simpleserver.ini';
  ReadConfigFile(ConfigFile);
  ProcessOptions;
  if FBackground then
    begin
{$ifdef unix}
    if FPFork>0 then Halt(0);
{$else}
    Log(etError,'Background option not supported');
{$endif}
    end;
  SetupCapture;
  if FPassword<>'' then
    begin
    HTTPRouter.RegisterRoute('/quit',rmAll,@Doquit,False);
    end;
  if FEcho  then
    HTTPRouter.RegisterRoute('/echo',rmAll,@DoEcho,False);
  if ProxyManager.LocationCount>0 then
    begin
    TProxyWebModule.RegisterModule('Proxy',True);
    ProxyManager.OnLog:=@DoProxyLog;
    end;
  DefaultCacheControlMaxAge:=FMaxAge; // one year by default
  if BaseDir='' then
    BaseDir:=GetCurrentDir;
  if (BaseDir<>'') then
    BaseDir:=IncludeTrailingPathDelimiter(BaseDir);
  if FAPISecret<>'' then
    TFPWebFileLocationAPIModule.RegisterFileLocationAPI(ExtractWord(1,FAPISecret,[',']),ExtractWord(2,FAPISecret,[',']));
  if FCrossOriginIsolation then  
    TSimpleFileModule.DefaultSimpleFileModuleClass:=TMySimpleFileModule;
  TSimpleFileModule.RegisterDefaultRoute;
  TSimpleFileModule.BaseDir:=BaseDir;
  TSimpleFileModule.OnLog:=@Log;
  If not NoIndexPage then
    begin
    if (IndexPageName='') then
      IndexPageName:='index.html';
    TSimpleFileModule.IndexPageName:=IndexPageName;
    end;
  if not Quiet then
    WriteInfo;
  if InterfaceAddress<>'' then
    HTTPHandler.Address:=InterfaceAddress;
  inherited;
end;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

