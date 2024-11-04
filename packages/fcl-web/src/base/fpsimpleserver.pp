{$IFNDEF FPC_DOTTEDUNITS}
unit fpsimpleserver;
{$ENDIF}
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

{$mode ObjFPC}{$H+}
{ $DEFINE USEMICROHTTP }

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Types, System.StrUtils, System.IniFiles, FpJson.Parser,
  {$ifdef unix}
  UnixApi.Base,
  {$endif}
  {$IFNDEF VER3_2}
  FpWeb.Http.DebugCapture,
  {$ENDIF}
  {$IFNDEF USEMICROHTTP}
    FpWeb.HostApp.Custom.HttpApp,
  {$ELSE}
    FpWeb.HostApp.Custom.MicroHttpApp,
  {$ENDIF}
  FpWeb.Route, FpWeb.Http.Defs, FpWeb.MimeTypes, FpWeb.Modules.Files, FpWeb.Modules.Proxy,
  FpWeb.Utils;
{$ELSE}
  SysUtils, Classes,
  {$ifdef unix}
  baseunix,
  {$endif}
  {$IFNDEF VER3_2}
  fpdebugcapturesvc,
  {$ENDIF}
  {$IFNDEF USEMICROHTTP}
    custhttpapp,
  {$ELSE}
    custmicrohttpapp,
  {$ENDIF}
  types, jsonparser, strutils, inifiles, httproute, httpdefs, fpmimetypes, fpwebfile, fpwebproxy,
  webutil;
{$ENDIF}
Const
  ServerVersion = '1.1';

Type

  { TFPSimpleServerApplication }
{$IFDEF USEMICROHTTP}
  TSimpleServerParentApp = TCustomMicroHTTPApplication;
{$ELSE}
  TSimpleServerParentApp = TCustomHTTPApplication;
{$ENDIF}


  TFPSimpleServerApplication = Class(TSimpleServerParentApp)
  private
    FProxyDefs : TStrings;
    FLocations : TStrings;
    FHeaders: TStrings;
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
    procedure DoProxyLog(Sender: TObject; const aMethod, aLocation, aFromURL, aToURL: String);
  Protected
    // Log
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    // Override doRun to run server
    Procedure DoRun; override;
    // Configure server. Called before registering routes.
    Procedure ConfigureServer; virtual;
    // Register all HTTP routes
    Procedure RegisterRoutes; virtual;
    {$IFNDEF VER3_2}
    // Set up debug capture
    procedure SetupCapture; virtual;
    // Register custom headers
    Procedure RegisterCustomHeaders; virtual;
    {$ENDIF}
    // Apply COI/CORP headers to file request
    procedure ApplyCoi(Sender: TObject; aResponse: TResponse); virtual;
    // Handle echo URL
    procedure DoEcho(ARequest: TRequest; AResponse: TResponse); virtual;
    // Handle Quit URL
    procedure DoQuit(ARequest: TRequest; AResponse: TResponse); virtual;
    // Get valid command-line options
    procedure GetValidOptions(out aShort: String; out aLong: TStringDynArray); virtual;
    // Load mime types
    procedure LoadMimeTypes; virtual;
    // Process command-line options
    procedure ProcessOptions; virtual;
    // Write usage to console;
    procedure WriteOptions; virtual;
    // Read config file.
    procedure DoReadConfigFile(const aIni : TCustomIniFile); virtual;
    // Write run info to console
    procedure Writeinfo; virtual;
    // Register file locatons
    procedure RegisterFileLocations; virtual;
    // Register proxy
    Procedure RegisterProxies; virtual;
    // Get name of default config file
    Function GetDefaultConfigFile : string; virtual;
  Public
    constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Write usage and halt application. If msg is non-empty halt application with exit code 1
    procedure Usage(Msg: String);
    // Add a proxy definition
    procedure AddProxy(const aProxyDef: String);
    // Read config ini file
    procedure ReadConfigFile(const ConfigFile: string); virtual;
    {$IFDEF UNIX}
    Property Background : Boolean Read FBackGround Write FBackGround;
    {$ENDIF}
    // Emit CORP headers
    Property CrossOriginIsolation : Boolean Read FCrossOriginIsolation Write FCrossOriginIsolation;
    // List of proxy definitions: /proxy/:Location=URL
    Property ProxyDefs : TStrings Read FProxyDefs;
    // List of file location definitions :Location=URL
    Property Locations : TStrings Read FLocations;
    // Extra headers to add during requests
    Property Headers : Tstrings Read FHeaders;
    // Enable echo response
    Property Echo : Boolean Read FEcho Write FEcho;
    // Do not log
    property Quiet : Boolean read FQuiet Write FQuiet;
    // MaxAge on files
    Property MaxAge : Integer Read FMaxAge Write FMaxAge;
    // Mime file to use for determining MIME types (content-type)
    Property MimeFile : String Read FMimeFile Write FMimeFile;
    // Base directory to serve files from
    Property BaseDir : string Read FBaseDir Write FBaseDir;
    // Password to use for locations API
    Property APISecret : String Read FAPISecret Write FAPISecret;
    // Password to use for /quit URL
    Property Password : String Read FPassword Write FPassword;
    // Do not use index page
    Property NoIndexPage : Boolean Read FNoIndexPage Write FNoIndexPage;
    // Name of index page
    Property IndexPageName : String Read FIndexPageName Write FIndexPageName;
    // Listen only on this interface address.
    Property InterfaceAddress : String Read FInterfaceAddress Write FInterfaceAddress;
  end;

implementation

Const
  SCaptureRoute = '/debugcapture';

Const
  SConfig  = 'Server';
  SProxy = 'Proxy';
  SLocations = 'Locations';
  SHeaders = 'Headers';

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


{$IFDEF VER3_2}
Type
  { TCoiAwareSimpleFileModule }
  TCoiAwareSimpleFileModule = class(TFPCustomFileModule)
  Public
    Constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override;
    Procedure SendFile(const AFileName: String; AResponse: TResponse); override;
  end;

{ TCoiAwareSimpleFileModule }

constructor TCoiAwareSimpleFileModule.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
end;

procedure TCoiAwareSimpleFileModule.SendFile(const AFileName: String; AResponse: TResponse);
begin
  AResponse.SetCustomHeader('Cross-Origin-Embedder-Policy','require-corp');
  AResponse.SetCustomHeader('Cross-Origin-Opener-Policy','same-origin');
  inherited SendFile(AFileName, AResponse);
end;
{$ENDIF}


{ THTTPApplication }
constructor TFPSimpleServerApplication.create(aOwner : TComponent);

begin
  Inherited;
  FProxyDefs:=TStringList.Create;
  FLocations:=TStringList.Create;
  FHeaders:=TStringList.Create;
end;

destructor TFPSimpleServerApplication.Destroy;
begin
  FreeAndNil(FProxyDefs);
  FreeAndNil(FLocations);
  FreeAndNil(FHeaders);
  inherited Destroy;
end;


procedure TFPSimpleServerApplication.ApplyCoi(Sender : TObject; aResponse : TResponse);

begin
  AResponse.SetCustomHeader('Cross-Origin-Embedder-Policy','require-corp');
  AResponse.SetCustomHeader('Cross-Origin-Opener-Policy','same-origin');
end;

procedure TFPSimpleServerApplication.DoEcho(ARequest: TRequest; AResponse: TResponse);

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


procedure TFPSimpleServerApplication.DoQuit(ARequest: TRequest; AResponse: TResponse);

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

procedure TFPSimpleServerApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if Quiet then
    exit;
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
  else
    inherited DoLog(EventType, Msg);
end;

procedure TFPSimpleServerApplication.DoProxyLog(Sender: TObject; const aMethod, aLocation, aFromURL, aToURL: String);

Var
  Msg : String;

begin
  if Quiet then
    exit;
  Msg:=Format('(Proxy redirect) location: %s, Method: %s, From: %s, to: %s.',[aLocation,aMethod,aFromURl,atoURL]);
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',etInfo,'] ',Msg)
  else
    inherited DoLog(etInfo, Msg);
end;

procedure TFPSimpleServerApplication.Usage(Msg : String);

begin
  if (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] ');
  WriteOptions;
  Writeln('');
  Writeln('Config file is ini file, section [Server]. Key names are long option names.');
  Writeln('Proxies are defined in section [Proxy], Key is name, value is URL.');
  Writeln('Locations are defined in section [Locations], Key is location name, value is path.');
  Halt(Ord(Msg<>''));
end;

procedure TFPSimpleServerApplication.LoadMimeTypes;

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

procedure TFPSimpleServerApplication.AddProxy(const aProxyDef: String);

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

procedure TFPSimpleServerApplication.ReadConfigFile(const ConfigFile: string);

var
   Ini : TCustomIniFile;
begin
  if (ConfigFile='') or Not FileExists(ConfigFile) then exit;
  Ini:=TMemIniFile.Create(ConfigFile);
  try
    DoReadConfigFile(Ini);
  finally
    Ini.free;
  end;
end;

procedure TFPSimpleServerApplication.RegisterProxies;

var
  I : integer;
  Proxy,URL : String;

begin
  For I:=0 to FProxyDefs.Count-1 do
    begin
    FProxyDefs.GetNameValue(I,Proxy,Url);
    if (Proxy<>'') and (Url<>'') then
      ProxyManager.RegisterLocation(Proxy,Url).AppendPathInfo:=true;
    end;
end;

function TFPSimpleServerApplication.GetDefaultConfigFile: string;
begin
  Result:='simpleserver.ini';
end;

{$IFNDEF VER3_2}
procedure TFPSimpleServerApplication.RegisterCustomHeaders;
var
  I : integer;
  lName,lValue : String;

begin
  For I:=0 to FHeaders.Count-1 do
    begin
    FHeaders.GetNameValue(I,lName,lValue);
    if (lName<>'') and (lValue<>'') then
      TFPCustomFileModule.RegisterGlobalResponseHeader(lName,lValue);
    end;
end;

procedure TFPSimpleServerApplication.SetupCapture;

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
    DoLog(etInfo,Format('Setting up capture on route "%s", writing to: %s',[SCaptureRoute,Dest]));
    HTTPRouter.RegisterRoute(SCaptureRoute,rmPost,@Svc.HandleRequest,False);
    end;
end;
{$ENDIF}

procedure TFPSimpleServerApplication.RegisterFileLocations;

var
  I : integer;
  loc,Dir : String;

begin
  For I:=0 to FLocations.Count-1 do
    begin
    FLocations.GetNameValue(I,Loc,Dir);
    if (Loc<>'') and (Dir<>'') then
      RegisterFileLocation(Loc,Dir);
    end;
end;

procedure TFPSimpleServerApplication.ProcessOptions;

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
  {$IFNDEF VER3_2}
  if HasOption('u','capture') then
    begin
    S:=GetOptionValue('u','capture');
    if S='' then
      TDebugCaptureService.Instance.LogToConsole:=True
    else
      TDebugCaptureService.Instance.LogFileName:=S;
    end;
  {$ENDIF}
end;

procedure TFPSimpleServerApplication.WriteOptions;
begin
  Writeln('Version : ',ServerVersion);
  Writeln('Where options is one or more of : ');
  Writeln('-A --api=path,secret  Activate location API on path, using secret as accepted bearer token.');
  Writeln('-a --max-age=age      Set max-age expiry header on returned file requests.');
  Writeln('-c --config=file      Ini configuration file (default: simpleserver.ini).');
{$ifdef unix}
  Writeln('-b --background       fork to background.');
{$endif}
  Writeln('-d --directory=dir    Base directory from which to serve files.');
  Writeln('                      Default is current working directory: ',GetCurrentDir);
  Writeln('-e --echo             Activate /echo URL.');
  Writeln('-h --help             This help text.');
  Writeln('-H --hostname=NAME    Set hostname for self-signed SSL certificate.');
  Writeln('-i --indexpage=name   Directory index page to use (default: index.html)/');
  Writeln('-I --interface=IP     Listen on this interface address only.');
  Writeln('-m --mimetypes=file   Path of mime.types. Loaded in addition to OS known types.');
  Writeln('-n --noindexpage      Do not allow index page.');
  Writeln('-o --coi              Enable Cross-Origin Isolation headers.');
  Writeln('-p --port=NNNN        TCP/IP port to listen on (default is 3000).');
  Writeln('-q --quiet            Do not write diagnostic messages.');
  Writeln('-Q --quit=PWD         Register /quit URL. Send request with password variable equal to PWD to stop.');
  Writeln('-s --ssl              Use SSL.');
  {$IFNDEF VER3_2}
  Writeln('-u --capture[=FILE]   Set up /debugcapture route to capture output sent by browser.');
  Writeln('                      If FILE is specified, write to file. If not specified, writes to STDOUT.');
  {$ENDIF}
  Writeln('-V --version          Display server version and exit.');
  Writeln('-x --proxy=proxydef   Add proxy definition. Definition is of form:');
  Writeln('                      name:BaseURL');
end;

procedure TFPSimpleServerApplication.DoReadConfigFile(const aIni: TCustomIniFile);

begin
  With aIni do
    begin
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
    {$IFNDEF VER3_2}
    if ValueExists(SConfig,KeyCapture) then
      begin
      TDebugCaptureService.Instance.LogFileName:=ReadString(SConfig,keyCapture,'');
      end;
    {$ENDIF}
    ReadSectionValues(SProxy,FProxyDefs,[]);
    ReadSectionValues(SLocations,FLocations,[]);
    ReadSectionValues(SHeaders,FHeaders,[]);
    end;
end;

procedure TFPSimpleServerApplication.Writeinfo;

  function BtoS(B : Boolean) : string;

  begin
    Result:=BoolToStr(B,'True','False');
  end;

Var
  I : Integer;
  Base,N,V : String;

begin
  Log(etInfo,'Listening on port %d',[Port]);
  Log(etInfo,'Serving files from directory: %s',[BaseDir]);
  For I:=0 to ProxyManager.LocationCount-1 do
    with ProxyManager.Locations[i] do
      Log(etInfo,'Proxy location /proxy/%s redirects to: %s',[Path,URL]);
  For I:=0 to FLocations.Count-1 do
    begin
    FLocations.GetNameValue(I,N,V);
    Log(etInfo,'Enabled file location "%s", serving from: %s',[N,V]);
    end;
  Log(etInfo,'Enabled index page: %s',[BToS(NoIndexPage)]);
  if not NoIndexPage then
    Log(etInfo,'Index page name: %s',[IndexPageName]);
  Log(etInfo,'Enabled SSL: %s',[BtoS(UseSSL)]);
  Log(etInfo,'Enabled COI/CORP: %s',[BToS(FCrossOriginIsolation)]);
  Log(etInfo,'Enabled /quit route: %s',[BtoS(Self.FPassword<>'')]);
  Log(etInfo,'Enabled /echo route: %s',[BtoS(FEcho)]);
  Log(etInfo,'Enabled location REST API: %s',[BtoS(FAPISecret<>'')]);
  Base:='http'+IfThen(UseSSL,'s','')+'://localhost:'+IntToStr(Port)+'/';
  Log(etInfo,'Navigate to: %s',[Base]);
  For I:=0 to FLocations.Count-1 do
    begin
    FLocations.GetNameValue(I,N,V);
    Log(etInfo,'Navigate to location "%s" at: %s/',[N,Base+N]);
    end;
end;

procedure TFPSimpleServerApplication.GetValidOptions(out aShort: String; out aLong: TStringDynArray);

Const
  LongOpts : TStringDynArray =
     ('help','quiet','noindexpage','directory:','port:','indexpage:','ssl','hostname:','mimetypes:','proxy:','config:','background','echo','quit:','max-age:','api:','coi','capture','version','interface');

begin
  aShort:='hqd:ni:p:sH:m:x:c:beQ:a:A:ou::VI';
  aLong:=LongOpts;
end;

procedure TFPSimpleServerApplication.DoRun;

Var
  lShort : String;
  lLong : TStringDynArray;
  S,ConfigFile : String;

begin
  FMaxAge:=31557600;
  GetValidOptions(lShort,lLong);
  S:=CheckOptions(lShort,lLong);
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
    ConfigFile:=GetDefaultConfigFile;
  ReadConfigFile(ConfigFile);
  ProcessOptions;
  if FBackground then
    begin
{$ifdef unix}
    if FPFork>0 then Halt(0);
{$else}
    Log(etError,'Background option not supported.');
{$endif}
    end;
  // Set up
  ConfigureServer;
  RegisterRoutes;
  if not Quiet then
    WriteInfo;
  if InterfaceAddress<>'' then
    HTTPHandler.Address:=InterfaceAddress;
  inherited;
end;

procedure TFPSimpleServerApplication.ConfigureServer;
begin
  DefaultCacheControlMaxAge:=FMaxAge; // one year by default
  if BaseDir='' then
    BaseDir:=GetCurrentDir;
  if (BaseDir<>'') then
    BaseDir:=IncludeTrailingPathDelimiter(BaseDir);
  TSimpleFileModule.BaseDir:=BaseDir;
  TSimpleFileModule.OnLog:=@Log;
  If not NoIndexPage then
    begin
    if (IndexPageName='') then
      IndexPageName:='index.html';
    TSimpleFileModule.IndexPageName:=IndexPageName;
    end;
  {$IFNDEF VER_3_2}
  RegisterCustomHeaders;
  {$ENDIF}
  RegisterProxies;
  RegisterFileLocations;
end;

procedure TFPSimpleServerApplication.RegisterRoutes;
begin
  {$IFNDEF VER3_2}
  SetupCapture;
  {$ENDIF}
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
  {$IFNDEF VER3_2_2}
  if FAPISecret<>'' then
    TFPWebFileLocationAPIModule.RegisterFileLocationAPI(ExtractWord(1,FAPISecret,[',']),ExtractWord(2,FAPISecret,[',']));
  {$ENDIF}
  if FCrossOriginIsolation then
    begin
    {$IFDEF VER3_2_2}
    DefaultFileModuleClass:=TCoiAwareSimpleFileModule;
    {$ELSE}
    TFPCustomFileModule.OnPrepareResponse:=@ApplyCoi;
    {$ENDIF}
    end;
  TSimpleFileModule.RegisterDefaultRoute;
end;

end.

