unit httpcompiler;

{$mode objfpc}
{$H+}

interface

uses
  {$ifdef unix}baseunix,{$endif}
  {$IF FPC_FULLVERSION > 30300}
  strutils,
  {$ENDIF}
  sysutils, classes, fpjson, contnrs, syncobjs, fpmimetypes, custhttpapp, inifiles,
  fpwebproxy, webutil, fpwebfile, httproute, httpdefs, dirwatch, Pas2JSFSCompiler,
  Pas2JSCompilerCfg, ssockets, fpdebugcapturesvc;

Const
  HTTPCompilerVersion = '1.0';
  nErrTooManyThreads = -1;
  nExitCodeSocketError = 1;

Type
  TDirWatcher = Class;
  THTTPCompilerApplication = Class;

  { TCompileItem }

  TCompileItem = Class(TCollectionItem)
  private
    FBaseDir: string;
    FConfigFile: String;
    FFileName: string;
    FOutput : TStrings;
    FOptions : TStrings;
    FSuccess: Boolean;
    FThread: TThread;
    function GetOptions: TStrings;
    function GetOutput: TStrings;
  Public
    Destructor Destroy; override;
    Property BaseDir : string Read FBaseDir Write FBaseDir;
    Property FileName : string Read FFileName Write FFileName;
    Property ConfigFile: String Read FConfigFile Write FConfigFile;
    Property Options : TStrings Read GetOptions;
    Property Output : TStrings Read GetOutput;
    Property Thread : TThread Read FThread;
    Property Success : Boolean  Read FSuccess;
  end;

  { TCompiles }

  TCompiles = Class(TCollection)
  private
    function GetC(AIndex : Integer): TCompileItem;
  Public
     Property Compiles[AIndex : Integer] : TCompileItem Read GetC; default;
  end;

  { TCompileThread }

  TCompileThread = class(TThread)
  private
    FApp : THTTPCompilerApplication;
    FItem: TCompileItem;
    procedure DoCompilerLog(Sender: TObject; const Msg: String);
    procedure SetItem(AValue: TCompileItem);
  Public
    Constructor create(App : THTTPCompilerApplication; aItem : TCompileItem);
    Procedure Execute; override;
    Property Item : TCompileItem read FItem write SetItem;
  end;

  { TDirWatcher }

  TDirWatcher = Class(TComponent)
  Private
    FApp : THTTPCompilerApplication;
    FDW : TDirWatch;
    procedure DoChange(Sender: TObject; aEntry: TDirectoryEntry; AEvents: TFileEvents);
  Public
    Constructor Create(App : THTTPCompilerApplication; ADir : String);overload;
    Destructor Destroy; override;
  end;

  { TMySimpleFileModule }

  TMySimpleFileModule = class(TSimpleFileModule)
  Public
    Procedure SendFile(const AFileName: String; AResponse: TResponse); override;
  end;

  { THTTPCompilerApplication }

  THTTPCompilerApplication = Class(TCustomHTTPApplication)
  private
    FAPI: String;
    FBaseDir: String;
    FConfigFile: String;
    FIndexPageName: String;
    FNoIndexPage: Boolean;
    FProjectFile: String;
    FStatusLock : TCriticalSection;
    FQuiet: Boolean;
    FWatch: Boolean;
    FDW : TDirWatcher;
    FStatusList : TFPObjectList;
    FCompiles : TCompiles;
    FServeOnly  : Boolean;
    FMimeFile : String;
    FBackground:boolean;
    FPassword:String;
    FEcho:Boolean;
    FMaxAge: integer;
    FCrossOriginIsolation : Boolean;
    FInterfaceAddress : String;
    procedure AddToStatus(O: TJSONObject);
    procedure DoEcho(ARequest: TRequest; AResponse: TResponse);
    procedure DoProxyLog(Sender: TObject; const aMethod, aLocation, aFromURL, aToURL: String);
    procedure Doquit(ARequest: TRequest; AResponse: TResponse);
    procedure SetupCapture;
    function HandleCompileOptions(aDir: String): Boolean;
    function ProcessOptions: Boolean;
    procedure ReadConfigFile(const ConfigFile: string);
    Procedure ReportBuilding(AItem : TCompileItem);
    Procedure ReportBuilt(AItem : TCompileItem);
    Procedure AddToStatus(AEntry : TDirectoryEntry; AEvents : TFileEvents);
    procedure DoStatusRequest(ARequest: TRequest; AResponse: TResponse);
    procedure DoRecompile(ARequest: TRequest; AResponse: TResponse);
    function ScheduleCompile(const aProjectFile: String; Options : TStrings = Nil): Integer;
    procedure StartWatch(ADir: String);
  protected
    procedure Usage(Msg: String); virtual;
    function GetDefaultMimeTypesFile: string; virtual;
    procedure LoadDefaultMimeTypes; virtual;
  public
    Constructor Create(AOWner : TComponent); override;
    Destructor Destroy; override;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    Procedure DoRun; override;
    Property API : String Read FAPI Write FAPI;
    property Quiet : Boolean read FQuiet Write FQuiet;
    Property Watch : Boolean Read FWatch Write FWatch;
    Property ProjectFile : String Read FProjectFile Write FProjectFile;
    Property ConfigFile : String Read FConfigFile Write FConfigFile;
    Property BaseDir : String Read FBaseDir;
    Property ServeOnly : Boolean Read FServeOnly;
    Property MimeFile : String Read FMimeFile;
    Property NoIndexPage : Boolean Read FNoIndexPage Write FNoIndexPage;
    Property IndexPageName : String Read FIndexPageName Write FIndexPageName;
    Property InterfaceAddress : String Read FInterfaceAddress Write FInterfaceAddress;
  end;


Implementation

{ TMySimpleFileModule }

procedure TMySimpleFileModule.SendFile(const AFileName: String; AResponse: TResponse);
begin
  AResponse.SetCustomHeader('Cross-Origin-Embedder-Policy','require-corp');
  AResponse.SetCustomHeader('Cross-Origin-Opener-Policy','same-origin');
  inherited SendFile(AFileName, AResponse);
end;



{ TCompileThread }

procedure TCompileThread.SetItem(AValue: TCompileItem);
begin
  if FItem=AValue then Exit;
  FItem:=AValue;
end;

procedure TCompileThread.DoCompilerLog(Sender: TObject; const Msg: String);
begin
  If Assigned(Item) then
    Item.Output.Add(Msg);
end;

constructor TCompileThread.create(App: THTTPCompilerApplication; aItem: TCompileItem);

begin
  FItem:=aItem;
  FApp:=App;
  FreeOnTerminate:=True;
  inherited create(False);
end;

procedure TCompileThread.Execute;

Var
  C : TPas2JSFSCompiler;
  L : TStrings;

begin
  L:=Nil;
  C:=TPas2JSFSCompiler.Create;
  Try
    C.ConfigSupport:=TPas2JSFileConfigSupport.Create(C);
    FApp.ReportBuilding(Item);
    L:=TStringList.Create;
    L.Assign(Item.Options);
    if (Item.ConfigFile<>'') then
      L.Add('@'+Item.ConfigFile);
    L.Add(Item.FileName);
    C.Log.OnLog:=@DoCompilerLog;
    try
      C.Run(ParamStr(0),Item.BaseDir,L,True);
      Item.FSuccess:=True;
    except
      On E : Exception do
        Item.Output.Add(Format('Error %s compiling %s: %s',[E.ClassName,Item.FileName,E.Message]));
    end;
    FApp.ReportBuilt(Item);
  Finally
    C.Free;
    L.Free;
  end;
  Item.FThread:=Nil;
end;

{ TCompiles }

function TCompiles.GetC(AIndex : Integer): TCompileItem;
begin
  Result:=Items[Aindex] as TCompileItem;
end;

{ TCompileItem }

function TCompileItem.GetOutput: TStrings;
begin
  If (FOutput=Nil) then
    FOutput:=TStringList.Create;
  Result:=FOutput;
end;

function TCompileItem.GetOptions: TStrings;
begin
  If (FOptions=Nil) then
    FOptions:=TStringList.Create;
  Result:=FOptions;
end;

destructor TCompileItem.Destroy;
begin
  FreeAndNil(FOutput);
  FreeAndNil(FOptions);
  inherited Destroy;
end;


{ TDirWatcher }

procedure TDirWatcher.DoChange(Sender: TObject; aEntry: TDirectoryEntry; AEvents: TFileEvents);
begin
  if Assigned(FApp) then
    FApp.AddToStatus(AEntry,AEvents);
end;

constructor TDirWatcher.Create(App: THTTPCompilerApplication; ADir: String);
begin
  Inherited create(APP);
  FApp:=App;
  FDW:=TDirwatch.Create(Self);
  FDW.AddWatch(ADir,allEvents);
  FDW.OnChange:=@DoChange;
  TThread.ExecuteInThread(@FDW.StartWatch);
end;

destructor TDirWatcher.Destroy;
begin
  FApp:=Nil;
  FDW.Terminate;
  FreeAndNil(FDW);
  inherited Destroy;
end;

{ THTTPCompilerApplication }

procedure THTTPCompilerApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  {AllowWriteln}
  if Quiet then
    exit;
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
  else
    inherited DoLog(EventType, Msg);
  {AllowWriteln-}
end;

procedure THTTPCompilerApplication.Usage(Msg : String);

begin
  {AllowWriteln}
  if (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Version ',HTTPCompilerVersion);
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] ');
  Writeln('Where options is one or more of : ');
  Writeln('-A --api=location,secret Enable location management API.');
  Writeln('-c --compile[=proj]      Recompile project if pascal files change. Default project is app.lpr');
  Writeln('-d --directory=dir       Base directory from which to serve files.');
  Writeln('                         Default is current working directory: ',GetCurrentDir);
  Writeln('-h --help                This help text');
  Writeln('-i --indexpage=name      Directory index page to use (default: index.html)');
  Writeln('-I --interface=IP        Listen on this interface address only.');
  Writeln('-m --mimetypes=file      Set Filename for loading mimetypes. Default is ',GetDefaultMimeTypesFile);
  Writeln('-n --noindexpage         Do not allow index page.');
  Writeln('-o --coi                 Enable Cross-Origin Isolation headers');
  Writeln('-p --port=NNNN           TCP/IP port to listen on (default is 3000)');
  Writeln('-q --quiet               Do not write diagnostic messages');
  Writeln('-s --simpleserver        Only serve files, do not enable compilation.');
  Writeln('-u --capture[=FILE]      Set up /debugcapture route to capture output sent by browser.');
  Writeln('                         If FILE is specified, write to file. If not specified, writes to STDOUT.');
  Writeln('-w --watch               Watch directory for changes');
  Halt(Ord(Msg<>''));
  {AllowWriteln-}
end;

function THTTPCompilerApplication.GetDefaultMimeTypesFile: string;
begin
  {$ifdef unix}
  Result:='/etc/mime.types';
  {$ifdef darwin}
  if not FileExists(Result) then
    Result:='/private/etc/apache2/mime.types';
  {$endif}
  {$else}
  Result:=ExtractFilePath(System.ParamStr(0))+'mime.types';
  {$endif}
end;

procedure THTTPCompilerApplication.LoadDefaultMimeTypes;
begin
  MimeTypes.LoadKnownTypes;
  // To be sure
  MimeTypes.AddType('application/xhtml+xml','xhtml;xht');
  MimeTypes.AddType('text/html','html;htm');
  MimeTypes.AddType('text/plain','txt');
  MimeTypes.AddType('application/javascript','js');
  MimeTypes.AddType('text/plain','map');
  MimeTypes.AddType('application/json','json');
  MimeTypes.AddType('image/png','png');
  MimeTypes.AddType('image/jpeg','jpeg;jpg');
  MimeTypes.AddType('image/gif','gif');
  MimeTypes.AddType('image/jp2','jp2');
  MimeTypes.AddType('image/tiff','tiff;tif');
  MimeTypes.AddType('application/pdf','pdf');
  MimeTypes.AddType('text/css','css');
end;

constructor THTTPCompilerApplication.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FStatusLock:=TCriticalSection.Create;
  FStatusList:=TFPObjectList.Create(False);
  FCompiles:=TCompiles.Create(TCompileItem);
end;

destructor THTTPCompilerApplication.Destroy;
begin
  FStatusLock.Enter;
  try
    FreeAndNil(FCompiles);
    FreeAndNil(FStatusList);
  finally
    FStatusLock.Leave;
  end;
  FreeAndNil(FStatusLock);
  inherited Destroy;
end;

procedure THTTPCompilerApplication.StartWatch(ADir : String);

begin
  FDW:=TDirWatcher.Create(Self,ADir);
end;

procedure THTTPCompilerApplication.ReportBuilding(AItem: TCompileItem);

Var
  O : TJSONObject;

begin
  O:=TJSONObject.Create(['action','building','compileID',AItem.ID,'project',AItem.FileName,'config',AItem.ConfigFile]);
  AddToStatus(O);
end;

procedure THTTPCompilerApplication.ReportBuilt(AItem: TCompileItem);

Var
  O : TJSONObject;
  A : TJSONArray;
  I : Integer;

begin
  A:=TJSONArray.Create;
  For I:=0 to AItem.Output.Count-1 do
    A.Add(AItem.Output[i]);
  O:=TJSONObject.Create(['action','built','compileID',AItem.ID,'project',AItem.FileName,'config',AItem.ConfigFile,'output',A,'success',AItem.Success]);
  AddToStatus(O);
end;

procedure THTTPCompilerApplication.AddToStatus(O : TJSONObject);

begin
  FStatusLock.Enter;
  try
    {$ifdef VerboseHTTPCompiler}
    Writeln('Adding to status ',Assigned(O),' : ',O.ClassName);
    {$endif}
    FStatusList.Add(O);
  finally
    FStatusLock.Leave;
  end;
end;

procedure THTTPCompilerApplication.AddToStatus(AEntry: TDirectoryEntry; AEvents: TFileEvents);

Var  
  O : TJSONObject;
  FN : String;

begin
  Log(etDebug,'File change detected: %s (%s)',[AEntry.name,FileEventsToStr(AEvents)]);
  O:=TJSONObject.Create(['action','file','name',AEntry.name,'events',FileEventsToStr(AEvents)]);
  if Pos(ExtractFileExt(AEntry.Name),'.lpr.pas.pp.inc.dpr')>0 then
    FN:=AEntry.Name;
  if (FN<>'') then
    O.Add('recompile',true);
  AddToStatus(O);
  if (FN<>'') then
    begin
    Log(etDebug,'File change forces recompile: %s',[AEntry.name]);
    ScheduleCompile('',Nil);
    end;
end;

procedure THTTPCompilerApplication.DoStatusRequest(ARequest : TRequest; AResponse : TResponse);

Var
  R,O : TJSONObject;
  A : TJSONArray;
  I : integer;
begin
  Log(etDebug,'Status request from: %s',[ARequest.RemoteAddress]);
  R:=Nil;
  try
    FStatusLock.Enter;
    try
      if (FStatusList.Count=0) then
        R:=TJSONObject.Create(['ping',True])
      else
        begin
        {$ifdef VerboseHTTPCompiler}
        Writeln(FStatusList[0].ClassName);
        {$endif}
        O:=FStatusList[0] as TJSONObject;
        FStatusList.Delete(0);
        if O.Get('action','')<>'file' then
          R:=O
        else
          begin
          // If first event is file event, then add and delete all file events in list.
          A:=TJSONArray.Create([O]);
          O.Delete('action');
          R:=TJSONObject.Create(['action','sync','files',A]);
          For I:=FStatusList.Count-1 downto 0 do
            begin
            O:=FStatusList[I] as TJSONObject;
            if (O.Get('action','')='file') then
              begin
              A.Add(O);
              O.Delete('action');
              FStatusList.Delete(I);
              end;
            end;
          end
        end;
    finally
      FStatusLock.Leave;
    end;
    AResponse.ContentType:='application/json';
    AResponse.Content:=R.AsJSON;
    AResponse.SendResponse;
  finally
    R.Free;
  end;
end;

function THTTPCompilerApplication.ScheduleCompile(const aProjectFile: String;
  Options: TStrings): Integer;

Var
  CI : TCompileItem;
  I,TC : Integer;

begin
  TC:=0;
  For I:=0 to FCompiles.Count-1 do
    if Assigned(FCompiles[I].THread) then
      Inc(TC);
  if TC>10 then
    begin
    Log(etError,'Refusing compile of file "%s" using config file "%s"',[AProjectFile, ConfigFile]);
    Exit(nErrTooManyThreads);
    end;
  CI:=FCompiles.Add as TCompileItem;
  Log(etInfo,'Scheduling compile ID %d of file "%s" using config file "%s"',[CI.ID,AProjectFile, ConfigFile]);
  CI.BaseDir:=BaseDir;
  CI.FileName:=AProjectFile;
  CI.ConfigFile:=ConfigFile;
  if Assigned(Options) then
    CI.Options.Assign(Options);
  TCompileThread.Create(Self,CI);
  Result:=CI.ID;
end;

procedure THTTPCompilerApplication.DoRecompile(ARequest: TRequest; AResponse: TResponse);

Var
  ID : Integer;
  PF,CL : String;
  Options: TStrings;

begin
  PF:=ARequest.ContentFields.Values['ProjectFile'];
  CL:=ARequest.ContentFields.Values['CompileOptions'];
  if PF='' then
    PF:=ProjectFile;
  If (PF='') then
    begin
    AResponse.ContentType:='application/json';
    AResponse.Content:='{ "success" : false, "message": "no project file set or provided" }';
    AResponse.Code:=404;
    AResponse.CodeText:='No project file';
    end
  else
    begin
    Options:=Nil;
    try
      if CL<>'' then
        begin
        Options:=TStringList.Create;
        Options.Text:=Cl;
        end;
      ID:=ScheduleCompile(PF,Options);
    finally
      FreeAndNil(Options);
    end;
    if ID=nErrTooManyThreads then
      begin
      AResponse.Code:=403;
      AResponse.CodeText:='Too many compiles';
      AResponse.ContentType:='application/json';
      AResponse.Content:='{ "success" : false, "message": "Too many compiles running" }';
      end
    else
      begin
      AResponse.Code:=200;
      AResponse.ContentType:='application/json';
      AResponse.Content:=Format('{ "success" : true, "file": "%s", "commandLine" : "%s", "compileID": %d }',[StringToJSONString(PF),StringToJSONString(CL),ID]);
      end
    end;
  AResponse.SendResponse;
end;

function THTTPCompilerApplication.HandleCompileOptions(aDir: String): Boolean;

begin
  Result:=False;
  Watch:=HasOption('w','watch');
  if Hasoption('P','project') then
    begin
    ProjectFile:=GetOptionValue('P','project');
    if ProjectFile='' then
      ProjectFile:=IncludeTrailingPathDelimiter(aDir)+'app.lpr';
    If Not FileExists(ProjectFile) then
      begin
      Terminate;
      Log(etError,'Project file "%s" does not exist, aborting.',[ProjectFile]);
      Exit;
      end;
    ConfigFile:=GetOptionValue('c','config');
    if (ConfigFile='') then
      ConfigFile:=ChangeFileExt(Projectfile,'.cfg');
    if not FileExists(ConfigFile) then
      ConfigFile:='';
    end;
  if Watch then
    begin
    if (ProjectFile='') then
      Log(etWarning,'No project file specified, disabling watch.')   ;
    StartWatch(aDir);
    end;
  Result:=True;
end;

procedure THTTPCompilerApplication.DoProxyLog(Sender: TObject; const aMethod, aLocation, aFromURL, aToURL: String);

Var
  Msg : String;

begin
  if Quiet then
    exit;
  Msg:=Format('(Proxy redirect) location: %s, Method: %s, From: %s, to: %s',[aLocation,aMethod,aFromURl,atoURL]);
  if IsConsole then
    {AllowWriteln}
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',etInfo,'] ',Msg)
    {AllowWriteln-}
  else
    inherited DoLog(etInfo, Msg);
end;

procedure THTTPCompilerApplication.DoEcho(ARequest: TRequest; AResponse: TResponse);

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
    AResponse.SendResponse;
  finally
    L.Free;
  end;
end;
procedure THTTPCompilerApplication.Doquit(ARequest: TRequest; AResponse: TResponse);

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

Const
  SCaptureRoute = '/debugcapture';

procedure THTTPCompilerApplication.ReadConfigFile(Const ConfigFile : string);

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


Var
  L : TStringList;
  C,P,U : String;
  I : Integer;

begin
  if (ConfigFile='') or Not FileExists(ConfigFile) then exit;
  L:=Nil;
  With TMemIniFile.Create(ConfigFile) do
    try
      FBaseDir:=ReadString(SConfig,KeyDir,BaseDir);
      Port:=ReadInteger(SConfig,KeyPort,Port);
      InterfaceAddress:=ReadString(SConfig,KeyInterface,InterfaceAddress);
      Quiet:=ReadBool(SConfig,KeyQuiet,Quiet);
      FMimeFile:=ReadString(SConfig,keyMimetypes,MimeFile);
      NoIndexPage:=ReadBool(SConfig,KeyNoIndexPage,NoIndexPage);
      IndexPageName:=ReadString(SConfig,KeyIndexPage,IndexPageName);
      HostName:=ReadString(SConfig,KeyHostName,HostName);
      UseSSL:=ReadBool(SConfig,KeySSL,UseSSL);
      FBackground:=ReadBool(SConfig,Keybackground,FBackGround);
      FPassword:=ReadString(SConfig,KeyQuit,FPassword);
      FEcho:=ReadBool(SConfig,KeyEcho,FEcho);
      FMaxAge:=ReadInteger(SConfig,KeyMaxAge,FMaxAge);
      FAPI:=ReadString(SConfig,keyAPI,'');
      FCrossOriginIsolation:=ReadBool(SConfig,KeyCOI,FCrossOriginIsolation);
      if ValueExists(SConfig,KeyCapture) then
        begin
        C:=ReadString(SConfig,keyCapture,'');
        if C='-' then
          TDebugCaptureService.Instance.LogToConsole:=True
        else
          TDebugCaptureService.Instance.LogFileName:=C;
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


function THTTPCompilerApplication.ProcessOptions: Boolean;

Var
  C,IndexPage,D : String;

begin
  Result:=False;
  if HasOption('A','api') then
    FAPI:=GetOptionValue('A','api');
  FServeOnly:=FServeOnly or HasOption('s','serve-only');
  Quiet:=Quiet or HasOption('q','quiet');
  if (Port=0) or HasOption('p','port') then
    Port:=StrToIntDef(GetOptionValue('p','port'),3000);
  if HasOption('d','directory') then
    D:=GetOptionValue('d','directory');
  if D='' then
    D:=GetCurrentDir;
  if HasOption('m','mimetypes') then
    MimeTypesFile:=GetOptionValue('m','mimetypes');
    
  if MimeTypesFile='' then
    begin
    MimeTypesFile:=GetDefaultMimeTypesFile;
    if not FileExists(MimeTypesFile) then
      begin
      MimeTypesFile:='';
      LoadDefaultMimeTypes;
      end;
    end
  else if not FileExists(MimeTypesFile) then
    Log(etWarning,'mimetypes file not found: '+MimeTypesFile);
  FBaseDir:=D;
  if not ServeOnly then
    if not HandleCompileOptions(D) then
      exit(False);
  TSimpleFileModule.BaseDir:=IncludeTrailingPathDelimiter(D);
  TSimpleFileModule.OnLog:=@Log;
  Log(etInfo,'Listening on port %d, serving files from directory: %s',[Port,D]);
  if ServeOnly then
    Log(etInfo,'Compile requests will be ignored.');
  NoIndexPage:=NoIndexPage or HasOption('n','noindexpage');
  if HasOption('i','indexpage') then
    IndexPage:=GetOptionValue('i','indexpage');
  if HasOption('I','interface') then
    InterfaceAddress:=GetOptionValue('I','interface');   
  If not NoIndexPage then
    begin
    if (IndexPage='') then
      IndexPage:='index.html';
    Log(etInfo,'Using index page %s',[IndexPage]);
    TSimpleFileModule.IndexPageName:=IndexPage;
    end;
  FCrossOriginIsolation:=hasOption('o','coi');
  if HasOption('u','capture') then
    begin
    C:=GetOptionValue('u','capture');
    if C='' then
      TDebugCaptureService.Instance.LogToConsole:=True
    else
      TDebugCaptureService.Instance.LogFileName:=C;
    end;
  Result:=True;
end;

procedure THTTPCompilerApplication.DoRun;

Var
  S : String;

begin
  S:=Checkoptions('shqVd:ni:p:wP::cm:A:I:u::',['help','quiet','version','noindexpage','directory:','port:','indexpage:','watch','project::','config:','simpleserver','mimetypes:','api:','interface:','capture']);
  if (S<>'') or HasOption('h','help') then
    Usage(S);
  if HasOption('V','version') then
    begin
    {AllowWriteln}
    writeln(HTTPCompilerVersion);
    {AllowWriteln-}
    Terminate;
    exit;
    end;
  if HasOption('c','config') then
    ConfigFile:=GetOptionValue('c','config')
  else
    ConfigFile:='compileserver.ini';
  Port:=3000;
  ReadConfigFile(ConfigFile);
  If not ProcessOptions then
    begin
    Terminate;
    exit;
    end;
  if FBackground then
    begin
{$ifdef unix}
    if FPFork>0 then Halt(0);
{$else}
    Log(etError,'Background option not supported');
{$endif}
    end;
  // Handle options
  SetupCapture;
  if FPassword<>'' then
    HTTPRouter.RegisterRoute('/quit',rmAll,@Doquit,False);
  if FEcho  then
    HTTPRouter.RegisterRoute('/echo',rmAll,@DoEcho,False);
  if ProxyManager.LocationCount>0 then
    begin
    TProxyWebModule.RegisterModule('Proxy',True);
    ProxyManager.OnLog:=@DoProxyLog;
    end;
  DefaultCacheControlMaxAge:=FMaxAge; // one year by default
  if not ServeOnly then
    begin
    httprouter.RegisterRoute('$sys/compile',rmPost,@DoRecompile);
    httprouter.RegisterRoute('$sys/status',rmGet,@DoStatusRequest);
    end;
  if FAPI<>'' then
    {$IF FPC_FULLVERSION > 30300}
    TFPWebFileLocationAPIModule.RegisterFileLocationAPI(ExtractWord(1,FAPI,[',']),ExtractWord(2,FAPI,[',']));
    {$ELSE}
    Log(etError,'API support missing, compile with fpc 3.3.1+');
    {$ENDIF}
  if FCrossOriginIsolation then
    {$IF FPC_FULLVERSION > 30300}
    TSimpleFileModule.DefaultSimpleFileModuleClass:=TMySimpleFileModule;
    {$ELSE}
    Log(etError,'CrossOriginIsolation support missing, compile with fpc 3.3.1+');
    {$ENDIF}
  TSimpleFileModule.RegisterDefaultRoute;
  if InterfaceAddress<>'' then
    HTTPHandler.Address:=InterfaceAddress;
  try
    inherited DoRun;
  except
    on E: ESocketError do begin
      Log(etError,E.ClassName+': '+E.Message);
      ExitCode:=nExitCodeSocketError;
      Terminate;
    end;
  end;
end;


procedure THTTPCompilerApplication.SetupCapture;

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

end.
