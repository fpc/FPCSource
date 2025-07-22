unit httpcompiler;

{$mode objfpc}
{$H+}

interface

uses
  {$ifdef unix}baseunix,{$endif}
  {$IF FPC_FULLVERSION > 30300}
  strutils,
  {$ENDIF}
  sysutils, classes, fpjson, contnrs, syncobjs, fpmimetypes, custhttpapp, inifiles, types,
  fpwebproxy, webutil, fpwebfile, httproute, httpdefs, dirw, Pas2JSFSCompiler,
  Pas2JSCompilerCfg, ssockets, fpdebugcapturesvc, fpsimpleserver;

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

  { THTTPCompilerApplication }

  THTTPCompilerApplication = Class(TFPSimpleServerApplication)
  private
    FProjectFile: String;
    FProjectConfigFile: String;
    FStatusLock : TCriticalSection;
    FWatch: Boolean;
    FDW : TDirWatcher;
    FAPI : String;
    FStatusList : TFPObjectList;
    FCompiles : TCompiles;
    FServeOnly  : Boolean;
    procedure AddToStatus(O: TJSONObject);
    function HandleCompileOptions(aDir: String): Boolean;
    Procedure ReportBuilding(AItem : TCompileItem);
    Procedure ReportBuilt(AItem : TCompileItem);
    Procedure AddToStatus(AEntry : TDirectoryEntry; AEvents : TFileEvents);
    procedure DoStatusRequest(ARequest: TRequest; AResponse: TResponse);
    procedure DoRecompile(ARequest: TRequest; AResponse: TResponse);
    function ScheduleCompile(const aProjectFile: String; Options : TStrings = Nil): Integer;
    procedure StartWatch(ADir: String);
  protected
    // Override
    procedure ProcessOptions; override;
    procedure GetValidOptions(out aShort: String; out aLong: TStringDynArray); override;
    procedure DoReadConfigFile(const aIni : TCustomIniFile); override;
    Function GetDefaultConfigFile : string; override;
    procedure LoadMimeTypes; override;
    procedure WriteOptions; override;
    procedure RegisterRoutes; override;
  public
    Constructor Create(AOWner : TComponent); override;
    Destructor Destroy; override;
    Property Watch : Boolean Read FWatch Write FWatch;
    Property ProjectFile : String Read FProjectFile Write FProjectFile;
    Property ProjectConfigFile : String Read FProjectConfigFile Write FProjectConfigFile;
    Property ServeOnly : Boolean Read FServeOnly;
  end;


Implementation

Const
  SConfig = 'Config';
  KeyServeOnly = 'ServeOnly';
  KeyWatch = 'Watch';
  KeyProject = 'Project';
  KeyProjectConfig = 'ProjectConfig';

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

procedure THTTPCompilerApplication.LoadMimeTypes;
begin
  Inherited;
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

function THTTPCompilerApplication.GetDefaultConfigFile: string;
begin
  Result:='compileserver.ini';
end;

procedure THTTPCompilerApplication.WriteOptions;
begin
  inherited WriteOptions;
  {AllowWriteln}
  Writeln('-s --simpleserver        Only serve files, do not enable compilation.');
  Writeln('-w --watch               Watch directory for changes');
  {AllowWriteln-}
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
    Log(etError,'Refusing compile of file "%s" using config file "%s"',[AProjectFile, ProjectConfigFile]);
    Exit(nErrTooManyThreads);
    end;
  CI:=FCompiles.Add as TCompileItem;
  Log(etInfo,'Scheduling compile ID %d of file "%s" using config file "%s"',[CI.ID,AProjectFile, ProjectConfigFile]);
  CI.BaseDir:=BaseDir;
  CI.FileName:=AProjectFile;
  CI.ConfigFile:=ProjectConfigFile;
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
    end;
  if Watch then
    begin
    if (ProjectFile='') then
      Log(etWarning,'No project file specified, disabling watch.')
    else
      StartWatch(aDir);
    end;
  Result:=True;
end;

procedure THTTPCompilerApplication.DoReadConfigFile(const aIni: TCustomIniFile);

begin
  With aIni do
    begin
    FServeOnly:=ReadBool(SConfig,KeyServeOnly,FServeOnly);
    FWatch:=ReadBool(SConfig,KeyWatch,FWatch);
    FProjectFile:=ReadString(SConfig,KeyProject,FProjectFile);
    FProjectConfigFile:=ReadString(SConfig,KeyProject,FProjectConfigFile);
    if (FProjectConfigFile='') and (FProjectFile<>'') then
      FProjectConfigFile:=ChangeFileExt(FProjectFile,'.cfg');
    end;
end;

procedure THTTPCompilerApplication.ProcessOptions;

begin
  Inherited;
  FServeOnly:=FServeOnly or HasOption('s','serve-only');
  if not ServeOnly then
    if not HandleCompileOptions(BaseDir) then

end;

procedure THTTPCompilerApplication.GetValidOptions(out aShort: String; out aLong: TStringDynArray);

var
  len : integer;

begin
  Inherited GetValidOptions(aShort,aLong);
  aShort:=aShort+'wP:';
  Len:=Length(aLong);
  SetLength(aLong,Len+2);
  aLong[Len]:='watch';
  aLong[Len+1]:='project';
end;

procedure THTTPCompilerApplication.RegisterRoutes;

begin
  if not ServeOnly then
    begin
    httprouter.RegisterRoute('$sys/compile',rmPost,@DoRecompile);
    httprouter.RegisterRoute('$sys/status',rmGet,@DoStatusRequest);
    end;
  Inherited;
end;



end.
