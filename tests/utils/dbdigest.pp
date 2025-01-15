{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This program inserts the last tests run 
    into TESTSUITE database.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
{$ifndef win32}
  {$linklib pthread}
{$endif}

program dbdigest;

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  types, classes, custapp, sysutils, inifiles, teststr, testu, tresults, dbtests, digestanalyst;

Type

  { TThreadTask }

  TThreadTask = Class(TObject)
    CfgFileName : string;
    Config: TDigestConfig;
    Data: TTestRunData;
    Constructor Create(const aFileName : String; const aConfig : TDigestConfig; aData : TTestRunData);
  end;

  { TDBDigestApplication }

  TDBDigestApplication = class(TCustomApplication)
  Const
     ShortOpts =
      'd'+ {  DatabaseName }
      'h'+ {  Host }
      'u'+ {  UserName }
      'p'+ {  Password }
      'P'+ {  Port }
      'l'+ {  LogFile }
      'L'+ {  LongLogFile }
      'o'+ {  OS }
      'c'+ {  CPU }
      'a'+ {  Category }
      'v'+ {  Version }
      't'+ {  Date }
      's'+ {  Submitter }
      'm'+ {  Machine }
      'C'+ {  Comment }
      'S'+ {  TestSrcDir }
      'r'+ {  RelSrcDir }
      'T'+ {  ThreadList }
      'j'+ {  ThreadCount }
      'V'+ {  Verbose }
      'Q'  {  SQL }
     ;

    LongOpts : Array of string = (
      'databasename',
      'host',
      'username',
      'password',
      'port',
      'logfile',
      'longlogfile',
      'os',
      'cpu',
      'category',
      'version',
      'date',
      'submitter',
      'machine',
      'comment',
      'testsrcdir',
      'relsrcdir',
      'threadlist',
      'threadcount',
      'verbose',
      'sql',
      'compilerdate',
      'compilerfullversion',
      'svncompilerrevision',
      'svntestsrevision',
      'svnrtlrevision',
      'svnpackagesrevision'
    );

  private
    FTasks : TThreadList;
    FMaxThreads : Integer;
    FThreadCount : Integer;
    // Process the command line. Return true if we can continue
    function ProcessCommandLine(var aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    // Check the names of the log files, expanding them if needed.
    function CheckConfigFiles(lCfg: String; var lData: TTestRunData): Boolean;
    // Extract a date.
    class function ExtractDate(aValue: string): TDateTime;
    // Analyse a log file (i.e. one dbdigest.cfg file)
    procedure Analyze(const aConfig: TDigestConfig; const aData: TTestRunData);
    // process a config file (dbdigest.cfg file);
    procedure ProcessConfigfile(const aFileName: String; var aConfig: TDigestConfig; var aData: TTestRunData);
    // process a single option. Adapt aConf,aData as needed. Return false if the option was not recognized.
    function ProcessOption(const aOption: String; aValue: String; var aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    // Read /etc/dbdigest.ini for database configuration.
    procedure ReadSystemDBConfig(var aConfig: TDigestConfig);
    // In thread mode, create a list of tasks.
    function CreateTaskList(const aBaseConfig: TDigestConfig; const aBaseData: TTestRunData): boolean;
    // Callback when a task is done. Checks to see if additional threads must be started.
    procedure TaskDone(Sender: TObject);
    // Wait for all tasks & threads to terminate.
    procedure WaitForThreads;
    // Start as many threads as allowed, up to task count.
    procedure StartThreads;
  protected
    // Run
    procedure DoRun; override;
    // Print usage message.
    procedure Usage(const aMsg: String);
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TProcessFileThread }

  TProcessFileThread = class(TThread)
  Private
    FTask : TThreadTask;
    FApp : TDBDigestApplication;
  Public
    Constructor Create(aApp : TDBDigestApplication; const aTask : TThreadTask; aOnTerminate : TNotifyEvent);
    Destructor Destroy; override;
    Procedure Execute; override;
  end;

{ TThreadTask }

constructor TThreadTask.Create(const aFileName: String; const aConfig: TDigestConfig; aData: TTestRunData);
begin
  CfgFileName:=aFileName;
  Config:=aConfig;
  Data:=aData;
end;

{ TProcessFileThread }

constructor TProcessFileThread.Create(aApp: TDBDigestApplication; const aTask: TThreadTask; aOnTerminate: TNotifyEvent);
begin
  FTask:=aTask;
  FApp:=aApp;
  Self.OnTerminate:=aOnTerminate;
  Inherited create(False);
end;

destructor TProcessFileThread.Destroy;
var
  lPrefix : String;
  lCfg : String;
begin
  try
    lCfg:=FTask.CfgFileName;
    lPrefix:='['+IntToStr(PtrInt(GetCurrentThreadId))+' - '+lCfg+'] ';
    FreeAndNil(FTask);
    Verbose(V_DEBUG,lPrefix+' task destroyed ');
  except
    On e : Exception do
      Verbose(V_WARNING,lPrefix+Format('Error %s during processing of "%s": %s',[E.ClassName,lCfg,E.Message]));
  end;
  inherited Destroy;
end;

procedure TProcessFileThread.Execute;
var
  lPrefix:String;

begin
  try
    lPrefix:='['+IntToStr(PtrInt(GetCurrentThreadId))+' - '+FTask.CfgFileName+'] ';
    FApp.Analyze(FTask.Config,FTask.Data);
    Writeln(IntToStr(PtrInt(GetCurrentThreadId))+'Thread done');
  except
    On e : Exception do
      Verbose(V_WARNING,lPrefix+Format('Error %s during processing of "%s": %s',[E.ClassName,FTask.CfgFileName,E.Message]));
  end;
end;

class function TDBDigestApplication.ExtractDate(aValue: string): TDateTime;

var
  year,month,day,min,hour : word;

begin
  if (Length(avalue)=12) or (Length(avalue)=8) then
    begin
      year:=StrToInt(Copy(avalue,1,4));
      month:=StrToInt(Copy(avalue,5,2));
      day:=StrToInt(Copy(aValue,7,2));
      if Length(avalue)=12 then
        begin
        hour:=StrToInt(Copy(aValue,9,2));
        min:=StrToInt(Copy(aValue,11,2));
        end;
      Result:=EncodeDate(year,month,day)+EncodeTime(hour,min,0,0);
    end
  else
    Verbose(V_Error,'Error in date format, use YYYYMMDDhhmm');
end;

function TDBDigestApplication.ProcessOption(const aOption: String; aValue: String; var aConfig: TDigestConfig;
  var aData: TTestRunData): Boolean;

begin
  Result:=True;
  Verbose(V_DEBUG,'Processing option: '+aOption);
  Case aOption of
    'd','databasename' : aConfig.databasename:=aValue;
    'h','host' : aConfig.host:=aValue;
    'u','username': aConfig.username:=aValue;
    'p','password': aConfig.password:=aValue;
    'P','port': aConfig.port:=StrToIntDef(aValue,0);
    'l','logfile': aData.logfile:=aValue;
    'L','longlogfile': aData.longlogfile:=aValue;
    'o','os': aData.os:=aValue;
    'c','cpu': aData.cpu:=aValue;
    'a','category': aData.category:=aValue;
    'v','version': aData.version:=aValue;
    't','date': aData.date:=ExtractDate(aValue);
    's','submitter': aData.submitter:=aValue;
    'm','machine': aData.machine:=aValue;
    'C','comment': aData.config:=aValue;
    'D','description': aData.description:=aValue;
    'S','testsrcdir': aConfig.testsrcdir:=aValue;
    'r','relsrcdir': aConfig.relsrcdir:=aValue;
    'V','verbose': DoVerbose:=True;
    'T','threadlist' : ; // treated elsewhere
    'j','threadcount' : ; // treated elsewhere
    //  'S','sql': aConfig.sql:=aValue;
    'compilerdate': aData.CompilerDate:=aValue;
    'compilerfullversion': aData.CompilerFullVersion:=aValue;
    'svncompilerrevision': aData.CompilerRevision:=aValue;
    'svntestsrevision': aData.TestsRevision:=aValue;
    'svnrtlrevision': aData.RTLRevision:=aValue;
    'svnpackagesrevision' : aData.PackagesRevision:=aValue;
  else
    Verbose(V_ERROR,'Unknown processing option: '+aOption);
  end;
end;

procedure TDBDigestApplication.ProcessConfigfile(const aFileName: String; var aConfig: TDigestConfig; var aData: TTestRunData);

Var
  Cfg : TStrings;
  aLine,S,N,V : String;
  I : Integer;

begin
  // Set the default value for old digests without RelSrcDir to the rtl/compiler
  // testsuite
  If Not FileExists(aFileName) Then
    Exit;
  Verbose(V_DEBUG,'Parsing config file: '+aFileName);
  Cfg:=TStringList.Create;
  try
    Cfg.LoadFromFile(aFileName);
    For aLine in Cfg do
      begin
      S:=Trim(aLine);
      I:=Pos('#',S);
      If I<>0 then
        S:=Copy(S,1,I-1);
      If (S<>'') then
        begin
        I:=Pos('=',S);
        if (I=0) then
          Verbose(V_ERROR,'Unknown processing option: '+S)
        else
          begin
          N:=LowerCase(Copy(S,1,I-1));
          V:=Copy(S,I+1,Length(S)-I);
          ProcessOption(N,V,aConfig,aData);
          end;
        end;
      end;

  finally
    Cfg.Free;
  end;
end;

{ TDBDigestApplication }

procedure TDBDigestApplication.Usage(const aMsg: String);

begin
  if (aMsg<>'') then
    Writeln('Error : ',aMsg);
  Writeln('Usage: ',ExeName,' [options] [test run data options]');
  Writeln('Configuration options:');
  Writeln('-H --help                         show this help');
  Writeln('-d --databasename=NAME            database name');
  Writeln('-f --config=FILENAME              config file. If not set, dbdigest.cfg is used.');
  Writeln('-h --host=HOST                    database hostname');
  Writeln('-p --password=PWD                 database user password');
  Writeln('-P --port=NNN                     database connection port');
  Writeln('-r --relsrcdir                    relative source dir');
  Writeln('-S --testsrcdir                   test source dir');
  Writeln('-u --username=USER                database user name');
  Writeln('-T --threadlist=FILE              file with configuration file names to imports.');
  Writeln('-j --threadcount=N                Number of threads to use');
  Writeln('-V --verbose                      be more verbose');
  Writeln('Test run data:');
  Writeln('-l --logfile=FILE                 set log file to analyse');
  Writeln('-L --longlogfile=FILE             set long log filename (logs of run tests)');
  Writeln('-o --os=OS                        set OS for testrun');
  Writeln('-c --cpu=CPU                      set CPU');
  Writeln('-a --category=CAT                 set category');
  Writeln('-v --version=VER                  set compiler version');
  Writeln('-t --date=DATE                    date in YYYMMDD(hhmmnn) format');
  Writeln('-s --submitter=NAME               submitter name');
  Writeln('-m --machine=NAME                 set machine name on which testsuite was run');
  Writeln('-C --compile-flags=FLAGS          set used compilation flags');
  Writeln('   --comment=FLAGS                backwards compatible way to set compilation flags (deprecated)');
  Writeln('-D --description=DESC             set config description (helpful comment)');
  Writeln('   --compilerdate=DATE            set compiler date');
  Writeln('   --compilerfullversion=VERSION  set full compiler version');
  Writeln('   --svncompilerrevision=REV      set revision of used compiler');
  Writeln('   --svntestsrevision=REV         set revision of testsuite files');
  Writeln('   --svnrtlrevision=REV           set revision of RTL');
  Writeln('   --svnpackagesrevision=REV      set revison of packages');
  Writeln('');
  Writeln('If -T is specified, no test run options may be specified');
  Writeln('');
  Writeln('The config file can contain the same options as the command-line in the form.');
  Writeln('option=value');
  Writeln('where option is the long or short version of the option');
  Writeln('comments may be included using the # character.');
  ExitCode:=Ord(aMsg<>'');
end;

constructor TDBDigestApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTasks:=TThreadList.Create;
end;

function TDBDigestApplication.ProcessCommandLine(var aConfig: TDigestConfig; var aData : TTestRunData): Boolean;

  Function MakeOpts(s : string) : string;
  var
    C : char;
  begin
    Result:='';
    For C in s do
      begin
      Result:=Result+C;
      if not (C in ['V','Q']) then
        Result:=Result+':';
      end;
  end;

  Function MakeLongOpts(s : array of string) : TStringDynArray;
  var
    I : Integer;
  begin
    Result:=['help'];
    SetLength(Result,1+Length(S));
    For I:=0 to Length(S)-1 do
      Result[1+i]:=S[I]+':'
  end;

var
  Long,ErrMsg,lValue : String;
  Short : Char;
  I : integer;
  lHas : boolean;

begin
  ErrMsg:=CheckOptions(MakeOpts(ShortOpts)+'H',MakeLongOpts(LongOpts));
  Result:=(ErrMsg='');
  if (not Result) or HasOption('H','help') then
    begin
    Usage(ErrMsg);
    Exit(false);
    end;
  I:=0;
  For Long in LongOpts do
    begin
    Inc(I);
    if I<=Length(ShortOpts) then
      begin
      Short:=ShortOpts[I];
      if Short='r' then
        Writeln('ag');
      lHas:=HasOption(Short,Long);
      lValue:=GetOptionValue(Short,Long);
      end
    else
      begin
      Short:=#0;
      lHas:=HasOption(Long);
      lValue:=GetOptionValue(Long);
      end;
    if lHas then
      ProcessOption(long,lValue,aConfig,aData);
    end;
  Result:=True;
end;

procedure TDBDigestApplication.Analyze(const aConfig : TDigestConfig; const aData : TTestRunData);

var
  lSQL : TTestSQL;
  lDigest : TDBDigestAnalyzer;
  lPrefix : string;

begin
  lDigest:=Nil;
  With aConfig do
    lSQL:=TTestSQL.create(databasename,host,username,password,port);
  try
    lSQL.ConnectToDatabase;
    if GetCurrentThreadId<>MainThreadID then
      lPrefix:='['+IntToStr(PtrInt(GetCurrentThreadId))+' - '+aData.logfile+']: '
    else
      lPrefix:='';
    lSQL.LogPrefix:=lPrefix;
    lDigest:=TDBDigestAnalyzer.Create(lSQL,lPrefix);
    lDigest.Analyse(aConfig,aData);
  finally
    lDigest.Free;
    lSQL.Free;
  end;
end;

procedure TDBDigestApplication.ReadSystemDBConfig(var aConfig : TDigestConfig);

// Keep filename in sync with algorithm in dbadd

var
  lFileName : String;
  Ini : TCustomIniFile;

begin
  lFileName:='/etc/dbdigest.ini';
  if not FileExists(lFileName) then exit;
  Ini:=TMemIniFile.Create(lFileName);
  With Ini do
    try
      aConfig.DatabaseName:=ReadString(SSection,KeyName,'testsuite');
      aConfig.Host:=ReadString(SSection,KeyHost,'localhost');
      aConfig.UserName:=ReadString(SSection,KeyUser,'');
      aConfig.Password:=ReadString(SSection,KeyPassword,'');
      aConfig.Port:=ReadInteger(SSection,KeyPort,0);
    finally
      Ini.Free;
    end;
end;

function TDBDigestApplication.CheckConfigFiles(lCfg : String; var lData : TTestRunData) : Boolean;

  function CheckFile(const aDir : String; var aFile : String) : boolean;

  var
    lExpanded : string;

  begin
    if (aFile<>'') and (aFile[1]<>'/') then
      begin
      lExpanded:=aDir+aFile;
      Verbose(V_Debug,Format('Expanding file from %s to %s',[aFile,lExpanded]));
      aFile:=lExpanded;
      end;
    Result:=FileExists(aFile);
    if not Result then
       Verbose(V_Warning,Format('file does not exist: %s',[lExpanded]));
  end;

var
  lDir : String;

begin
  lDir:=ExtractFilePath(ExpandFileName(lCfg));
  Result:=CheckFile(lDir,lData.logfile);
  if Result then
    Result:=CheckFile(lDir,lData.longlogfile);
end;


function TDBDigestApplication.CreateTaskList(const aBaseConfig: TDigestConfig; const aBaseData: TTestRunData) : boolean;

var
  lCfg,lFileName : String;
  L : TStrings;
  lConfig : TDigestConfig;
  lData : TTestRunData;
  lList : TList;


begin
  Result:=False;
  lFileName:=GetOptionValue('T','threadlist');
  if not FileExists(lFileName) then
    begin
    Verbose(V_Normal,'No such file :'+lFileName);
    Exit;
    end;
  L:=TStringList.Create;
  try
    l.LoadFromFile(lFileName);
    Result:=True;
    For lcfg in L do
      begin
      if not FileExists(lCfg) then
        begin
        Verbose(V_Warning,'No such file: '+lcfg);
        Result:=False;
        end
      else
        begin
        lConfig:=aBaseConfig;
        lData:=aBaseData;
        lList:=FTasks.LockList;
        ProcessConfigfile(lCfg,lConfig,lData);
        if CheckConfigFiles(lCfg,lData) then
          lList.Add(TThreadTask.Create(lCfg,lConfig,lData))
        else
          Result:=False;
        end;
      end;
  finally
    l.Free;
  end;
end;

procedure TDBDigestApplication.TaskDone(Sender: TObject);
begin
  InterlockedDecrement(FThreadCount);
  StartThreads;
end;

Procedure TDBDigestApplication.StartThreads;

var
  L : TList;
  lTask : TThreadTask;

begin
  L:=FTasks.LockList;
  try
    Verbose(V_DEBUG,Format('Starting tasks. Current thread count: %d remaining tasks: %d.',[FThreadCount,l.Count]));
    While (L.Count>0) and (FThreadCount<FMaxThreads) do
      begin
      lTask:=TThreadTask(L[0]);
      L.Delete(0);
      Verbose(V_DEBUG,'Starting task for '+lTask.CfgFileName);
      TProcessFileThread.Create(Self,lTask,@TaskDone);
      InterlockedIncrement(FThreadCount);
      end;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TDBDigestApplication.WaitForThreads;

var
  lDone : Boolean;
  lList : TList;

begin
  Repeat
    CheckSynchronize;
    Sleep(100);
    lList:=FTasks.LockList;
    try
      Verbose(V_DEBUG,Format('Waiting...(Todo: %d threads: %d)',[lList.Count,FThreadCount]));
      lDone:=(lList.Count=0) and (FThreadCount=0);
    finally
      FTasks.UnlockList;
    end
  until ldone;
end;

procedure TDBDigestApplication.DoRun;

var
  lConfigFile : String;
  lConfig : TDigestConfig;
  lData : TTestRunData;
begin
  Terminate;
  lConfigFile:=GetOptionValue('f','config');
  if lConfigFile='' then
    lConfigFile:='dbdigest.cfg';
  lConfig:=Default(TDigestConfig);
  lConfig.RelSrcDir:='tests/';
  ReadSystemDBConfig(lConfig);
  if not HasOption('T','threadlist') then
    begin
    lData:=Default(TTestRunData);
    ProcessConfigFile(lConfigFile,lConfig,lData);
    if ProcessCommandLine(lConfig,lData) then
      Analyze(lConfig,lData);
    end
  else
    begin
    FMaxThreads:=StrToIntDef(GetOptionValue('j','threadcount'),4);
    if ProcessCommandLine(lConfig,lData) then
      if CreateTaskList(lConfig,lData) then
        begin
        StartThreads;
        WaitForThreads;
        end;
    end;
end;

var
  Application : TDBDigestApplication;

begin
  Application:=TDBDigestApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
