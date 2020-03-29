program docindexer;

{$mode objfpc}{$H+}
{$IFDEF UNIX}
  {$linklib pthread}
{$ENDIF}

uses
  cwstring, cthreads, SysUtils, Classes, DateUtils, sqldb, SQLDBindexDB, FBindexDB, sqliteindexdb,  pgindexdb, memindexdb, fpIndexer, inifiles,
  // indexer readers
  IReaderTXT, IReaderPAS, IReaderHTML, CustApp;

Type
  { TDocIndexerApplication }

  TDocIndexerApplication = class(TCustomApplication)
  Private
    FDirs : TStringArray;
    FCreateDB : Boolean;
    FEmptyDB : Boolean;
    FStripPath,
    FLanguage,
    FIgnoreList,
    FConfig : String;
    FCommitFiles,
    FLogSQL : Boolean;
    FCodePage : TSystemCodePage;
  Protected
    Procedure WriteLog(Const Msg : String); virtual;
    Procedure WriteLog(Const Fmt : String; Const Args : Array of Const);
    procedure IndexLog(Sender : TObject; Const ACurrent,ACount : Integer; Const AURL : UTF8String);
    Procedure DBHook(Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String);
    function ParseOptions: Boolean; virtual;
    function SetupDB : TCustomIndexDB; virtual;
    procedure CreateDB(aDB : TCustomIndexDB);virtual;
    procedure ClearDB(aDB : TCustomIndexDB);virtual;
    procedure DoIndex(aDB: TCustomIndexDB);virtual;
    procedure Usage(const Msg: String);virtual;
    Procedure DoRun; override;
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

procedure TDocIndexerApplication.CreateDB(aDB : TCustomIndexDB);

begin
  WriteLog('Creating database');
  aDB.CreateDB;
end;

procedure TDocIndexerApplication.ClearDB(aDB: TCustomIndexDB);
begin
  WriteLog('Clearing database tables');
  aDB.CreateIndexerTables;
end;

function TDocIndexerApplication.SetupDB : TCustomIndexDB;

Const
  SDatabase = 'Database';
  KeyHostName = 'HostName';
  KeyDatabaseName = 'DatabaseName';
  KeyUser = 'User';
  KeyPassword = 'Password';
  KeyType = 'Type';

  Procedure ConfigSQLDB(DB : TSQLDBIndexDB; aIni : TInifile);

  begin
    DB.HostName:= aIni.ReadString(SDatabase,KeyHostName,DB.HostName);
    DB.DatabasePath := aIni.ReadString(SDatabase,KeyDatabaseName,DB.DatabasePath);
    DB.UserName := aIni.ReadString(SDatabase,KeyUser,DB.UserName);
    DB.Password := aIni.ReadString(SDatabase,KeyPassword,DB.Password);
  end;

  Procedure ConfigSQLIte(SDB : TSQLiteIndexDB; aIni : TInifile);

  begin
    SDB.FileName := aIni.ReadString(SDatabase,KeyDatabaseName,SDB.FileName);
  end;

  Procedure ConfigFile(FDB : TFileIndexDB; aIni : TInifile);

  begin
    FDB.FileName := aIni.ReadString(SDatabase,KeyDatabaseName,FDB.FileName);
  end;

Var
  Ini : TIniFile;
  DB : TSQLDBIndexDB;
  SDB : TSQLiteIndexDB;
  FDB :  TFileIndexDB;

begin
  if FLogSQL then
    GlobalDBLogHook:=@DBHook;
  Result:=nil;
  Ini:=TIniFile.Create(FConfig);
  try
    Case lowercase(Ini.ReadString(SDatabase,KeyType,'PostGres')) of
      'postgres' :
          begin
          DB := TPGIndexDB.Create(nil);
          ConfigSQLDB(DB,Ini);
          Result:=DB;
          end;
      'firebird' :
          begin
          DB := TFBIndexDB.Create(nil);
          ConfigSQLDB(DB,Ini);
          Result:=DB;
          end;
      'sqlite' :
          begin
          SDB := TSQLiteIndexDB.Create(nil);
          ConfigSQLite(SDB,Ini);
          Result:=SDB;
          end;
      'file' :
          begin
          FDB := TFileIndexDB.Create(nil);
          ConfigFile(FDB,Ini);
          Result:=FDB;
          end;
    else
      Raise Exception.CreateFmt('Unknown database type: "%s" ',[Ini.ReadString(SDatabase,KeyType,'PostGres')]);
    end;
  finally
    ini.Free;
  end;
end;

Procedure TDocIndexerApplication.DoIndex(aDB  : TCustomIndexDB);

var
  Indexer: TFPIndexer; //indexes files
  start: TDateTime;
  Dn,n: int64;
  endtime: TDateTime;
  D : String;

begin
  //SetHeapTraceOutput('heap.trc');
  start := Now;
  Indexer := TFPIndexer.Create(Nil);
  try
    Indexer.CodePage:=FCodePage;
    Indexer.Database:=aDB;
    //setup parameters for indexing
    Indexer.FileMask := '*.pas;*.html;readme.txt'; //semicolon separated list
    Indexer.SearchRecursive := True;
    Indexer.DetectLanguage := False;
    if (FIgnoreList<>'') then
      IgnoreListManager.LoadIgnoreWordsFromFile(FLanguage,FIgnoreList);
    indexer.Language:=FLanguage;
    Indexer.UseIgnoreList:=true;
    Indexer.CommitFiles:=FCommitFiles;
    Indexer.StripPath:=FStripPath;
    Indexer.OnProgress:=@IndexLog;
    N:=0;
    DN:=0;
    For D in FDirs do
      begin
      inc(DN);
      IndexLog(Self,-1,-1,Format('Treating directory %d of %d: %s',[DN,Length(FDirs),D]));
      Indexer.SearchPath:=D;
      //execute the search
      N := N+Indexer.Execute(False);
      end;
    endtime := Now;
    if N <> 0 then
      WriteLog('Endexing succesful')
    else
      WriteLog('Error indexing or no words found...');
    WriteLog(Format('Done, indexed %d words in %d directories in %d sec.', [N,Length(FDirs),SecondsBetween(endtime,start)]));
  finally
    FreeAndNil(Indexer);
  end;
end;

Procedure TDocIndexerApplication.Usage(Const Msg : String);

begin
  If (Msg<>'') then
    Writeln(Msg);
  ExitCode:=Ord(Msg<>'')
end;

Function TDocIndexerApplication.ParseOptions : Boolean;

Var
  Enc : String;

begin
  Result:=True;
  FConfig:=GetOptionValue('c','config');
  If (FConfig='') then
    begin
    Usage('Need database connection configuration file');
    Exit(False);
    end;
  FDirs:=GetOptionValues('d','directory');
  if (Length(FDirs)=0) then
    begin
    SetLength(FDirs,1);
    FDirs[0]:='.';
    end;
  FCreateDB:=HasOption('r','createdb');
  FEmptyDB:=(Not FCreateDB) and HasOption('e','cleardb');
  FLogSQL:=HasOption('q','querylog');
  FCommitFiles:=HasOption('m','commit-files');
  FLanguage:=GetOptionValue('l','language');
  if FLanguage='' then
    FLanguage:='english';
  FIgnoreList:=GetOptionValue('i','ignore');
  Enc:=getOptionValue('p','codepage');
  FStripPath:=GetOptionValue('s','strip');
  if Enc='' then
    FCodePage:=CP_UTF8
  else
    begin
    FCodePage := CodePageNameToCodePage(Enc);
    if (FCodePage = $FFFF) then
      begin
      Usage('Invalid or unsupported encoding: '+Enc);
      Exit(False);
      end;
    end;
end;

procedure TDocIndexerApplication.DoRun;

Var
  S : String;
  DB : TCustomIndexDB;

begin
  Terminate;
  S:=Checkoptions('hd:reqmc:l:i:p:s:',['help','directory','createdb','cleardb','querylog','commit-files','config','language','ignore-list','codepage','strip']);
  if (S<>'') or HasOption('h','help') then
    begin
    Usage(S);
    exit;
    end;
  If not ParseOptions then
    exit;
  DB:=SetupDB;
  try
    If FCreateDB then
      DB.CreateDB
    else
      begin
      DB.Connect;
      if FEmptyDB then
        ClearDB(DB);
      end;
    DoIndex(DB);
  finally
    DB.Free;
  end;
end;

constructor TDocIndexerApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  StopOnException:=True;
  FCodePage:=CP_UTF8;
end;

procedure TDocIndexerApplication.WriteLog(const Msg: String);
begin
  Writeln(Msg);
end;

procedure TDocIndexerApplication.WriteLog(const Fmt: String; const Args: array of const);
begin
  WriteLog(Format(Fmt,Args));
end;

procedure TDocIndexerApplication.IndexLog(Sender: TObject; const ACurrent, ACount: Integer; const AURL: UTF8String);
begin
  if ACurrent=-1 then
    WriteLog(AURL)
  else
    WriteLog('%5.2f%% [%d/%d] : %s',[(ACurrent/ACount*100),ACurrent,ACount,AURL]);
end;

procedure TDocIndexerApplication.DBHook(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);

Var
  S : String;

begin
  Str(EventType,S);
  WriteLog('SQL [%s] : %s',[S,Msg]);
end;

begin
  with TDocIndexerApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.

