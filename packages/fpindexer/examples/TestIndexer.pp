program TestIndexer;

{ $define usefirebird}
{ $define usemem}
{$mode objfpc}{$H+}
{$IFDEF UNIX}
  {$linklib pthread}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads,
  {$ENDIF} {$ENDIF}
  {$ifdef usefirebird}
    ibase60dyn,SQLDBIndexDB, fbIndexdb,
  {$else}
    {$ifdef usemem}
      memindexdb,
    {$else}
      SQLIteIndexDB,
    {$endif}
  {$endif}
   fpIndexer,
  //indexer readers
  IReaderTXT, IReaderPAS, IReaderHTML, fpTextCat;

Type

  { TProgressLog }

  TProgressLog = Class(TObject)
    procedure DoLog(Sender : TObject; Const ACurrent,ACount : Integer; Const AURL : String);
  end;

{$ifdef usefirebird}
  function SetupDB : TCustomIndexDB;
  var
    IB: TFBIndexDB;
  begin
    IB := TFBIndexDB.Create(nil);
    try
      IB.DatabasePath := '/home/firebird/index.fb';
      IB.UserName := 'SYSDBA';
      IB.Password := 'masterkey';
      if not FileExists(IB.DatabasePath) then
        IB.CreateDB
      else
      begin
        IB.Connect;
        IB.CreateIndexerTables;
      end;
    except
      FreeAndNil(IB);
      Raise;
    end;
    Result:=IB;
  end;
{$else}
  {$ifdef usemem}
    Function SetupDB : TCustomIndexDB;
    Var
      FI : TFileIndexDB;
    begin
      FI:=TFileIndexDB.Create(Nil);
      FI.FileName:='index.dat';
      FI.Connect;
      FI.WriteOnCommit:=True;;
      Result:=FI;
    end;
  {$else}
    Function SetupDB : TCustomIndexDB;
    Var
      SB: TSQLIteIndexDB;
    begin
      SB := TSQLIteIndexDB.Create(nil);
      SB.FileName := 'index.db';
      if not FileExists(SB.FileName) then
        SB.CreateDB
      else
      begin
        SB.Connect;
        SB.CreateIndexerTables;
      end;
      Result:=SB;
    end;
  {$endif}
{$endif}

Procedure Testindex(ADir : String);
var
  Indexer: TFPIndexer; //indexes files
  start: TDateTime;
  n: int64;
  endtime: TDateTime;
  Logger : TProgressLog;
begin
  //SetHeapTraceOutput('heap.trc');
  start := Now;
  Indexer := TFPIndexer.Create(Nil);
  try
    Indexer.Database:=SetupDB;
    //setup parameters for indexing
    if (ADir<>'') then
      Indexer.SearchPath:=ADir
    else
{$ifdef unix}
      Indexer.SearchPath := '/home/michael/fpc/docs/fcl';
{$else}
      Indexer.SearchPath := 'C:\fcl';
{$endif}
    Indexer.FileMask := '*.pas;*.html;readme.txt'; //semicolon separated list
    Indexer.SearchRecursive := True;
    Indexer.DetectLanguage := False;
    IgnoreListManager.LoadIgnoreWordsFromFile('english','english.txt');
    indexer.Language:='english';
    Indexer.UseIgnoreList:=true;
    Logger := TProgressLog.Create;
    try
      Indexer.OnProgress:=@Logger.DoLog;
      n := Indexer.Execute(True);
    finally
      Logger.Free;
    end;
    //execute the search

    endtime := Now;
    if N <> 0 then
      writeln('indexing succesfull')
    else
      writeln('error indexing.');
    writeln(Format('done in %.1f sec.', [(endtime - start) * 24 * 3600]));
  finally
    Indexer.Database.free;
    FreeAndNil(Indexer);
  end;
end;

{ TProgressLog }

procedure TProgressLog.DoLog(Sender: TObject; const ACurrent, ACount: Integer;
  const AURL: String);
begin
  Writeln((ACurrent/ACount*100):5:2,'% : ',ACurrent,'/',ACount,' : ',AURL);
end;

begin
  TestIndex(ParamStr(1));
end.

