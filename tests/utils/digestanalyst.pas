unit digestanalyst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, teststr, testu, tresults, dbtests;

Type
  TDigestConfig = record
    databasename: string;
    host: string;
    username: string;
    password: string;
    port: string;
    logfile: string;
    longlogfile : string;
    os: string;
    cpu: string;
    category: string;
    version: string;
    date: string;
    submitter: string;
    machine: string;
    config : string;
    description : string;
    testsrcdir: string;
    relsrcdir: string;
    verbose: string;
    sql: string;
  end;

  TTestRunData = Record
    Date : TDateTime;
    CompilerDate,
    CompilerFullVersion,
    SvnCompilerRevision,
    SvnTestsRevision,
    SvnRTLRevision,
    SvnPackagesRevision : String;
    CPUID : Integer;
    OSID  : Integer;
    VersionID  : Integer;
    CategoryID : Integer;
    RunID : Integer;
    ConfigID : Integer;
  end;

  { TDBDigestAnalyzer }

  TDBDigestAnalyzer = Class(TObject)
    FDB : TTestDB;
    LongLogFile : TStrings;
    StatusCount : Array[TTestStatus] of Integer;
    UnknownLines : integer;
    UseLongLog : Boolean;
    FCurLongLogLine : Integer;
    constructor Create(aDB : TTestDB);
    function CheckIDs(aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    function GetExecuteLog(Line, FN: String): String;
    function GetIDs(const aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    procedure Processfile(const aFileName: String; const aData: TTestRunData);
    procedure UpdateTestRun(const aData: TTestRunData);
    procedure UpdateTestRunBefore(const aConfig: TDigestConfig; const aData: TTestRunData);
    procedure Analyse(aConfig : TDigestConfig);
  private
    function GetContentsFromLongLog(Line: String): String;
    function GetLog(Line, FN: String): String;
  end;


implementation

constructor TDBDigestAnalyzer.Create(aDB: TTestDB);
begin
  FDB:=aDB;
end;

function TDBDigestAnalyzer.CheckIDs(aConfig : TDigestConfig; var aData : TTestRunData): Boolean;

begin
  If aData.CategoryID=-1 then
    aData.CategoryID:=1;
  // Checks
  If aData.CPUID=-1 then
    Verbose(V_Error,'NO ID for CPU "'+aConfig.CPU+'" found.');
  If aData.OSID=-1 then
    Verbose(V_Error,'NO ID for OS "'+aConfig.OS+'" found.');
  If aData.VersionID=-1 then
    Verbose(V_Error,'NO ID for version "'+aConfig.Version+'" found.');
end;


procedure TDBDigestAnalyzer.UpdateTestRunBefore(const aConfig : TDigestConfig; const aData : TTestRunData);

var
  qry : string;

begin
  { Add known infomration at start }
  qry:=format('UPDATE TESTRUN SET TU_SUBMITTER=''%s'', TU_MACHINE=''%s'', TU_COMMENT=''%s'', TU_DATE=''%s''',[aConfig.Submitter,aConfig.Machine,aConfig.Comment,TTestDB.SqlDate(aData.Date)]);
  qry:=qry+' WHERE TU_ID='+format('%d',[aData.RunID]);
  FDB.ExecuteQuery(Qry,False);
end;

procedure TDBDigestAnalyzer.Analyse(aConfig: TDigestConfig);

var
  lData : TTestRunData;

begin
  lData:=Default(TTestRunData);
  if (aConfig.longlogfile<>'') and FileExists(aConfig.longlogfile) then
    begin
    LongLogFile:=TStringList.Create;
    LongLogFile.LoadFromFile(aConfig.longlogfile);
    end;
  if not GetIDS(aConfig,lData) then
    exit;
  UpdateTestRunBefore(aConfig,lData);
  ProcessFile(aConfig.logfile,lData);
  UpdateTestRun(lData);
end;

function TDBDigestAnalyzer.GetIDs(const aConfig : TDigestConfig; var aData : TTestRunData): Boolean;


begin
  Result:=False;
  aData:=Default(TTestRunData);
  aData.CPUID := FDB.GetCPUID(aConfig.CPU);
  aData.OSID  := FDB.GetOSID(aConfig.OS);
  aData.VersionID  := FDB.GetVersionID(aConfig.Version);
  aData.CategoryID := FDB.GetCategoryID(aConfig.Category);
  aData.PlatformID := FDB.GetCategoryID(aConfig.Category);
  If (Round(aData.Date)=0) then
    aData.Date:=Now;
  Result:=CheckIDS(aConfig,aData);
  if not Result then
    Exit;
  aData.RunID:=FDB.GetRunID(aData.OSID,aData.CPUID,aData.VersionID,aData.Date);
  If (aData.RunID<>-1) then
    FDB.CleanTestRun(aData.RunID)
  else
    begin
    aData.RunID:=FDB.AddRun(aData.OSID,aData.CPUID,aData.VersionID,aData.CategoryID,aData.Date);
    Result:=aData.RunID<>-1;
    if not Result then
      begin
      Verbose(V_Error,'Could not insert new testrun record!');
      exit;
      end;
    end;
end;

Procedure ExtractTestFileName(Var Line : string);

Var I : integer;

begin
  I:=Pos(' ',Line);
  If (I<>0) then
    Line:=Copy(Line,1,I-1);
end;

Function AnalyseLine(Var Line : string; Var Status : TTestStatus) : Boolean;

Var
  TS : TTestStatus;

begin
  Result:=False;
  For TS:=FirstStatus to LastStatus do
    begin
    Result:=Pos(StatusText[TS],Line)=1;
    If Result then
      begin
      Status:=TS;
      Delete(Line,1,Length(StatusText[TS]));
      ExtractTestFileName(Line);
      Break;
      end;
    end;
end;

(*

ConfigAddStrings : Array [TConfigAddOpt] of string = (
  'compilerdate',
  'compilerfullversion',
  'svncompilerrevision',
  'svntestsrevision',
  'svnrtlrevision',
  'svnpackagesrevision'
 );

ConfigAddCols : Array [TConfigAddOpt] of string = (
  'TU_COMPILERDATE',
  'TU_COMPILERFULLVERSION',
  'TU_SVNCOMPILERREVISION',
  'TU_SVNTESTSREVISION',
  'TU_SVNRTLREVISION',
  'TU_SVNPACKAGESREVISION'
 );

*)



const
   SeparationLine = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>';

Function TDBDigestAnalyzer.GetContentsFromLongLog(Line : String) : String;

  Function GetLongLogLine : String;
  begin
    Result:=LongLogFile[FCurLongLogLine];
    Inc(FCurLongLogLine);
  end;

  Function HaveLongLogLine : Boolean; inline;
  begin
    Result:=FCurLongLogLine<LongLogFile.Count;
  end;

var
  S : String;
  IsFirst, IsFound : boolean;

begin
  Result:='';
  IsFirst:=true;
  IsFound:=false;
  While HaveLongLogLine do
    begin
      S:=GetLongLogLine;
      if FCurLongLogLine=1 then
        begin
          { At start of file there is a separation line }
          if (pos(Line,S)=0) and (pos(SeparationLine,S)>=1) then
            S:=GetLongLogLine
        end;
      if pos(Line,S)=1 then
        begin
          IsFound:=true;
          while HaveLongLogLine do
            begin
              S:=GetLongLogLine;
              { End of file marker }
              if (Not HaveLongLogLine) or (pos(SeparationLine,S)=1) then
                exit;
              if length(Result)<MaxLogSize then
                Result:=Result+S+LineEnding;
              if pos(SeparationLine,S)>1 then
                exit;
            end;
        end
      else if IsFirst then
        begin
          Verbose(V_Warning,'Line "'+Line+'" not found as next "'+S+'"');
          IsFirst:=false;
        end;
    end;
  if not IsFound then
    begin
    Verbose(V_Warning,'Line "'+Line+'" not found');
    FCurlongLogLine:=0; // Reset
    end;
end;

Function TDBDigestAnalyzer.GetLog(Line, FN : String) : String;

begin
  if UseLongLog then
    begin
      Result:=GetContentsFromLongLog(Line);
      exit;
    end;
  FN:=ChangeFileExt(FN,'.log');
  { packages tests have ../ replaced by root/ }
  if not FileExists(FN) and (Copy(FN,1,3)='../') then
    FN:='root/'+Copy(FN,4,length(FN));
  If FileExists(FN) then
    Result:=GetFileContents(FN)
  else
    begin
      Verbose(V_Warning,'File "'+FN+'" not found');
      Result:='';
    end;
end;

function TDBDigestAnalyzer.GetExecuteLog(Line, FN: String): String;

begin
  if UseLongLog then
    begin
      Result:=GetContentsFromLongLog(Line);
      exit;
    end;
  FN:=ChangeFileExt(FN,'.elg');
  { packages tests have ../ replaced by root/ }
  if not FileExists(FN) and (Copy(FN,1,3)='../') then
    FN:='root/'+Copy(FN,4,length(FN));
  If FileExists(FN) then
    Result:=GetFileContents(FN)
  else
    begin
      Verbose(V_Warning,'File "'+FN+'" not found');
      Result:='';
    end;
end;

procedure TDBDigestAnalyzer.Processfile(const aFileName: String; const aData: TTestRunData);

var
  logfile : text;
  fullline,line,prevLine : string;
  TS,PrevTS : TTestStatus;
  ID,PrevID : integer;
  Testlog : string;
  count_test : boolean;
begin
  AssignFile(logfile,aFileName);
  PrevId:=-1;
  PrevLine:='';
  count_test:=false;
  PrevTS:=low(TTestStatus);
{$i-}
  reset(logfile);
  if ioresult<>0 then
    begin
    Verbose(V_Error,'Unable to open log file'+aFileName);
    exit;
    end;
{$i+}
  while not eof(logfile) do
    begin
    readln(logfile,line);
    fullline:=line;
    ts:=stFailedToCompile;
    If AnalyseLine(line,TS) then
      begin
      Verbose(V_NORMAL,'Analysing result for test '+Line);
      If Not ExpectRun[TS] then
        begin
        ID:=FDB.RequireTestID(Line);
        if (PrevID<>-1) and (PrevID<>ID) then
          begin
            { This can only happen if a Successfully compiled message
              is not followed by any other line about the same test }
            TestLog:='';
            FDB.AddTestResult(PrevID,aData.RunId,ord(PrevTS),
              TestOK[PrevTS],TestSkipped[PrevTS],TestLog,count_test);
            Verbose(V_Warning,'Orphaned test: "'+prevline+'"');
          end;
        PrevID:=-1;
        If (ID<>-1) then
          begin
          If Not (TestOK[TS] or TestSkipped[TS]) then
            begin
              TestLog:=GetExecuteLog(Fullline,Line);
              if pos(failed_to_compile,TestLog)=1 then
                TestLog:=GetLog(Fullline,Line);
            end
          else
            TestLog:='';
          { AddTestResult can fail for test that contain %recompile
            as the same }
          if FDB.AddTestResult(ID,aData.RunID,Ord(TS),TestOK[TS],
               TestSkipped[TS],TestLog,count_test) <> -1 then
            begin
              if count_test then
                Inc(StatusCount[TS])
              else
                Verbose(V_Debug,'Test: "'+line+'" was updated');
            end
          else
            begin
              Verbose(V_Warning,'Test: "'+line+'" already registered');
            end;

          end;
        end
      else
        begin
          Inc(StatusCount[TS]);
          PrevTS:=TS;
          PrevID:=FDB.RequireTestID(line);
          PrevLine:=line;
        end;

      end
    else
      begin
        Inc(UnknownLines);
        Verbose(V_Warning,'Unknown line: "'+line+'"');
      end;
    end;
  close(logfile);
end;

procedure TDBDigestAnalyzer.UpdateTestRun(const aData : TTestRunData);

var
   i : TTestStatus;
   qry : string;

begin
  qry:='UPDATE TESTRUN SET ';
  for i:=low(TTestStatus) to high(TTestStatus) do
    qry:=qry+format('%s=%d, ',[SQLField[i],StatusCount[i]]);
  if aData.CompilerDate<>'' then
    qry:=qry+format('%s=''%s'', ',['TU_COMPILERDATE',TTestDB.EscapeSQL(aData.CompilerDate)]);
  if aData.CompilerFullVersion<>'' then
    qry:=qry+format('%s=''%s'', ',['TU_COMPILERFULLVERSION',TTestDB.EscapeSQL(aData.CompilerFullVersion)]);
  if aData.SvnCompilerRevision<>'' then
    qry:=qry+format('%s=''%s'', ',['TU_SVNCOMPILERREVISION',TTestDB.EscapeSQL(aData.SvnCompilerRevision)]);
  if aData.SvnTestsRevision<>'' then
    qry:=qry+format('%s=''%s'', ',['TU_SVNTESTSREVISION',TTestDB.EscapeSQL(aData.SvnTestsRevision)]);
  if aData.SvnRTLRevision<>'' then
    qry:=qry+format('%s=''%s'', ',['TU_SVNRTLREVISION',TTestDB.EscapeSQL(aData.SvnRTLRevision)]);
  if aData.SvnPackagesRevision<>'' then
    qry:=qry+format('%s=''%s'', ',['TU_SVNPACKAGESREVISION',TTestDB.EscapeSQL(aData.SvnPackagesRevision)]);
  qry:=qry+' WHERE TU_ID='+format('%d',[aData.RunID]);
  FDB.ExecuteQuery(Qry,False);
end;


end.

