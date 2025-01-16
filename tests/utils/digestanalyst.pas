unit digestanalyst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, teststr, testu, tresults, dbtests;

Type
  // Program configuration
  TDigestConfig = record
    databasename: string;
    host: string;
    username: string;
    password: string;
    port: integer;
    testsrcdir: string;
    relsrcdir: string;
    verbose: string;
    sql: string;
  end;


  { TDBDigestAnalyzer }

  TDBDigestAnalyzer = Class(TObject)
  private
    FDB : TTestSQL;
    LongLogFile : TStrings;
    UnknownLines : integer;
    UseLongLog : Boolean;
    FCurLongLogLine : Integer;
    FPrefix : String;
    // Call global verbose with prefix to message.
    procedure Verbose(aLevel : TVerboseLevel; const aMsg : string);
    // Get the execute log for a given test
    function GetExecuteLog(Line, FN: String): String;
    // Get the IDs from all config parameters: OS, Log,
    function GetIDs(const aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    // Check that all IDS needed for a test run are <>-1
    function CheckIDs(var aData: TTestRunData): Boolean;
    // process a log file.
    procedure Processfile(const aFileName: String; var aData: TTestRunData);
    // Update the test run statistics.
    procedure UpdateTestRun(const aData: TTestRunData);
    // Get contents from longlog
    function GetContentsFromLongLog(Line: String): String;
    // Get Log from file line
    function GetLog(Line, FN: String): String;
  public
    constructor Create(aDB : TTestSQL; const aPrefix : String);
    // Extract the status from a log line. Will change the log line.
    class function AnalyseLine(var Line: string; var Status: TTestStatus): Boolean;
    // Extract test filename from a log line
    class procedure ExtractTestFileName(var Line: string);
    // Analyse the file.
    procedure Analyse(aConfig : TDigestConfig; aData : TTestRunData);
    // Save test result. Return true if a NEW test result record was created (and the result must be counted)
    function SaveTestResult(var aResult: TTestResultData): Boolean;
    // DB connection to use
    property DB : TTestSQL read FDB;
  end;


implementation

constructor TDBDigestAnalyzer.Create(aDB: TTestSQL; const aPrefix: String);
begin
  FDB:=aDB;
  FPrefix:=aPrefix;
end;

procedure TDBDigestAnalyzer.Verbose(aLevel: TVerboseLevel; const aMsg: string);
begin
  testu.Verbose(aLevel,FPrefix+aMsg);
end;

function TDBDigestAnalyzer.CheckIDs(var aData : TTestRunData): Boolean;

begin
  If aData.CategoryID=-1 then
    aData.CategoryID:=1;
  Result:=(aData.CPUID<>-1) and (aData.OSID<>-1) and (aData.VersionID<>-1);
  if Result then
    exit;
  If aData.CPUID=-1 then
    Verbose(V_WARNING,'NO ID for CPU "'+aData.CPU+'" found.');
  If aData.OSID=-1 then
    Verbose(V_WARNING,'NO ID for OS "'+aData.OS+'" found.');
  If aData.VersionID=-1 then
    Verbose(V_WARNING,'NO ID for version "'+aData.Version+'" found.');
end;

procedure TDBDigestAnalyzer.Analyse(aConfig: TDigestConfig; aData : TTestRunData);

begin
  FDB.RelSrcDir:=aConfig.relsrcdir;
  FDB.TestSrcDir:=aConfig.testsrcdir;
  if (aData.longlogfile<>'') and FileExists(aData.longlogfile) then
    begin
    LongLogFile:=TStringList.Create;
    LongLogFile.LoadFromFile(aData.longlogfile);
    end;
  if not GetIDS(aConfig,aData) then
    exit;
  ProcessFile(aData.logfile,aData);
  UpdateTestRun(aData);
end;

function TDBDigestAnalyzer.GetIDs(const aConfig : TDigestConfig; var aData : TTestRunData): Boolean;


begin
  Result := False;
  aData.CPUID := FDB.GetCPUID(aData.CPU);
  aData.OSID := FDB.GetOSID(aData.OS);
  aData.VersionID := FDB.GetVersionID(aData.Version);
  if aData.Category='' then
    aData.Category:='Compiler/RTL';
  aData.CategoryID := FDB.GetCategoryID(aData.Category);
  aData.PlatformID := FDB.GetPlatformID(aData,True);
  If (Round(aData.Date)=0) then
    aData.Date:=Date;
  Result:=CheckIDS(aData);
  if not Result then
    Exit;
  aData.RunID:=FDB.GetRunID(aData);
  If (aData.RunID<>-1) then
    FDB.CleanTestRun(aData.RunID)
  else
    aData.RunID:=FDB.AddRun(aData);
  Result:=aData.RunID<>-1;
  if not Result then
    begin
    Verbose(V_Error,'Could not insert new testrun record!');
    exit;
    end;
end;

class procedure TDBDigestAnalyzer.ExtractTestFileName(var Line: string);

Var I : integer;

begin
  I:=Pos(' ',Line);
  If (I<>0) then
    Line:=Copy(Line,1,I-1);
end;

class function TDBDigestAnalyzer.AnalyseLine(var Line: string; var Status: TTestStatus): Boolean;

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

function TDBDigestAnalyzer.GetContentsFromLongLog(Line: String): String;

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

function TDBDigestAnalyzer.GetLog(Line, FN: String): String;

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

function TDBDigestAnalyzer.SaveTestResult(var aResult: TTestResultData): Boolean;

var
  lLast : TTestResultData;
  lNewID : Int64;

begin
  Result:=False;
  // Get last result for this test.
  lLast:=FDB.GetLastTestResult(aResult.TestID,aResult.PlatformID);
  if (aResult.Date<lLast.Date) then
    exit; // Do not save earlier results
  if not aResult.ResultDiffers(lLast) then
    exit; // do not save identical results
  // Need to save.
  lNewID:=FDB.AddTestResult(aResult);
  aResult.ID:=lNewId;
  // Save current in lastresult
  Result:=(LLast.ID<>lNewID);
  if Result then
    begin
    // When new, save previous.
    FDB.AddLastResult(aResult.TestID,aResult.PlatformID,lNewID);
    FDB.AddPreviousResult(aResult.TestID,aResult.PlatformID,LLast.ID);
    end;
end;

procedure TDBDigestAnalyzer.Processfile(const aFileName: String; var aData: TTestRunData);

var
  logfile : TStrings;
  fullline,line,prevLine : string;
  TS : TTestStatus;
  lPrev,lResult : TTestResultData;

begin
  lPrev:=Default(TTestResultData);
  lResult:=Default(TTestResultData);
  // init data common to the whole testrun
  lResult.RunID:=aData.RunID;
  lResult.PlatFormID:=aData.PlatFormID;
  lResult.Date:=aData.Date;
  lPrev.RunID:=aData.RunID;
  lPrev.PlatformID:=aData.PlatformID;
  lPrev.TestID:=-1; // Init no test
  lPrev.Date:=aData.Date;
  for TS in TTestStatus do
    aData.StatusCount[TS]:=0;
  PrevLine:='';
  logfile:=TStringList.Create;
  try
    LogFile.Capacity:=20000;
    LogFile.LoadFromFile(aFileName);
    For FullLine in LogFile do
      begin
        line:=fullline;
        TS:=stFailedToCompile;
        lResult.TestResult:=TS;
        If not AnalyseLine(line,TS) then
          begin
          Inc(UnknownLines);
          Verbose(V_Warning,'Unknown line: "'+line+'"');
          end
        else
          begin
          Verbose(V_NORMAL,'Analysing result for test '+Line);
          lResult.TestID:=FDB.RequireTestID(line);
          if lResult.TestID=-1 then
            begin
            Verbose(V_Warning,'No test ID: "'+line+'", skipping');
            Continue;
            end;
          If ExpectRun[TS] then
            begin
            // We expect a log line with log result, save
            Inc(aData.StatusCount[TS]);
            lPrev.TestResult:=TS;
            lPrev.TestID:=lResult.TestID;
            PrevLine:=line;
            end
          else
            begin
            // New test, insert previous result
            if (lPrev.TestID<>-1) and (lPrev.TestID<>lResult.TestID) then
              begin
              { This can only happen if a Successfully compiled message
                is not followed by any other line about the same test }
              SaveTestResult(lPrev);
              Verbose(V_Warning,'Orphaned test: "'+prevline+'"');
              end;
            // same test, so now we have run result
            lPrev.TestID:=-1;
            lResult.TestResult:=TS;
            If (lResult.TestID<>-1) then
              begin
              If Not (TestOK[TS] or TestSkipped[TS]) then
                begin
                  lResult.Log:=GetExecuteLog(Fullline,Line);
                  if pos(failed_to_compile,lResult.Log)=1 then
                    lResult.Log:=GetLog(Fullline,Line);
                end
              else
                lResult.Log:='';
              if SaveTestResult(lResult) then
                Inc(aData.StatusCount[TS]);
              end;
            end
          end
      end;
  finally
    Logfile.Free;
  end;
end;

procedure TDBDigestAnalyzer.UpdateTestRun(const aData : TTestRunData);

begin
  FDB.UpdateTestRun(aData);
end;


end.

