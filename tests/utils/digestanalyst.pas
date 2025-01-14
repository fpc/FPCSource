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
    function CheckIDs(var aData: TTestRunData): Boolean;
    function GetExecuteLog(Line, FN: String): String;
    function GetIDs(const aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    procedure Processfile(const aFileName: String; var aData: TTestRunData);
    function SaveTestResult(aResult: TTestResultData): Boolean;
    procedure UpdateTestRun(const aData: TTestRunData);
    function GetContentsFromLongLog(Line: String): String;
    function GetLog(Line, FN: String): String;
  public
    constructor Create(aDB : TTestSQL);
    class function AnalyseLine(var Line: string; var Status: TTestStatus): Boolean;
    class procedure ExtractTestFileName(var Line: string);
    procedure Analyse(aConfig : TDigestConfig; aData : TTestRunData);
  end;


implementation

constructor TDBDigestAnalyzer.Create(aDB: TTestSQL);
begin
  FDB:=aDB;
end;

function TDBDigestAnalyzer.CheckIDs(var aData : TTestRunData): Boolean;

begin
  If aData.CategoryID=-1 then
    aData.CategoryID:=1;
  Result:=(aData.CPUID<>-1) and (aData.OSID<>-1) and (aData.VersionID<>-1);
  if Result then
    exit;
  If aData.CPUID=-1 then
    Verbose(V_Error,'NO ID for CPU "'+aData.CPU+'" found.');
  If aData.OSID=-1 then
    Verbose(V_Error,'NO ID for OS "'+aData.OS+'" found.');
  If aData.VersionID=-1 then
    Verbose(V_Error,'NO ID for version "'+aData.Version+'" found.');
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
    begin
    aData.RunID:=FDB.AddRun(aData);
    Result:=aData.RunID<>-1;
    if not Result then
      begin
      Verbose(V_Error,'Could not insert new testrun record!');
      exit;
      end;
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

function TDBDigestAnalyzer.SaveTestResult(aResult : TTestResultData) : Boolean;

var
  lLast : TTestResultData;
  lNewID : Int64;

begin
  lLast:=FDB.GetLastTestResult(aResult.TestID,aResult.PlatformID);
  if aResult.Differs(lLast) then
    begin
    // Need to save
    lNewID:=FDB.AddTestResult(aResult)
    end
  else
    // Update status, testrun & log
    FDB.UpdateTestResult(aResult);
end;

procedure TDBDigestAnalyzer.Processfile(const aFileName: String; var aData: TTestRunData);

var
  logfile : TStrings;
  fullline,line,prevLine : string;
  TS : TTestStatus;
  Testlog : string;
  count_test : boolean;
  lPrev,lResult : TTestResultData;

begin
  lPrev:=Default(TTestResultData);
  // init data common to the whole testrun
  lResult.RunID:=aData.RunID;
  lResult.PlatFormID:=aData.PlatFormID;
  lPrev.RunID:=aData.RunID;
  lPrev.PlatformID:=aData.PlatformID;
  lPrev.TestID:=-1; // Init no test
  PrevLine:='';
  logfile:=TStringList.Create;
  try
    LogFile.Capacity:=20000;
    LogFile.LoadFromFile(aFileName);
    For FullLine in LogFile do
      begin
        lResult:=Default(TTestResultData);
        line:=fullline;
        lResult.TestResult:=stFailedToCompile;
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
              SaveTestResult(lResult);
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

