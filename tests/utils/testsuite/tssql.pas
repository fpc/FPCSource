unit tssql;

{$mode ObjFPC}
{$h+}

interface

uses
  Classes, SysUtils, sqldb, tsdb, tsconsts;


Type

  { TQueryData }

  TQueryData = Class(TObject)
    PlatFormID,
    RunID,
    CompareRunID,
    PreviousRunID,
    NextRunID,
    Previous2RunID,
    Next2RunID : Int64;
    TestFileID,
    CPUID,
    AllCategoryID,
    CategoryID,
    OSID : Integer;
    VersionID : Integer;
    TestFileName,
    VersionBranch,
    Cond,
    Submitter,
    Machine,
    config : String;
    Date : TDateTime;
    Debug,
    ListAll,
    NoSkipped,
    OnlyFailed : Boolean;
    RunSkipCount,
    RunFailedCount,
    RunCount : Integer;
    Action,
    Limit : Integer;
    TestLastDays : Integer;
    procedure InitFromVars(aSQL: TTestSQL; aVars: TStrings);
  end;

  { TDBInfo }

  TDBInfo = Class (TObject)
    AllCategoryID : Integer;
    AllCPUID : Integer;
    AllOSID : Integer;
    AllVersionID : Integer;
    Function IsAllCPU(aCPUID : Integer) : boolean;
    Function IsAllOS(aOSID : Integer) : boolean;
    Function IsAllVersion(aVersionID : Integer) : boolean;
  end;

  { TTestSuiteSQL }

  TTestSuiteSQL = class(TObject)
    FVars : TQueryData;
    FSQL : TTestSQL;
    FInfo : TDBInfo;
    constructor create(aVars : TQueryData; aSQL : TTestSQL; aDBInfo: TDBInfo);
    function GetTestResultsSQL : String;
    function GetTestResults : TSQLQuery;
    function GetRunOverviewSQL : String;
    function GetCompareRunSQL : String;
    function GetSimpleTestResultsSQL : String;
  private
    class function FieldIs(aField: String; aValue: String): String;
    class function PointerIs(aField: String; aValue: Int64; aSkipValue: Int64=-1): String;
  end;



implementation

{ TQueryData }

procedure TQueryData.InitFromVars(aSQL : TTestSQL; aVars: TStrings);

  function GetVar(aName: string): string;

  begin
    Result:=aVars.Values[aName];
  end;

  function Int64Var(const aVar : String; const aVar2 : String = '') : Int64;

  begin
    Result:=StrToInt64Def(GetVar(aVar),-1);
    if (Result=-1) and (aVar2<>'') then
      Result:=StrToInt64Def(GetVar(aVar2),-1);
  end;

  function IntVar(const aVar : String; const aVar2 : String = '') : Integer;

  begin
    Result:=StrToIntDef(GetVar(aVar),-1);
    if (Result=-1) and (aVar2<>'') then
      Result:=StrToIntDef(GetVar(aVar2),-1);
  end;

  function StrVar(const aVar : String; const aVar2 : String = '') : string;
  begin
    Result:=GetVar(aVar);
    if (Result='') and (aVar2<>'') then
      Result:=GetVar(aVar2);
  end;

  function BoolVar(const aVar : String; const aVar2 : String = '') : Boolean;
  var
    S : string;
  begin
    S:=GetVar(aVar);
    if (S='') and (aVar2<>'') then
      S:=GetVar(aVar2);
    Result:=(S='1');
  end;

Var
  S : String;

begin
  S:=StrVar('action','TESTACTION');
  Case S of
    'View_history' : Action:=faction_show_history;
    'Show/Compare' : Action:=faction_show_run_results;
    'Compare_to_previous':  Action:=faction_compare_with_previous;
    'Compare_to_next' : Action:=faction_compare_with_next;
    'Compare_right_to_previous' : Action:=faction_compare2_with_previous;
    'Compare_right_to_next' : Action:=faction_compare2_with_next;
    'Compare_both_to_previous' : Action:=faction_compare_both_with_previous;
    'Compare_both_to_next' : Action:=faction_compare_both_with_next;
  else
    Action:=StrToIntDef(S,0);
  end;
  Limit:=IntVar('limit','TESTLIMIT');
  if Limit=-1 then
    Limit:=50;
  if Limit > MaxLimit then
    Limit:=MaxLimit;
  Submitter:=StrVar('submitter','TESTSUBMITTER');
  Machine:=StrVar('machine','TESTMACHINE');
  RunID:=Int64Var('run1id','TESTRUN');
  TestLastDays:=IntVar('lastdays','TESTLASTDAYS');
  if TestLastDays=-1 then
    TestLastDays:=31;
  S:=StrVar('date','TESTDATE');
  if Length(S) > 0 then
    try
      Self.Date:=StrToDate(S);
    except
      Self.Date:=0;
    end;
  OnlyFailed:=BoolVar('failedonly','TESTFAILEDONLY');
  NoSkipped:=BoolVar('noskipped','TESTNOSKIPPED');
  CompareRunID:=Int64Var('run2id');
  PreviousRunID:=Int64Var('previousrunid');
  NextRunID:=Int64Var('nextrunid');
  Previous2RunID:=Int64Var('previous2runid');
  Next2RunID:=Int64Var('next2runid');
  TestFileID:=Int64Var('testfileid');
  TestFileName:=StrVar('testfilename');
  RunCount:=IntVar('PIETOTAL');
  RunSkipCount:=IntVar('PIESKIPPED');
  RunFailedCount:=IntVar('PIEFAILED');
  Debug:=BoolVar('DEBUGCGI');
  ListAll:=BoolVar('listall');
  Cond:=StrVar('cond','TESTCOND');
  Config:=StrVar('comment','TESTCOMMENT');
  if Config='' then
    Config:=StrVar('config','TESTCONFIG');

  // For Version,OS,CPU,Category: try integer, else try string and convert to integer.
  VersionID:=IntVar('version','TESTVERSION');
  if VersionID=-1 then
    VersionID:=aSQL.GetVersionID(StrVar('version','TESTVERSION'));
  OSID:=IntVar('os','TESTOS');
  if OSID=-1 then
    OSID:=aSQL.GetOSID(StrVar('os','TESTOS'));
  CPUID:=IntVar('cpu','TESTCPU');
  if CPUID=-1 then
    CPUID:=aSQL.GetCPUID(StrVar('cpu','TESTCPU'));
  CategoryID:=IntVar('category','TESTCATEGORY');
  if CategoryID=-1 then
    CategoryID:=aSQL.GetCategoryID(StrVar('category','TESTCATEGORY'));
  if (TestFileID=-1) and (TestFileName<>'') then
    TestFileID:=aSQL.GetTestID(TestFileName);
  if (TestFileID<>-1) then
    TestFileName:=aSQL.GetTestFileName(TestFileID);
end;

{ TDBInfo }

function TDBInfo.IsAllCPU(aCPUID: Integer): boolean;
begin
  Result:=(aCPUID=-1) or (aCPUID=AllCPUID);
end;

function TDBInfo.IsAllOS(aOSID: Integer): boolean;
begin
  Result:=(aOSID=-1) or (aOSID=AllOSID);
end;

function TDBInfo.IsAllVersion(aVersionID: Integer): boolean;
begin
  Result:=(aVersionID=-1) or (aVersionID=AllVersionID);
end;

{ TTestSuiteSQL }

class function TTestSuiteSQL.PointerIs(aField: String; aValue: Int64; aSkipValue: Int64): String;

begin
  Result:='';
  if (aValue<0) or (aValue=aSkipValue) then
    exit;
  Result:=Format(' AND (%s=%d)',[aField,aValue]);
end;

class function TTestSuiteSQL.FieldIs(aField: String; aValue: String): String;
begin
  Result:='';
  if aValue='' then exit;
  Result:=Format('AND (%s = ''%s'')',[aField,TTestSQL.EscapeSQL(aValue)]);
end;

constructor TTestSuiteSQL.create(aVars: TQueryData; aSQL: TTestSQL; aDBInfo : TDBInfo);
begin
  FVars:=aVars;
  FSQL:=aSQL;
  FInfo:=aDBInfo;
end;

function TTestSuiteSQL.GetTestResultsSQL: String;

var
  S,SS : String;

begin
  SS:='SELECT TR_ID,TR_TESTRUN_FK AS Run, TR_TEST_FK, TR_OK AS OK'
    +', TR_SKIP As Skip,TR_RESULT  As Result'
  //S:='SELECT * '
    +',TC_NAME AS CPU, TV_VERSION AS Version, TO_NAME AS OS'
    +',TU_ID,TU_DATE AS Date,TU_SUBMITTER  AS Submitter'
    +',(TU_FAILEDTOCOMPILE + TU_FAILEDTOFAIL + TU_FAILEDTORUN) AS Fails'
    +',TP_MACHINE AS Machine,TP_CONFIG AS config'
    +',TU_COMPILERDATE As CompDate'
    +',TU_TESTSREVISION AS Tests_rev'
    +',TU_RTLREVISION AS RTL_rev'
    +',TU_COMPILERREVISION AS Compiler_rev'
    +',TU_PACKAGESREVISION AS Packages_rev'
    +',TO_ID,TC_ID,TV_ID'
    +' FROM TESTRUN '
    +' Inner join TESTPLATFORM ON (TU_PLATFORM_FK=TP_ID) '
    +' LEFT JOIN TESTRESULTS ON  (TR_TESTRUN_FK=TU_ID)'
    +' LEFT JOIN TESTOS ON  (TP_OS_FK=TO_ID)'
    +' LEFT JOIN TESTCPU ON  (TP_CPU_FK=TC_ID)'
    +' LEFT JOIN TESTVERSION ON  (TP_VERSION_FK=TV_ID)';
  S:='';
  S:=S+PointerIS('TR_TEST_FK',FVars.TestFileID);
  S:=S+PointerIs('TR_TESTRUN_FK',FVars.RunID);
  If FVars.OnlyFailed then
    S:=S+' AND (NOT TR_OK)';
  If FVars.NoSkipped then
    S:=S+' AND (NOT TR_SKIP)';
  If FVars.Cond<>'' then
    S:=S+' AND ('+FVars.Cond+')';
  S:=S+PointerIs('TP_CPU_FK',FVars.CPUID, FInfo.AllCPUID);
  S:=S+PointerIs('TP_VERSION_FK',FVars.VERSIONID,FInfo.AllVersionID);
  S:=S+PointerIs('TP_OS_FK',FVars.OSID,FInfo.AllOSID);
  S:=S+FieldIs('TP_MACHINE',FVars.Machine);
  S:=S+FieldIs('TP_CONFIG',FVars.Config);
  S:=S+FieldIs('TU_SUBMITTER',FVars.Submitter);
  if FVars.DATE<>0 then
    S:=S+Format(' AND (TU_DATE >= ''%s'')',[FormatDateTime('YYYY-MM-DD',FVars.Date)]);

  if S <> '' then
  begin
    Delete(S, 1, 4);
    S:=SS + ' WHERE '+ S;
  end
  else
    S:=SS;

  S:=S+' ORDER BY TU_ID DESC';
  if FVars.DATE=0 then
    S:=S+' LIMIT '+IntToStr(FVars.Limit)
  else
    S:=S+' LIMIT '+IntToStr(MaxLimit);
  Result:=S;
end;

function TTestSuiteSQL.GetTestResults: TSQLQuery;
begin
  Result:=FSQL.CreateQuery(GetTestResultsSQL);
end;

function TTestSuiteSQL.GetRunOverviewSQL: String;

Const
  SOverview = 'SELECT TU_ID as ID,TU_DATE as Date,TC_NAME as CPU,TO_NAME as OS,'+
               'TV_VERSION as Version, '+
               '(select count(*) from testresults where (TR_TESTRUN_FK=TU_ID)) as Count,'+
               'TU_COMPILERREVISION as CompRev,'+
               'TU_RTLREVISION as RTLRev,'+
               'TU_PACKAGESREVISION as PackRev,'+
               'TU_TESTSREVISION as TestsRev,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN) AS OK,'+
               '(TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Failed,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN+'+
                'TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Total,'+
               'TU_SUBMITTER as Submitter, TP_MACHINE as Machine, TP_CONFIG as Comment %s '+
              'FROM '+
               ' TESTRUN '+
               ' left join TESTPLATFORM on (TP_ID=TU_PLATFORM_FK) '+
               ' left join TESTCPU on (TC_ID=TP_CPU_FK) '+
               ' left join TESTOS on (TO_ID=TP_OS_FK) '+
               ' left join TESTVERSION on (TV_ID=TP_VERSION_FK) '+
               ' left join TESTCATEGORY on (TA_ID=TP_CATEGORY_FK) '+
              '%s'+
              'ORDER BY TU_ID DESC LIMIT %d';

Var
  SC,S : String;

begin
   S:='';
   S:=S+PointerIs('TP_CPU_FK',FVars.CPUID,FInfo.AllCPUID);
   S:=S+PointerIs('TP_CATEGORY_FK',FVars.CategoryID,FInfo.AllCategoryID);
   S:=S+PointerIs('TP_VERSION_FK',FVars.VersionID);
   S:=S+PointerIs('TP_OS_FK',FVars.OSID,FInfo.ALLOSID);
   If (Round(FVars.Date)<>0) then
     S:=S+Format(' AND (TU_DATE=''%s'')',[FormatDateTime('YYYY-MM-DD',FVars.Date)]);
   S:=S+FieldIs('TU_SUBMITTER',FVars.Submitter);
   S:=S+FieldIs('TP_MACHINE',FVars.Machine);
   S:=S+FieldIs('TP_CONFIG',FVars.Config);
   If FVars.Cond<>'' then
     S:=S+' AND ('+FVars.Cond+')';
   If (FSQL.GetCategoryName(FVars.CategoryID)<>'DB') then
     SC:=', CONCAT(TU_COMPILERREVISION,''/'',TU_RTLREVISION,''/'', '+
          'TU_PACKAGESREVISION,''/'',TU_TESTSREVISION) as rev'
   else
     SC:='';
   If (FVars.CategoryID=-1) or (FSQL.GetCategoryName(FVars.CategoryID)='All') then
     SC:=SC+', TA_NAME as Cat';


  if S <> '' then
  begin
    Delete(S, 1, 4);
    S:='WHERE '+ S + ' ';
  end;
  Result:=Format(SOverview,[SC,S,FVars.Limit]);
end;

function TTestSuiteSQL.GetCompareRunSQL: String;
var
  S,QRy : String;
begin
  If FVars.NoSkipped then
    begin
    Qry:='(((tr1.TR_SKIP) and (not tr2.TR_OK) and (not tr2.TR_SKIP)) or '
       +'((not tr1.TR_OK) and (not tr1.TR_SKIP) and (tr2.TR_SKIP)) or '
       +'((not tr1.TR_SKIP) and (not tr2.TR_SKIP))) and ';
    end
  else
    Qry:='';
  S:=Format(
     'with tr1 as (SELECT * FROM TESTRESULTS WHERE TR_TESTRUN_FK=%d), '+
     '     tr2 as (SELECT * FROM TESTRESULTS WHERE TR_TESTRUN_FK=%d) '+
     ' SELECT T_ID as id,T_NAME as Filename,tr1.TR_SKIP as Run1_Skipped,'
     +'tr2.TR_SKIP as Run2_Skipped,tr1.TR_OK as Run1_OK,'
     +'tr2.TR_OK as Run2_OK, tr1.TR_Result as Run1_Result,'
     +'tr2.TR_RESULT as Run2_Result '
     +'FROM TESTS, tr2 LEFT JOIN tr1 USING (TR_TEST_FK) '
     +'WHERE ((tr1.TR_SKIP IS NULL) or (tr2.TR_SKIP IS NULL) or '
     +' (%s (tr1.TR_Result<>tr2.TR_Result)))'
     +'and (T_ID=tr2.TR_TEST_FK)',[FVars.RunID,FVars.CompareRunID,Qry]);
  Result:=S;
end;

function TTestSuiteSQL.GetSimpleTestResultsSQL: String;

var
  S : String;

begin
  S:=Format('SELECT TR_ID, TR_TESTRUN_FK AS RUN, TR_TEST_FK, TR_OK, TR_SKIP, TR_RESULT '
            +' FROM TESTRESULTS '
            +' WHERE  (TR_TEST_FK=%d)',[FVars.TestFileID]);
  If FVars.OnlyFailed then
    S:=S+' AND (TR_OK=''f'')';
  if (FVars.comparerunid<>-1) then
     S:=S+Format(' AND ((TR_TESTRUN_FK=%d) OR (TR_TESTRUN_FK=%d))',[FVars.runid,FVars.comparerunid])
  else if (FVars.runid<>-1) then
     S:=S+Format(' AND (TR_TESTRUN_FK=%d)',[FVars.runid])
  else
     S:=S+' ORDER BY TR_TESTRUN_FK DESC LIMIT '+IntToStr(FVars.Limit);
  Result:=S;
end;

end.

