unit tctestsql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tsutils, tsdb, tstypes, sqldb, pqconnection;


type
  { TTestSQLCase }

  { TTestBaseSQLCase }

  TTestBaseSQLCase = class(TTestCase)
  Protected
    function CreateResultData(out aData: TTestRunData; out aResult: TTestResultData; DateOffset: Integer = 0): Int64;
    function PreparePlatform(var aData: TTestRunData): Integer;
    procedure CreateSource(const aFileName : String);
    procedure DeleteSource(const aFileName: String);
    procedure AssertTestRunData(aQry: TSQLQuery; aData: TTestRunData);
    function GetSQL: TTestSQL; virtual; abstract;
    property SQL : TTestSQL Read GetSQL;
  end;

  TTestSQLCase = class(TTestBaseSQLCase)
  const
    SQLTestResultFilter = '(TR_ID=%d) and (TR_TESTRUN_FK=%d) and (TR_TEST_FK=%d) and (TR_OK=''%s'') and (TR_SKIP=''%s'') and (TR_RESULT=%d) and (TR_LOG=''%s'')';
  private
  protected
    function GetSQL: TTestSQL; override;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestAddCPU;
    procedure TestAddOS;
    procedure TestAddVersion;
    procedure TestAddCategory;
    procedure TestAddTest;
    Procedure TestUpdateTest;
    procedure TestAddPlatform;
    Procedure TestAddRun;
    procedure TestUpdateRun;
    Procedure TestAddTestResult;
    Procedure TestAddTestResultTwice;
    Procedure TestUpdateTestResult;
    Procedure TestAddLastResult;
    Procedure TestAddLastResultTwice;
    Procedure TestGetLastTestResult;
    Procedure TestAddPreviousResult;
    Procedure TestAddPreviousResultTwice;
    procedure TestGetCPUID;
    procedure TestGetOSID;
    procedure TestGetCategoryID;
    procedure TestGetVersionID;
    procedure TestGetTestID;
    procedure TestGetRunID;
    procedure TestHistoryNoHistory;
    procedure TestHistoryWithHistory;
    Procedure TestGetPreviousTestRun;
    Procedure TestGetNextTestRun;
    procedure TestAddCheckAllRtl;
  end;



implementation

uses tsstring, tcsetup;

{ TTestBaseSQLCase }


procedure TTestBaseSQLCase.DeleteSource(const aFileName: String);
begin
  if FileExists(aFilename+'.pp') then
    if not DeleteFile(aFilename+'.pp') then
      Fail('Failed to delete '+aFileName+'.pp');
end;

procedure TTestBaseSQLCase.CreateSource(const aFileName: String);
var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
    Src.Add('program '+aFileName+';');
    Src.Add('begin');
    Src.Add('end.');
    Src.SaveToFile(afileName+'.pp');
  finally
    Src.Free;
  end;
end;

function TTestBaseSQLCase.PreparePlatform(var aData : TTestRunData) : Integer;

begin
  aData.CategoryID:=SQL.GetCategoryID('x');
  if aData.CategoryID=-1 then
    aData.CategoryID:=SQL.AddCategory('x');

  aData.OSID:=SQL.GetOSID('y');
  if aData.OSID=-1 then
    aData.OSID:=SQL.AddOS('y');

  aData.CPUID:=SQL.GetCPUID('z');
  if aData.CPUID=-1 then
    aData.CPUID:=SQL.AddCPU('z');

  aData.VersionID:=SQL.GetVersionID('w');
  if aData.VersionID=-1 then
    aData.VersionID:=SQL.AddVersion('w',Date);

  aData.config:='v';
  aData.machine:='w';
  Result:=SQL.GetPlatformID(aData,False);

  if Result=-1 then
    Result:=SQL.AddPlatform(aData);
end;

procedure TTestBaseSQLCase.AssertTestRunData(aQry : TSQLQuery; aData : TTestRunData);

var
  St : TTestStatus;

begin
  With aQry,aData do
    begin
    AssertEquals('Date',DATE,FieldByName('TU_DATE').AsDateTime);
    AssertEquals('Platform',PlatformID,FieldByName('TU_PLATFORM_FK').AsInteger);
    AssertEquals('Submitter',Submitter,FieldByName('TU_SUBMITTER').AsString);
    For St in TValidTestStatus do
      AssertEquals(StatusText[St],StatusCount[st],FieldByName(SQLField[ST]).AsInteger);
    AssertEquals('CompilerDate',CompilerDate,FieldByName('TU_COMPILERDATE').AsString);
    AssertEquals('CompilerFullVersion',CompilerFullVersion,FieldByName('TU_COMPILERFULLVERSION').AsString);
    AssertEquals('CompilerRevision',CompilerRevision,FieldByName('TU_COMPILERREVISION').AsString);
    AssertEquals('TestsRevision',TestsRevision,FieldByName('TU_TESTSREVISION').AsString);
    AssertEquals('RTLRevision',RTLRevision,FieldByName('TU_RTLREVISION').AsString);
    AssertEquals('PackagesRevision',PackagesRevision,FieldByName('TU_PACKAGESREVISION').AsString);
    end;
end;

function TTestBaseSQLCase.CreateResultData(out aData: TTestRunData; out aResult: TTestResultData; DateOffset: Integer): Int64;

begin
  aData:=Default(TTestRunData);
  aData.PlatformID:=PreparePlatform(aData);
  aData.Date:=Date-DateOffset;
  aData.RunID:=SQL.AddRun(aData);
  aResult:=Default(TTestResultData);
  aResult.RunID:=aData.RunID;
  aResult.PlatformID:=aData.PlatformID;
  aResult.Date:=Date-DateOffset;
  CreateSource('x');
  if SQL.GetTestID('x.pp')=-1 then
    aResult.TestID:=SQL.AddTest('x.pp',False)
  else
    aResult.TestID:=SQL.GetTestID('x.pp');
  aResult.TestResult:=stSuccessCompilationFailed;
  aResult.Log:='xyz';
  With aData do
    begin
    Result:=SQL.AddTestResult(aResult);
    aResult.ID:=Result;
    end;
end;


{ TTestSQLCase }

procedure TTestSQLCase.TestHookUp;
begin
  AssertEquals('Empty testos',0,TDBHelper.CountRecords('TESTOS'));
  AssertEquals('Empty TESTCPU',0,TDBHelper.CountRecords('TESTCPU'));
  AssertEquals('Empty TESTCATEGORY',0,TDBHelper.CountRecords('TESTCATEGORY'));
  AssertEquals('Empty TESTVERSION',0,TDBHelper.CountRecords('TESTVERSION'));
  AssertEquals('Empty TESTPLATFORM',0,TDBHelper.CountRecords('TESTPLATFORM'));
  AssertEquals('Empty TESTRUN',0,TDBHelper.CountRecords('TESTRUN'));
  AssertEquals('Empty TESTS',0,TDBHelper.CountRecords('TESTS'));
  AssertEquals('Empty TESTRESULTS',0,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('Empty TESTLASTRESULTS',0,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('Empty TESTPREVIOUSRESULTS',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
end;

procedure TTestSQLCase.TestAddCPU;
var
  lID : Int64;
begin
  lID:=SQL.AddCPU('x');
  AssertEquals('exists',1,TDBHelper.CountRecords('TESTCPU',Format('(TC_ID=%d) and (tc_name=''x'')',[lID])));
end;

procedure TTestSQLCase.TestAddOS;
var
  lID : Int64;
begin
  lID:=SQL.AddOS('x');
  AssertEquals('exists',1,TDBHelper.CountRecords('TESTOS',Format('(TO_ID=%d) and (to_name=''x'')',[lID])));
end;

procedure TTestSQLCase.TestAddVersion;
var
  lID : Int64;
begin
  lID:=SQL.AddVersion('x',date);
  AssertEquals('exists',1,TDBHelper.CountRecords('TESTVERSION',Format('(Tv_ID=%d) and (tv_version=''x'')',[lID])));
end;

procedure TTestSQLCase.TestAddCategory;
var
  lID : Int64;
begin
  lID:=SQL.AddCategory('x');
  AssertEquals('exists',1,TDBHelper.CountRecords('TESTCATEGORY',Format('(TA_ID=%d) and (ta_name=''x'')',[lID])));
end;

procedure TTestSQLCase.TestAddTest;
var
  lID : Integer;
begin
  CreateSource('x');
  lID:=SQL.AddTest('x.pp',False);
  AssertEquals('exists',1,TDBHelper.CountRecords('TESTS',Format('(T_ID=%d) and (t_name=''x.pp'')',[lID])));
end;

procedure TTestSQLCase.TestUpdateTest;
var
  lInfo : TTestInfo;
  lID : Integer;
  lFilter : string;
begin
  lID:=SQL.AddTest('x.pp',False);
  lInfo:=Default(TTestInfo);
  With lInfo do
    begin
    Name:='name';  // Will not be changed
    CPU:='cpu';
    OS:='os';
    Version:='version';
    AddDate:=Date-1; // Will not be changed
    Graph:=True;
    Interactive:=True;
    Result:=123;
    Fail:=True;
    ReCompile:=True;
    NoRun:=True;
    NeedLibrary:=True;
    KnownRunError:=456;
    Known:=True;
    Note:='note';
    Description:='description';
    Source:='source';
    Opts:='opts';
    DelOptions:='deloptions';
    SkipCPU:='skipcpu';
    SkipEmu:='skipemu';
    NeedTarget:='needtarget';
    SkipTarget:='skiptarget';
    MaxVersion:='maxversion';
    KnownRunNote:='knownrunnote';
    KnownCompileNote:='knowncompilenote';
    RecompileOpt:='recompileopt';
    KnownCompileError:=789;
    NeededAfter:=True;
    IsKnownRunError:=True;
    Timeout:=543;
    Category:='category';
    Files:='files';
    ConfigFileSrc:='configfilesrc';
    ConfigFileDst:='configfiledst';
    WpoParas:='wpoparas';
    WpoPasses:=321;
    DelFiles:='delfiles';
    ExpectMsgs:=[1,2,3];
    end;
  SQL.UpdateTest(lID,lInfo,'xyz');
  // Construct filter with the values we expect.
  lFilter := Format('(T_ID = %d) AND ',[lID])
  + '(T_Name = ''x.pp'') AND '
  + '(T_CPU = ''cpu'') AND '
  + '(T_OS = ''os'') AND '
  + '(T_Version = ''version'') AND '
  + '(T_AddDate = '''+TTestSQL.SQLDate(Date)+''') AND '
  + '(T_Graph = ''t'') AND '
  + '(T_Interactive = ''t'') AND '
  + '(T_Result = 123) AND '
  + '(T_Fail = ''t'') AND '
  + '(T_ReCompile = ''t'') AND '
  + '(T_NoRun = ''t'') AND '
  + '(T_NeedLibrary = ''t'') AND '
  + '(T_KnownRunError = 456) AND '
  + '(T_Known = ''t'') AND '
  + '(T_Note = ''note'') AND '
  + '(T_Description = ''description'') AND '
  + '(T_Source = ''source'') AND '
  + '(T_Opts = ''opts'') AND '
  + '(T_DELOPTS =''deloptions'') AND '
  + '(T_SKIPCPU = ''skipcpu'') AND '
  + '(T_NEEDTARGET = ''needtarget'') AND '
  + '(T_MAXVERSION = ''maxversion'') AND '
  + '(T_KNOWNRUNNOTE = ''knownrunnote'') AND '
  + '(T_KNOWNCOMPILENOTE = ''knowncompilenote'') AND '
  + '(T_RECOMPILEOPT = ''recompileopt'') AND '
  + '(T_KNOWNCOMPILEERROR = 789) AND '
  + '(T_NEEDEDAFTER = ''t'') AND '
  + '(T_ISKNOWNRUNERROR = ''t'') AND '
  + '(T_Timeout = 543) AND '
  + '(T_CATEGORY = ''category'') AND '
  + '(T_FILES = ''files'') AND '
  + '(T_CONFIGFILESRC = ''configfilesrc'') AND '
  + '(T_CONFIGFILEDST = ''configfiledst'') AND '
  + '(T_WPOPARAS = ''wpoparas'') AND '
  + '(T_WPOPASSES = 321) AND '
  + '(T_DELFILES = ''delfiles'') AND '
  + '(T_EXPECTMSGS = ''1,2,3'')' ;
  // We should have 1 record with this filter
  AssertEquals('Updated',1,TDBHelper.CountRecords('TESTS',lFilter));
end;


procedure TTestSQLCase.TestAddPlatform;

const
  SQLFilter = '(TP_ID=%d) and (TP_OS_FK=%d) and (TP_CPU_FK=%d) '+
              'and (TP_VERSION_FK=%d) and (TP_CONFIG=''%s'') and (TP_MACHINE=''%s'')';
var
  lData : TTestRunData;
  lID : integer;
  Flt : String;
begin
  lData:=Default(TTestRunData);
  lID:=PreparePlatform(lData);
  With lData do
    flt:=Format(SQLFilter,[lID,OSID,CPUID,VersionID,Config,Machine]);
  AssertEquals('Platform',1,TDBHelper.CountRecords('TESTPLATFORM',Flt));
end;


procedure TTestSQLCase.TestAddRun;
var
  lData : TTestRunData;
  lID : Int64;
  Qry : TSQLQuery;

begin
  lData:=Default(TTestRunData);
  lData.PlatformID:=PreparePlatform(lData);
  With lData do
    begin
    machine:='a';
    submitter:='b';
    description:='c';
    Date:=Sysutils.Date;
    CompilerDate:='ymd';
    CompilerFullVersion:='1.2';
    CompilerRevision:='1.3';
    TestsRevision:='1.4';
    RTLRevision:='1.5';
    PackagesRevision:='1.6';
    end;
  lID:=SQL.AddRun(lData);
  Qry:=TDBHelper.CreateQuery(Format('Select * from testrun where (tu_id=%d)',[lID]));
  try
    Qry.Open;
    AssertFalse('Have data',Qry.IsEmpty);
    AssertTestRunData(Qry,lData);
  finally
    Qry.Free;
  end;
end;


procedure TTestSQLCase.TestAddTestResult;

var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID : Int64;
  flt : String;
  OK,Skip : Boolean;

begin
  lID:=CreateResultData(lData,lResult);
  OK:=TestOK[lResult.TestResult];
  Skip:=TestSkipped[lResult.TestResult];
  With lResult do
    flt:=Format(SQLTestResultFilter,[lID,RunID,TestID,Bools[OK],Bools[Skip],Ord(TestResult),Log]);
  AssertEquals('Platform',1,TDBHelper.CountRecords('TESTRESULTS',Flt));
end;

procedure TTestSQLCase.TestAddTestResultTwice;
var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID,lID2 : Int64;
  flt : String;
  OK,Skip : Boolean;

begin
  CreateResultData(lData,lResult);
  OK:=TestOK[lResult.TestResult];
  Skip:=TestSkipped[lResult.TestResult];
  lID:=SQL.AddTestResult(lResult);
  // Change result
  lResult.TestResult:=stFailedToCompile;
  lResult.Log:='xyza';
  OK:=TestOK[lResult.TestResult];
  Skip:=TestSkipped[lResult.TestResult];
  // Insert again...
  lID2:=SQL.AddTestResult(lResult);
  AssertEquals('Same ID',lID,lID2);
  flt:=Format(SQLTestResultFilter,[lID,lResult.RunID,lResult.TestID,Bools[OK],Bools[Skip],Ord(lResult.TestResult),lResult.Log]);
  AssertEquals('Result',1,TDBHelper.CountRecords('TESTRESULTS',Flt));
end;

procedure TTestSQLCase.TestUpdateTestResult;
var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID2,lID : Int64;
  flt : String;
  OK,Skip : Boolean;

begin
  lID:=CreateResultData(lData,lResult);
  // Change result
  lResult.ID:=lID;
  lResult.TestResult:=stFailedToCompile;
  lResult.Log:='xyza';
  OK:=TestOK[lResult.TestResult];
  Skip:=TestSkipped[lResult.TestResult];
  // Update
  lID2:=SQL.UpdateTestResult(lResult);
  AssertEquals('Same ID',lID,lID2);
  flt:=Format(SQLTestResultFilter,[lID,lResult.RunID,lResult.TestID,Bools[OK],Bools[Skip],Ord(lResult.TestResult),lResult.Log]);
  AssertEquals('Result',1,TDBHelper.CountRecords('TESTRESULTS',Flt));
end;

procedure TTestSQLCase.TestAddLastResult;

var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID : Int64;
  flt : String;

begin
  lID:=CreateResultData(lData,lResult);
  AssertTrue('Add',SQL.AddLastResult(lResult.TestID,lData.PlatformID,lID));
  flt:=Format('(TL_TEST_FK=%d) and (TL_PLATFORM_FK=%d) and (TL_TESTRESULTS_FK=%d)',[lResult.TestID,lData.PlatformID,lID]);
  AssertEquals('Result',1,TDBHelper.CountRecords('TESTLASTRESULTS',Flt));
end;

procedure TTestSQLCase.TestAddLastResultTwice;
var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID,lID2 : Integer;
  flt : string;
begin
  lID:=CreateResultData(lData,lResult,1);
  AssertTrue('Add',SQL.AddLastResult(lResult.TestID,lData.PlatformID,lID));
  lID2:=CreateResultData(lData,lResult,0);
  AssertTrue('Add',SQL.AddLastResult(lResult.TestID,lData.PlatformID,lID2));
  flt:=Format('(TL_TEST_FK=%d) and (TL_PLATFORM_FK=%d) and (TL_TESTRESULTS_FK=%d)',[lResult.TestID,lData.PlatformID,lID2]);
  AssertEquals('Result',1,TDBHelper.CountRecords('TESTLASTRESULTS',Flt));
end;

procedure TTestSQLCase.TestGetLastTestResult;
var
  lData : TTestRunData;
  lResult2,lResult : TTestResultData;
  lID : Integer;
begin
  lID:=CreateResultData(lData,lResult,1);
  AssertTrue('Add',SQL.AddLastResult(lResult.TestID,lData.PlatformID,lID));
  lResult2:=SQL.GetLastTestResult(lResult.TestID,lData.PlatformID);
  AssertEquals('ID',lID,lResult2.ID);
  AssertEquals('Run',lResult.RunID,lResult2.RunID);
  AssertTrue('Status',lResult.TestResult=lResult2.TestResult);
  AssertEquals('Log',lResult.Log,lResult2.Log);
  AssertEquals('Date',Date-1,lResult2.Date);
end;

procedure TTestSQLCase.TestAddPreviousResult;
var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID : Int64;
  flt : String;

begin
  lID:=CreateResultData(lData,lResult);
  AssertTrue('Add',SQL.AddPreviousResult(lResult.TestID,lData.PlatformID,lID));
  flt:=Format('(TPR_TEST_FK=%d) and (TPR_PLATFORM_FK=%d) and (TPR_TESTRESULTS_FK=%d)',[lResult.TestID,lData.PlatformID,lID]);
  AssertEquals('Result',1,TDBHelper.CountRecords('TESTPREVIOUSRESULTS',Flt));
end;

procedure TTestSQLCase.TestAddPreviousResultTwice;
var
  lData : TTestRunData;
  lResult : TTestResultData;
  lID,lID2 : Integer;
  flt : string;
begin
  lID:=CreateResultData(lData,lResult,1);
  AssertTrue('Add',SQL.AddPreviousResult(lResult.TestID,lData.PlatformID,lID));
  lID2:=CreateResultData(lData,lResult,0);
  AssertTrue('Add',SQL.AddPreviousResult(lResult.TestID,lData.PlatformID,lID2));
  flt:=Format('(TPR_TEST_FK=%d) and (TPR_PLATFORM_FK=%d) and (TPR_TESTRESULTS_FK=%d)',[lResult.TestID,lData.PlatformID,lID2]);
  AssertEquals('Result',1,TDBHelper.CountRecords('TESTPREVIOUSRESULTS',Flt));
end;

procedure TTestSQLCase.TestUpdateRun;

var
  lData : TTestRunData;
  St : TTestStatus;
  Qry : TSQLQuery;

begin
  lData:=Default(TTestRunData);
  lData.PlatformID:=PreparePlatform(lData);
  lData.RunID:=SQL.AddRun(lData);
  for St in TValidTestStatus do
    lData.StatusCount[st]:=(Ord(st)+1)*100;
  AssertTrue('Update',SQL.UpdateTestRun(lData));
  Qry:=TDBHelper.CreateQuery(Format('Select * from testrun where (tu_id=%d)',[lData.RunID]));
  try
    Qry.Open;
    AssertFalse('Have data',Qry.IsEmpty);
    AssertTestRunData(Qry,lData);
  finally
    Qry.Free;
  end;
end;

procedure TTestSQLCase.TestGetCPUID;
begin
  TDBHelper.ExecSQL('INSERT INTO TESTCPU VALUES (1,''x'')');
  TDBHelper.ExecSQL('INSERT INTO TESTCPU VALUES (2,''y'')');
  AssertEquals('Count',2,TDBHelper.CountRecords('TESTCPU'));
  AssertEquals('Get x',1,SQL.GetCPUID('x'));
  AssertEquals('Get y',2,SQL.GetCPUID('y'));
  AssertEquals('Get z',-1,SQL.GetCPUID('z'));
end;

procedure TTestSQLCase.TestGetOSID;

begin
  TDBHelper.ExecSQL('INSERT INTO TESTOS VALUES (1,''x'')');
  TDBHelper.ExecSQL('INSERT INTO TESTOS VALUES (2,''y'')');
  AssertEquals('Count',2,TDBHelper.CountRecords('TESTOS'));
  AssertEquals('Get x',1,SQL.GetOSID('x'));
  AssertEquals('Get y',2,SQL.GetOSID('y'));
  AssertEquals('Get z',-1,SQL.GetOSID('z'));
end;

procedure TTestSQLCase.TestGetCategoryID;
begin
  TDBHelper.ExecSQL('INSERT INTO TESTCategory VALUES (1,''x'')');
  TDBHelper.ExecSQL('INSERT INTO TESTCategory VALUES (2,''y'')');
  AssertEquals('Count',2,TDBHelper.CountRecords('TESTCategory'));
  AssertEquals('Get x',1,SQL.GetCategoryID('x'));
  AssertEquals('Get y',2,SQL.GetCategoryID('y'));
  AssertEquals('Get z',-1,SQL.GetCategoryID('z'));
end;

procedure TTestSQLCase.TestGetVersionID;
begin
  TDBHelper.ExecSQL('INSERT INTO TESTVERSION (TV_ID,TV_VERSION) VALUES (1,''x'')');
  TDBHelper.ExecSQL('INSERT INTO TESTVERSION (TV_ID,TV_VERSION) VALUES (2,''y'')');
  AssertEquals('Count',2,TDBHelper.CountRecords('TESTVERSION'));
  AssertEquals('Get x',1,SQL.GetVersionID('x'));
  AssertEquals('Get y',2,SQL.GetVersionID('y'));
  AssertEquals('Get z',-1,SQL.GetVersionID('z'));
end;

procedure TTestSQLCase.TestGetTestID;
begin
  TDBHelper.ExecSQL('INSERT INTO TESTS (T_ID,T_NAME,T_ADDDATE) VALUES (1,''x.pp'',CURRENT_TIMESTAMP)');
  TDBHelper.ExecSQL('INSERT INTO TESTS (T_ID,T_NAME,T_ADDDATE) VALUES (2,''y.pp'',CURRENT_TIMESTAMP)');
  AssertEquals('Count',2,TDBHelper.CountRecords('TESTS'));
  AssertEquals('Get x',1,SQL.GetTestID('x.pp'));
  AssertEquals('Get y',2,SQL.GetTestID('y.pp'));
  AssertEquals('Get z',-1,SQL.GetCategoryID('z.pp'));
end;

procedure TTestSQLCase.TestGetRunID;

var
  lData : TTestRunData;
  lPlatformID : integer;
  lRunID : Int64;
begin
  lData:=Default(TTestRunData);
  lPlatformID:=PreparePlatform(lData);
  lData.PlatformID:=lPlatFormID;
  lData.Date:=Date;
  lRunID:=SQL.AddRun(lData);
  AssertEquals('Get run id',lRunID,SQL.GetRunID(lData));
end;

function TTestSQLCase.GetSQL: TTestSQL;
begin
  Result:=TDBHelper.SQL;
end;

procedure TTestSQLCase.SetUp;
begin
  TDBHelper.ClearAllTables;
  SQL.TestSrcDir:='./';
end;

procedure TTestSQLCase.TearDown;
begin
  TDBHelper.MaybeRollback;
  DeleteSource('x');
end;

procedure TTestSQLCase.TestHistoryNoHistory;

Var
  lData : TTestRunData;
  lResultID : Int64;

begin
  AssertEquals('count TESTRUNHISTORY before',0,TDBHelper.CountRecords('TESTRUNHISTORY'));
  lData:=Default(TTestRunData);
  lData.PlatformID:=PreparePlatform(lData);
  lData.Date:=Date;
  lResultID:=SQL.AddRun(lData);
  AssertEquals('count TESTRUN', 1, TDBHelper.CountRecords('TESTRUN',Format('(TU_ID=%d)',[lResultID])));
  AssertEquals('count TESTRUNHISTORY after',0,TDBHelper.CountRecords('TESTRUNHISTORY'));
end;

procedure TTestSQLCase.TestHistoryWithHistory;

Var
  lData : TTestRunData;
  lResult1ID,lResult2ID : Int64;
  lFilter : String;

begin
  AssertEquals('count TESTRUNHISTORY before',0,TDBHelper.CountRecords('TESTRUNHISTORY'));
  lData:=Default(TTestRunData);
  lData.PlatformID:=PreparePlatform(lData);
  lData.Date:=Date-1;
  lResult1ID:=SQL.AddRun(lData);
  AssertEquals('count TESTRUN', 1, TDBHelper.CountRecords('TESTRUN',Format('(TU_ID=%d)',[lResult1ID])));
  AssertEquals('count TESTRUNHISTORY after',0,TDBHelper.CountRecords('TESTRUNHISTORY'));
  lData.Date:=Date;
  lResult2ID:=SQL.AddRun(lData);
  AssertEquals('count TESTRUN', 1, TDBHelper.CountRecords('TESTRUN',Format('(TU_ID=%d)',[lResult2ID])));
  lFilter:=Format('(TH_ID_FK=%d) and (TH_PREVIOUS_FK=%d)',[lResult2ID,lResult1ID]);
  AssertEquals('count TESTRUNHISTORY after',1,TDBHelper.CountRecords('TESTRUNHISTORY',lFilter));
end;

procedure TTestSQLCase.TestGetPreviousTestRun;
begin
  TDBHelper.ExecSQL('INSERT INTO TESTRUNHISTORY VALUES (2,1)');
  TDBHelper.ExecSQL('INSERT INTO TESTRUNHISTORY VALUES (3,2)');
  TDBHelper.ExecSQL('INSERT INTO TESTRUNHISTORY VALUES (4,3)');
  AssertEquals('First',-1,SQL.GetPreviousRunID(1));
  AssertEquals('Second',1,SQL.GetPreviousRunID(2));
  AssertEquals('third',2,SQL.GetPreviousRunID(3));
  AssertEquals('last',3,SQL.GetPreviousRunID(4));
end;

procedure TTestSQLCase.TestGetNextTestRun;
begin
  TDBHelper.ExecSQL('INSERT INTO TESTRUNHISTORY VALUES (2,1)');
  TDBHelper.ExecSQL('INSERT INTO TESTRUNHISTORY VALUES (3,2)');
  TDBHelper.ExecSQL('INSERT INTO TESTRUNHISTORY VALUES (4,3)');
  AssertEquals('First',2,SQL.GetNextRunID(1));
  AssertEquals('Second',3,SQL.GetNextRunID(2));
  AssertEquals('third',4,SQL.GetNextRunID(3));
  AssertEquals('last',-1,SQL.GetNextRunID(4));
end;

procedure TTestSQLCase.TestAddCheckAllRtl;

var
  lData : TCheckAllRTL;
  lTestRunData: TTestRunData;
  I : integer;

begin
  lTestRunData:=Default(TTestRunData);
  lData:=Default(TCheckAllRTL);
  lData.Platform:=PreparePlatform(lTestRunData);
  lData.Date:=Date;
  for I:=Low(TCheckStage) to High(TCheckStage) do
    begin
    lData.Steps[i]:=(I mod 2)=0;
    lData.Logs[i]:='Step '+IntToStr(i)+' log';
    end;
  lData.ID:=SQL.AddCheckAllRtl(lData);
  AssertEquals('count CheckAllRTL', 1, TDBHelper.CountRecords('CHECKALLRTL',Format('(CA_ID=%d)',[lData.ID])));
  AssertEquals('count CheckAllRTLLog', 3, TDBHelper.CountRecords('CHECKALLRTLLOG',Format('CAL_CHECKALLRTL_FK=%d',[lData.ID])));

end;


initialization
  RegisterTestDecorator(TDBDecorator,TTestSQLCase);

end.

