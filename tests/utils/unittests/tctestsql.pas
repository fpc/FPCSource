unit tctestsql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testu, dbtests, tresults, sqldb, pqconnection;

const
  Bools : Array[Boolean] of string = ('f','t');

type


  { TTestSQLCase }

  TTestSQLCase= class(TTestCase)
  const
    SQLTestResultFilter = '(TR_ID=%d) and (TR_TESTRUN_FK=%d) and (TR_TEST_FK=%d) and (TR_OK=''%s'') and (TR_SKIP=''%s'') and (TR_RESULT=%d) and (TR_LOG=''%s'')';
  private
    procedure AssertTestRunData(aQry: TSQLQuery; aData: TTestRunData);
    function CreateResultData(out aData: TTestRunData; out aResult: TTestResultData; DateOffset: Integer = 0): Int64;
    procedure DeleteSource(const aFileName: String);
    function GetSQL: TTestSQL;
    function PreparePlatform(var aData: TTestRunData): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CreateSource(const aFileName : String);
    property SQL : TTestSQL Read GetSQL;
  published
    procedure TestHookUp;
    procedure TestAddCPU;
    procedure TestAddOS;
    procedure TestAddVersion;
    procedure TestAddCategory;
    procedure TestAddTest;
    procedure TestAddPlatform;
    Procedure TestAddRun;
    procedure TestUpdateRun;
    Procedure TestAddTestResult;
    Procedure TestAddTestResultTwice;
    Procedure TestUpdateTestResult;
    Procedure TestAddLastResult;
    Procedure TestAddLastResultTwice;
    Procedure TestGetLastTestResult;
    procedure TestGetCPUID;
    procedure TestGetOSID;
    procedure TestGetCategoryID;
    procedure TestGetVersionID;
    procedure TestGetTestID;
    procedure TestGetRunID;
  end;



implementation

uses tcsetup;

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

function TTestSQLCase.PreparePlatform(var aData : TTestRunData) : Integer;

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
  Result:=SQL.GetPlatformID(aData,False);
  if Result=-1 then
    Result:=SQL.AddPlatform(aData);
end;

procedure TTestSQLCase.TestAddPlatform;

const
  SQLFilter = '(TP_ID=%d) and (TP_OS_FK=%d) and (TP_CPU_FK=%d) '+
              'and (TP_VERSION_FK=%d) and (TP_CONFIG=''%s'')';
var
  lData : TTestRunData;
  lID : integer;
  Flt : String;
begin
  lData:=Default(TTestRunData);
  lID:=PreparePlatform(lData);
  With lData do
    flt:=Format(SQLFilter,[lID,OSID,CPUID,VersionID,Config]);
  AssertEquals('Platform',1,TDBHelper.CountRecords('TESTPLATFORM',Flt));
end;

procedure TTestSQLCase.AssertTestRunData(aQry : TSQLQuery; aData : TTestRunData);

var
  St : TTestStatus;

begin
  With aQry,aData do
    begin
    AssertEquals('Date',DATE,FieldByName('TU_DATE').AsDateTime);
    AssertEquals('Platform',PlatformID,FieldByName('TU_PLATFORM_FK').AsInteger);
    AssertEquals('Machine',Machine,FieldByName('TU_MACHINE').AsString);
    AssertEquals('Submitter',Submitter,FieldByName('TU_SUBMITTER').AsString);
    For St in TTestStatus do
      AssertEquals(StatusText[St],StatusCount[st],FieldByName(SQLField[ST]).AsInteger);
    AssertEquals('CompilerDate',CompilerDate,FieldByName('TU_COMPILERDATE').AsString);
    AssertEquals('CompilerFullVersion',CompilerFullVersion,FieldByName('TU_COMPILERFULLVERSION').AsString);
    AssertEquals('CompilerRevision',CompilerRevision,FieldByName('TU_COMPILERREVISION').AsString);
    AssertEquals('TestsRevision',TestsRevision,FieldByName('TU_TESTSREVISION').AsString);
    AssertEquals('RTLRevision',RTLRevision,FieldByName('TU_RTLREVISION').AsString);
    AssertEquals('PackagesRevision',PackagesRevision,FieldByName('TU_PACKAGESREVISION').AsString);
    end;
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

function TTestSQLCase.CreateResultData(out aData: TTestRunData; out aResult: TTestResultData; DateOffset: Integer): Int64;

begin
  aData:=Default(TTestRunData);
  aData.PlatformID:=PreparePlatform(aData);
  aData.Date:=Date-DateOffset;
  aData.RunID:=SQL.AddRun(aData);
  aResult:=Default(TTestResultData);
  aResult.RunID:=aData.RunID;
  CreateSource('x');
  if SQL.GetTestID('x.pp')=-1 then
    aResult.TestID:=SQL.AddTest('x.pp',False);
  aResult.TestResult:=stSuccessCompilationFailed;
  aResult.Log:='xyz';
  With aData do
    Result:=SQL.AddTestResult(aResult);
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
  lID:=CreateResultData(lData,lResult);
  AssertTrue('Add',SQL.AddLastResult(lResult.TestID,lData.PlatformID,lID));
  lResult2:=SQL.GetLastTestResult(lResult.TestID,lData.PlatformID);
  AssertEquals('ID',lID,lResult2.ID);
  AssertEquals('Run',lResult.RunID,lResult2.RunID);
  AssertTrue('Status',lResult.TestResult=lResult2.TestResult);
  AssertEquals('Log',lResult.Log,lResult2.Log);
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
  for St in TTestStatus do
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
  TDBHelper.ClearTable('TESTOS');
  TDBHelper.ClearTable('TESTCPU');
  TDBHelper.ClearTable('TESTCATEGORY');
  TDBHelper.ClearTable('TESTVERSION');
  TDBHelper.ClearTable('TESTPLATFORM');
  TDBHelper.ClearTable('TESTRUN');
  TDBHelper.ClearTable('TESTS');
  TDBHelper.ClearTable('TESTRESULTS');
  TDBHelper.ClearTable('TESTLASTRESULTS');
  TDBHelper.ClearTable('TESTPREVIOUSRESULTS');
  SQL.TestSrcDir:='./';
end;

procedure TTestSQLCase.TearDown;
begin
  TDBHelper.MaybeRollback;
  DeleteSource('x');
end;

procedure TTestSQLCase.DeleteSource(const aFileName: String);
begin
  if FileExists(aFilename+'.pp') then
    if not DeleteFile(aFilename+'.pp') then
      Fail('Failed to delete '+aFileName+'.pp');
end;
procedure TTestSQLCase.CreateSource(const aFileName: String);
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



initialization
  RegisterTestDecorator(TDBDecorator,TTestSQLCase);

end.

