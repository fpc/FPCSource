unit tcanalyst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqldb, digestanalyst, pqconnection, tcsetup, tctestsql, tsutils, tsdb, tstypes;

type

  { TTestAnalyst }

  TTestAnalyst = class(TTestBaseSQLCase)
  private
    FAnalyst: TDBDigestAnalyzer;
    FSQL: TTestSQL;
  protected
    function GetSQL: TTestSQL; override;
  public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Analyst : TDBDigestAnalyzer read FAnalyst;

  Published
    Procedure TestHookup;
    procedure TestSaveResultsIdentical;
    procedure TestSaveResultsDifferSameDate;
    procedure TestSaveResultsDifferOlderDate;
    procedure TestSaveResultsDifferNewerDate;
  end;

implementation

{ TTestAnalyst }

function TTestAnalyst.GetSQL: TTestSQL;
begin
  Result:=FSQL;
end;

procedure TTestAnalyst.SetUp;
begin
  inherited SetUp;
  if not Assigned(TDBHelper.SQL) then
    TDBHelper.Setup;
  FSQL:=TDBHelper.SQL;
  FAnalyst:=TDBDigestAnalyzer.Create(FSQL,'');
  TDBHelper.ClearAllTables;
end;

procedure TTestAnalyst.TearDown;
begin
  FreeAndNil(FAnalyst);
  FSQL:=Nil;
  inherited TearDown;
end;

procedure TTestAnalyst.TestHookup;
begin
  AssertNotNull('SQL',SQL);
  AssertNotNull('Analyst',Analyst);
  AssertSame('Analyst SQL',SQL,Analyst.DB);
end;

procedure TTestAnalyst.TestSaveResultsDifferSameDate;
var
  lData : TTestRunData;
  lResults2,lResults : TTestResultData;
  lResultID : Int64;

begin
  lResultID:=CreateResultData(lData,lResults,1);
  SQL.AddLastResult(lResults.TestID,lData.PlatformID,lResultID);
  AssertEquals('count TESTRESULTS before',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  AssertTrue('Have iD',lResultID>0);
  lResults.ID:=0;
  lResults.TestResult:=TTestStatus.stFailedToRun;
  AssertFalse('No new record for identical date',Analyst.SaveTestResult(lResults));
  AssertEquals('count TESTRESULTS after identical date ',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  lResults2:=SQL.GetLastTestResult(lResults.TestID,lData.PlatformID);
  AssertTrue('Existing record was updated',lResults2.TestResult=lResults.TestResult);
end;

procedure TTestAnalyst.TestSaveResultsDifferOlderDate;
var
  lData : TTestRunData;
  lResults2,lResults : TTestResultData;
  lResultID : Int64;

begin
  lResultID:=CreateResultData(lData,lResults,1);
  SQL.AddLastResult(lResults.TestID,lData.PlatformID,lResultID);
  AssertEquals('count TESTRESULTS before',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  AssertTrue('Have iD',lResultID>0);
  // Simulate new run, but on older date
  lData.Date:=Date-2;
  lData.RunID:=SQL.AddRun(lData);
  // test result
  lResults.ID:=0;
  lResults.RunID:=lData.RunID;
  lResults.TestResult:=TTestStatus.stFailedToRun;
  lResults.Date:=Date-2;
  AssertFalse('No new record for identical date',Analyst.SaveTestResult(lResults));
  AssertEquals('count TESTRESULTS after identical date ',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  lResults2:=SQL.GetLastTestResult(lResults.TestID,lData.PlatformID);
  AssertTrue('Existing record was not updated',lResults2.TestResult<>lResults.TestResult);
end;

procedure TTestAnalyst.TestSaveResultsDifferNewerDate;


var
  lData : TTestRunData;
  lResults2,lResults : TTestResultData;
  lResultID : Int64;

begin
  lResultID:=CreateResultData(lData,lResults,1);
  SQL.AddLastResult(lResults.TestID,lData.PlatformID,lResultID);
  AssertEquals('count TESTRESULTS before',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  AssertTrue('Have iD',lResultID>0);
  // Simulate new run
  lData.Date:=Date;
  lData.RunID:=SQL.AddRun(lData);
  // test result
  lResults.ID:=0;
  lResults.RunID:=lData.RunID;
  lResults.TestResult:=TTestStatus.stFailedToRun;
  lResults.Date:=Date;
  AssertTrue('new record for identical date',Analyst.SaveTestResult(lResults));
  AssertEquals('count TESTRESULTS after ',2,TDBHelper.CountRecords('TESTRESULTS'));
  // these remain the same, the platform/test is the same...
  AssertEquals('count TESTLASTRESULTS after',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS after',1,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  lResults2:=SQL.GetLastTestResult(lResults.TestID,lData.PlatformID);
  AssertEquals('Existing record was updated (id)',lResults.ID,lResults2.ID);
  AssertTrue('New record was marked as last (status)',lResults2.TestResult=lResults.TestResult);
end;


procedure TTestAnalyst.TestSaveResultsIdentical;

var
  lData : TTestRunData;
  lResults : TTestResultData;
  lResultID : Int64;

begin
  lResultID:=CreateResultData(lData,lResults,1);
  SQL.AddLastResult(lResults.TestID,lData.PlatformID,lResultID);
  AssertEquals('count TESTRESULTS before',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
  AssertTrue('Have iD',lResultID>0);
  lResults.ID:=0;
  AssertFalse('No new record for identical',Analyst.SaveTestResult(lResults));
  AssertEquals('count TESTRESULTS after identical',1,TDBHelper.CountRecords('TESTRESULTS'));
  AssertEquals('count TESTLASTRESULTS before',1,TDBHelper.CountRecords('TESTLASTRESULTS'));
  AssertEquals('count TESTPREVIOUSRESULTS before',0,TDBHelper.CountRecords('TESTPREVIOUSRESULTS'));
end;

begin
  Registertest(TTestAnalyst);

end.

